module Data.Codec.Argonaut.Sum
  ( Encoding(..)
  , FlatEncoding
  , class GCases
  , class GFields
  , class GFlatCases
  , defaultEncoding
  , defaultFlatEncoding
  , enumSum
  , gCasesDecode
  , gCasesEncode
  , gFieldsDecode
  , gFieldsEncode
  , gFlatCasesDecode
  , gFlatCasesEncode
  , sum
  , sumFlat
  , sumFlatWith
  , sumWith
  , taggedSum
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core (Json, fromString) as J
import Data.Array (catMaybes)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec (codec', encode)
import Data.Codec as Codec
import Data.Codec.Argonaut (JPropCodec, JsonCodec, JsonDecodeError(..), jobject)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..), either, note)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..), from, to)
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.Profunctor (dimap)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object (Object)
import Foreign.Object as FO
import Foreign.Object as Obj
import Foreign.Object.ST as FOST
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | A helper for defining JSON codecs for "enum" sum types, where every
-- | constructor is nullary, and the type will be encoded as a string.
enumSum
  ∷ ∀ a
  . (a → String)
  → (String → Maybe a)
  → CA.JsonCodec a
enumSum printTag parseTag =
  Codec.codec
    (\j → maybe (Left (CA.UnexpectedValue j)) Right <<< parseTag =<< Codec.decode CA.string j)
    (Codec.encode CA.string <<< printTag)

-- | A helper for defining JSON codecs for sum types. To ensure exhaustivity
-- | there needs to be a mapping to and from a tag type for the type to be
-- | encoded.
-- |
-- | - The first argument is the name of the type being decoded, for error
-- |   message purposes.
-- | - The second argument maps a tag value to a string to use in the encoding.
-- | - The third argument maps a string back to a tag value during decoding.
-- | - The fourth argument returns either a constant value or a decoder function
-- |   based on a tag value.
-- | - The fifth argument returns a tag value and optional encoded value to
-- |   store for a constructor of the sum.
taggedSum
  ∷ ∀ tag a
  . String
  → (tag → String)
  → (String → Maybe tag)
  → (tag → Either a (J.Json → Either CA.JsonDecodeError a))
  → (a → Tuple tag (Maybe J.Json))
  → CA.JsonCodec a
taggedSum name printTag parseTag f g = Codec.codec decodeCase encodeCase
  where
  decodeCase ∷ J.Json → Either CA.JsonDecodeError a
  decodeCase j = lmap (CA.Named name) do
    obj ← Codec.decode CA.jobject j
    tag ← Codec.decode (CA.prop "tag" CA.string) obj
    case parseTag tag of
      Nothing → Left (CA.AtKey "tag" (CA.UnexpectedValue (J.fromString tag)))
      Just t →
        case f t of
          Left a → pure a
          Right decoder → do
            value ← Codec.decode (CA.prop "value" CA.json) obj
            lmap (CA.AtKey "value") (decoder value)

  encodeCase ∷ a → J.Json
  encodeCase a = case g a of
    Tuple tag value →
      Codec.encode CA.jobject $
        FO.runST do
          obj ← FOST.new
          _ ← FOST.poke "tag" (Codec.encode CA.string (printTag tag)) obj
          maybe (pure obj) (\v → FOST.poke "value" v obj) value

--------------------------------------------------------------------------------

data Encoding
  = EncodeNested
      { unwrapSingleArguments ∷ Boolean }
  | EncodeTagged
      { tagKey ∷ String
      , valuesKey ∷ String
      , omitEmptyArguments ∷ Boolean
      , unwrapSingleArguments ∷ Boolean
      }

defaultEncoding ∷ Encoding
defaultEncoding = EncodeTagged
  { tagKey: "tag"
  , valuesKey: "values"
  , unwrapSingleArguments: false
  , omitEmptyArguments: false
  }

--------------------------------------------------------------------------------

mapError ∷ ∀ r. Maybe String → Object Json → Either JsonDecodeError (Maybe r) → Either JsonDecodeError r
mapError mbTag obj =
  either Left (maybe' noMatchErr Right)
  where
  getTagErr tag =
    AtKey tag $ maybe MissingValue UnexpectedValue (Obj.lookup tag obj)

  noMatchErr _ =
    Left $ maybe
      ( TypeMismatch
          $ "No match for sum cases in nested keys: "
              <> joinWith ", " ((\k → "`" <> k <> "`") <$> Obj.keys obj)
      )
      getTagErr mbTag

sum ∷ ∀ r rep a. Generic a rep ⇒ GCases r rep ⇒ String → Record r → JsonCodec a
sum = sumWith defaultEncoding

sumWith ∷ ∀ r rep a. GCases r rep ⇒ Generic a rep ⇒ Encoding → String → Record r → JsonCodec a
sumWith encoding name r =
  dimap from to $ codec' decode encode
  where
  mbTag = case encoding of
    EncodeTagged { tagKey } → Just tagKey
    EncodeNested _ → Nothing

  decodeObj obj = gCasesDecode encoding r obj # mapError mbTag obj
  decode = CA.decode jobject >>> either Left decodeObj >>> (lmap $ Named name)
  encode = gCasesEncode encoding r

--------------------------------------------------------------------------------

type GCasesEncode r rep = Encoding → Record r → rep → Json
type GCasesDecode r rep = Encoding → Record r → Object Json → Either JsonDecodeError (Maybe rep)

class GCases ∷ Row Type → Type → Constraint
class
  GCases r rep
  where
  gCasesEncode ∷ GCasesEncode r rep
  gCasesDecode ∷ GCasesDecode r rep

instance gCasesConstructorNoArgs ∷
  ( Row.Cons name Unit () r
  , IsSymbol name
  ) ⇒
  GCases r (Constructor name NoArguments) where
  gCasesEncode ∷ GCasesEncode r (Constructor name NoArguments)
  gCasesEncode encoding _ _ =
    let
      name = reflectSymbol @name Proxy ∷ String
    in
      encodeSumCase encoding name []

  gCasesDecode ∷ GCasesDecode r (Constructor name NoArguments)
  gCasesDecode encoding _ json = do
    let name = reflectSymbol @name Proxy ∷ String
    lmap (Named ("case " <> name)) do
      parsed ← parseNoFields encoding json name
      pure $ parsed <#> const (Constructor NoArguments)

else instance gCasesConstructorSingleArg ∷
  ( Row.Cons name (JsonCodec a) () r
  , IsSymbol name
  ) ⇒
  GCases r (Constructor name (Argument a)) where
  gCasesEncode ∷ GCasesEncode r (Constructor name (Argument a))
  gCasesEncode encoding r (Constructor (Argument x)) =
    let
      codec = Record.get (Proxy @name) r ∷ JsonCodec a
      name = reflectSymbol @name Proxy ∷ String
    in
      encodeSumCase encoding name [ CA.encode codec x ]

  gCasesDecode ∷ GCasesDecode r (Constructor name (Argument a))
  gCasesDecode encoding r obj = do
    let name = reflectSymbol @name Proxy ∷ String
    lmap (Named ("case " <> name)) do
      parsed ← parseSingleField encoding obj name
      flip (maybe (pure Nothing)) parsed
        \field → do
          let codec = Record.get (Proxy @name) r ∷ JsonCodec a
          result ← CA.decode codec field ∷ _ a
          pure $ Just $ Constructor (Argument result)

else instance gCasesConstructorManyArgs ∷
  ( Row.Cons name codecs () r
  , GFields codecs args
  , IsSymbol name
  ) ⇒
  GCases r (Constructor name args) where
  gCasesEncode ∷ GCasesEncode r (Constructor name args)
  gCasesEncode encoding r (Constructor rep) =
    let
      codecs = Record.get (Proxy @name) r ∷ codecs
      name = reflectSymbol @name Proxy ∷ String
      jsons = gFieldsEncode encoding codecs rep ∷ Array Json
    in
      encodeSumCase encoding name jsons

  gCasesDecode ∷ GCasesDecode r (Constructor name args)
  gCasesDecode encoding r obj = do
    let name = reflectSymbol @name Proxy ∷ String
    lmap (Named ("case " <> name)) do
      parsed ← parseManyFields encoding obj name
      flip (maybe (pure Nothing)) parsed
        \jsons → do
          let codecs = Record.get (Proxy @name) r ∷ codecs
          result ← gFieldsDecode encoding codecs jsons 0 ∷ _ args
          pure $ Just $ Constructor result

instance gCasesSum ∷
  ( GCases r1 (Constructor name lhs)
  , GCases r2 rhs
  , Row.Cons name codec () r1
  , Row.Cons name codec r2 r
  , Row.Union r1 r2 r
  , Row.Lacks name r2
  , IsSymbol name
  ) ⇒
  GCases r (Sum (Constructor name lhs) rhs) where
  gCasesEncode ∷ GCasesEncode r (Sum (Constructor name lhs) rhs)
  gCasesEncode encoding r =
    let
      codec = Record.get (Proxy @name) r ∷ codec
      r1 = Record.insert (Proxy @name) codec {} ∷ Record r1
      r2 = unsafeDelete (Proxy @name) r ∷ Record r2
    in
      case _ of
        Inl lhs → gCasesEncode encoding r1 lhs
        Inr rhs → gCasesEncode encoding r2 rhs

  gCasesDecode ∷ GCasesDecode r (Sum (Constructor name lhs) rhs)
  gCasesDecode encoding r tagged = do
    let
      codec = Record.get (Proxy @name) r ∷ codec
      r1 = Record.insert (Proxy @name) codec {} ∷ Record r1
      lhs = gCasesDecode encoding r1 tagged ∷ _ (Maybe (Constructor name lhs))

    lhs >>= case _ of
      Just result →
        pure (Just $ Inl result)
      Nothing → do
        let r2 = Record.delete (Proxy @name) r ∷ Record r2
        let rhs = gCasesDecode encoding r2 tagged ∷ _ (Maybe rhs)
        map Inr <$> rhs

--------------------------------------------------------------------------------

type GFieldsEncode codecs rep = Encoding → codecs → rep → Array Json
type GFieldsDecode codecs rep = Encoding → codecs → Array Json → Int → Either JsonDecodeError rep

class GFields ∷ Type → Type → Constraint
class GFields codecs rep where
  gFieldsEncode ∷ GFieldsEncode codecs rep
  gFieldsDecode ∷ GFieldsDecode codecs rep

instance gFieldsArgument ∷ GFields (JsonCodec a) (Argument a) where
  gFieldsEncode ∷ GFieldsEncode (JsonCodec a) (Argument a)
  gFieldsEncode _ codec (Argument val) = [ CA.encode codec val ]

  gFieldsDecode ∷ GFieldsDecode (JsonCodec a) (Argument a)
  gFieldsDecode _ codec jsons idx = do
    json ← expectOneElement jsons
    res ← lmap (AtIndex idx) $ CA.decode codec json ∷ _ a
    pure $ Argument res

instance gFieldsProduct ∷
  ( GFields codec rep
  , GFields codecs reps
  ) ⇒
  GFields (codec /\ codecs) (Product rep reps) where
  gFieldsEncode ∷ GFieldsEncode (codec /\ codecs) (Product rep reps)
  gFieldsEncode encoding (codec /\ codecs) (Product rep reps) =
    let
      r1 = gFieldsEncode encoding codec rep ∷ Array Json
      r2 = gFieldsEncode encoding codecs reps ∷ Array Json
    in
      r1 <> r2

  gFieldsDecode ∷ GFieldsDecode (codec /\ codecs) (Product rep reps)
  gFieldsDecode encoding (codec /\ codecs) jsons idx = do
    { head, tail } ←
      (Array.uncons jsons # note (TypeMismatch "Expecting at least one element"))
        ∷ _ { head ∷ Json, tail ∷ Array Json }
    rep ← gFieldsDecode encoding codec [ head ] idx ∷ _ rep
    reps ← gFieldsDecode encoding codecs tail (idx + 1) ∷ _ reps
    pure $ Product rep reps

--------------------------------------------------------------------------------

type ParseFields a = Encoding → Object Json → String → Either JsonDecodeError (Maybe a)

ifTagOk ∷ ∀ a. String → Object Json → String → Either JsonDecodeError a → Either JsonDecodeError (Maybe a)
ifTagOk tagKey obj expectedTag act =
  case Obj.lookup tagKey obj <#> CA.decode CA.string of
    Just (Right tag) | tag == expectedTag → map Just act
    _ → pure Nothing

ifNestedOk ∷ ∀ a. Object Json → String → (Json → Either JsonDecodeError a) → Either JsonDecodeError (Maybe a)
ifNestedOk obj expectedTag act =
  maybe (pure Nothing) (map Just <<< act) (Obj.lookup expectedTag obj)

getValue ∷ String → Object Json → Either JsonDecodeError Json
getValue valuesKey obj =
  Obj.lookup valuesKey obj
    # note (TypeMismatch ("Expecting a value property `" <> valuesKey <> "`"))

expectOneElement ∷ ∀ a. Array a → Either JsonDecodeError a
expectOneElement =
  case _ of
    [ head ] → pure head
    _ → Left $ TypeMismatch "Expecting exactly one element"

parseNoFields ∷ ParseFields Unit
parseNoFields encoding obj expectedTag =
  case encoding of
    EncodeNested {} → do
      ifNestedOk obj expectedTag expectEmpty

    EncodeTagged { tagKey, valuesKey, omitEmptyArguments } → do
      ifTagOk tagKey obj expectedTag do
        when (not omitEmptyArguments) do
          getValue valuesKey obj >>= expectEmpty
  where
  expectEmpty val = do
    fields ← CA.decode CA.jarray val ∷ _ (Array Json)
    when (fields /= []) do
      Left $ TypeMismatch "Expecting an empty array"

parseSingleField ∷ ParseFields Json
parseSingleField encoding obj expectedTag = case encoding of
  EncodeNested { unwrapSingleArguments } → do
    ifNestedOk obj expectedTag (handleVal unwrapSingleArguments)

  EncodeTagged { tagKey, valuesKey, unwrapSingleArguments } → do
    ifTagOk tagKey obj expectedTag do
      getValue valuesKey obj >>= handleVal unwrapSingleArguments

  where
  handleVal unwrapSingleArguments val =
    if unwrapSingleArguments then
      pure val
    else
      CA.decode CA.jarray val >>= expectOneElement

parseManyFields ∷ ParseFields (Array Json)
parseManyFields encoding obj expectedTag =
  case encoding of
    EncodeNested {} → do
      ifNestedOk obj expectedTag (CA.decode CA.jarray)

    EncodeTagged { tagKey, valuesKey } → do
      ifTagOk tagKey obj expectedTag do
        getValue valuesKey obj >>= CA.decode CA.jarray

encodeSumCase ∷ Encoding → String → Array Json → Json
encodeSumCase encoding tag jsons =
  case encoding of
    EncodeNested { unwrapSingleArguments } →
      let
        val = case jsons of
          [] → CA.encode CA.jarray []
          [ json ] | unwrapSingleArguments → json
          manyJsons → CA.encode CA.jarray manyJsons
      in
        encode jobject $ Obj.fromFoldable
          [ tag /\ val
          ]

    EncodeTagged { tagKey, valuesKey, unwrapSingleArguments, omitEmptyArguments } →
      let
        tagEntry =
          Just (tagKey /\ CA.encode CA.string tag) ∷ Maybe (String /\ Json)
        valEntry =
          case jsons of
            [] | omitEmptyArguments → Nothing
            [ json ] | unwrapSingleArguments → Just (valuesKey /\ json)
            manyJsons → Just (valuesKey /\ CA.encode CA.jarray manyJsons)
      in
        encode jobject $ Obj.fromFoldable $ catMaybes
          [ tagEntry, valEntry ]

type FlatEncoding (tag ∷ Symbol) =
  { tag ∷ Proxy tag
  }

defaultFlatEncoding ∷ FlatEncoding "tag"
defaultFlatEncoding = { tag: Proxy }

sumFlat ∷ ∀ r rep a. GFlatCases "tag" r rep ⇒ Generic a rep ⇒ String → Record r → JsonCodec a
sumFlat = sumFlatWith defaultFlatEncoding

sumFlatWith ∷ ∀ @tag r rep a. IsSymbol tag ⇒ GFlatCases tag r rep ⇒ Generic a rep ⇒ FlatEncoding tag → String → Record r → JsonCodec a
sumFlatWith _ name r =
  dimap from to $ codec' decode encode
  where
  tag = reflectSymbol (Proxy @tag) ∷ String
  decodeObj obj = gFlatCasesDecode @tag r obj # mapError (Just tag) obj
  decode = CA.decode jobject >>> either Left decodeObj >>> (lmap $ Named name)
  encode = gFlatCasesEncode @tag r

type GFlatCasesEncode r rep = Record r → rep → Json
type GFlatCasesDecode r rep = Record r → Object Json → Either JsonDecodeError (Maybe rep)

class GFlatCases ∷ Symbol → Row Type → Type → Constraint
class
  GFlatCases tag r rep
  where
  gFlatCasesEncode ∷ GFlatCasesEncode r rep
  gFlatCasesDecode ∷ GFlatCasesDecode r rep

instance gFlatCasesConstructorNoArg ∷
  ( Row.Cons name Unit () rc
  , Row.Cons tag String () rf
  , IsSymbol name
  , IsSymbol tag
  ) ⇒
  GFlatCases tag rc (Constructor name NoArguments) where
  gFlatCasesEncode ∷ GFlatCasesEncode rc (Constructor name NoArguments)
  gFlatCasesEncode _ (Constructor NoArguments) =
    let
      name = reflectSymbol (Proxy @name) ∷ String
      propCodec = CAR.record {} ∷ JPropCodec {}
      propCodecWithTag = CA.recordProp (Proxy @tag) CA.string propCodec ∷ JPropCodec (Record rf)
      codecWithTag = CA.object ("case " <> name) propCodecWithTag ∷ JsonCodec (Record rf)
      rcWithTag = Record.insert (Proxy @tag) name {} ∷ Record rf
    in
      CA.encode codecWithTag rcWithTag

  gFlatCasesDecode ∷ GFlatCasesDecode rc (Constructor name NoArguments)
  gFlatCasesDecode _ obj = do
    let name = reflectSymbol (Proxy @name)
    let tagKey = reflectSymbol (Proxy @tag)

    lmap (Named ("case " <> name)) $ ifTagOk tagKey obj name do
      pure $ Constructor NoArguments

instance gFlatCasesConstructorSingleArg ∷
  ( Row.Cons name (JPropCodec (Record rf)) () rc
  , Row.Lacks tag rf
  , Row.Cons tag String rf rf'
  , IsSymbol name
  , IsSymbol tag
  ) ⇒
  GFlatCases tag rc (Constructor name (Argument (Record rf))) where
  gFlatCasesEncode ∷ GFlatCasesEncode rc (Constructor name (Argument (Record rf)))
  gFlatCasesEncode rc (Constructor (Argument rf)) =
    let
      name = reflectSymbol (Proxy @name) ∷ String
      propCodec = Record.get (Proxy @name) rc ∷ JPropCodec (Record rf)
      propCodecWithTag = CA.recordProp (Proxy @tag) CA.string propCodec ∷ JPropCodec (Record rf')
      codecWithTag = CA.object ("case " <> name) propCodecWithTag ∷ JsonCodec (Record rf')
      rcWithTag = Record.insert (Proxy @tag) name rf ∷ Record rf'
    in
      CA.encode codecWithTag rcWithTag

  gFlatCasesDecode ∷ GFlatCasesDecode rc (Constructor name (Argument (Record rf)))
  gFlatCasesDecode rc obj = do
    let name = reflectSymbol (Proxy @name)
    let tagKey = reflectSymbol (Proxy @tag)

    lmap (Named ("case " <> name)) $ ifTagOk tagKey obj name do
      let propCodec = Record.get (Proxy @name) rc ∷ JPropCodec (Record rf)
      r ← CA.decode propCodec obj
      pure $ (Constructor (Argument r))

instance gFlatCasesSum ∷
  ( GFlatCases tag r1 (Constructor name lhs)
  , GFlatCases tag r2 rhs
  , Row.Cons name codec () r1
  , Row.Cons name codec r2 r
  , Row.Union r1 r2 r
  , Row.Lacks name r2
  , IsSymbol name
  ) ⇒
  GFlatCases tag r (Sum (Constructor name lhs) rhs) where
  gFlatCasesEncode ∷ GFlatCasesEncode r (Sum (Constructor name lhs) rhs)
  gFlatCasesEncode r =
    let
      codec = Record.get (Proxy @name) r ∷ codec
      r1 = Record.insert (Proxy @name) codec {} ∷ Record r1
      r2 = unsafeDelete (Proxy @name) r ∷ Record r2
    in
      case _ of
        Inl lhs → gFlatCasesEncode @tag r1 lhs
        Inr rhs → gFlatCasesEncode @tag r2 rhs

  gFlatCasesDecode ∷ GFlatCasesDecode r (Sum (Constructor name lhs) rhs)
  gFlatCasesDecode r tagged = do
    let
      codec = Record.get (Proxy @name) r ∷ codec
      r1 = Record.insert (Proxy @name) codec {} ∷ Record r1
      lhs = gFlatCasesDecode @tag r1 tagged ∷ _ (Maybe (Constructor name lhs))

    lhs >>= case _ of
      Just result →
        pure (Just $ Inl result)
      Nothing → do
        let r2 = Record.delete (Proxy @name) r ∷ Record r2
        let rhs = gFlatCasesDecode @tag r2 tagged ∷ _ (Maybe rhs)
        map Inr <$> rhs

-- | Same as `Record.delete` but deleting only happens at the type level
-- | and the value is left untouched.
unsafeDelete ∷ ∀ r1 r2 l a. IsSymbol l ⇒ Row.Lacks l r1 ⇒ Row.Cons l a r1 r2 ⇒ Proxy l → Record r2 → Record r1
unsafeDelete _ r = unsafeCoerce r
