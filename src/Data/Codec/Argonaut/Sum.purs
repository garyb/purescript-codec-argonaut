module Data.Codec.Argonaut.Sum
  ( Encoding(..)
  , class GCases
  , class GFields
  , defaultEncoding
  , enumSum
  , gCasesDecode
  , gCasesEncode
  , gFieldsDecode
  , gFieldsEncode
  , sum
  , sumWith
  , taggedSum
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core (Json, fromString) as J
import Data.Array (catMaybes)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec (codec', encode)
import Data.Codec as Codec
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..), jobject)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..), from, to)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor (dimap)
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
  = EncodeCtorAsTag
      { unwrapSingleArguments ∷ Boolean }
  | EncodeTagValue
      { tagKey ∷ String
      , valuesKey ∷ String
      , omitEmptyArguments ∷ Boolean
      , unwrapSingleArguments ∷ Boolean
      }

defaultEncoding ∷ Encoding
defaultEncoding = EncodeTagValue
  { tagKey: "tag"
  , valuesKey: "values"
  , unwrapSingleArguments: false
  , omitEmptyArguments: false
  }

--------------------------------------------------------------------------------

sum ∷ ∀ r rep a. Generic a rep ⇒ GCases r rep ⇒ String → Record r → JsonCodec a
sum = sumWith defaultEncoding

sumWith ∷ ∀ r rep a. GCases r rep ⇒ Generic a rep ⇒ Encoding → String → Record r → JsonCodec a
sumWith encoding name r =
  dimap from to $ codec' decode encode
  where
  decode = gCasesDecode encoding r >>> (lmap $ Named name)
  encode = gCasesEncode encoding r

--------------------------------------------------------------------------------

class GCases ∷ Row Type → Type → Constraint
class
  GCases r rep
  where
  gCasesEncode ∷ Encoding → Record r → rep → Json
  gCasesDecode ∷ Encoding → Record r → Json → Either JsonDecodeError rep

instance gCasesConstructorNoArgs ∷
  ( Row.Cons name Unit () r
  , IsSymbol name
  ) ⇒
  GCases r (Constructor name NoArguments) where
  gCasesEncode ∷ Encoding → Record r → Constructor name NoArguments → Json
  gCasesEncode encoding _ _ =
    let
      name = reflectSymbol @name Proxy ∷ String
    in
      encodeCase encoding name []

  gCasesDecode ∷ Encoding → Record r → Json → Either JsonDecodeError (Constructor name NoArguments)
  gCasesDecode encoding _ json = do
    let name = reflectSymbol @name Proxy ∷ String

    parseNoFields encoding json name
    pure $ Constructor NoArguments

else instance gCasesConstructorSingleArg ∷
  ( Row.Cons name (JsonCodec a) () r
  , IsSymbol name
  ) ⇒
  GCases r (Constructor name (Argument a)) where
  gCasesEncode ∷ Encoding → Record r → Constructor name (Argument a) → Json
  gCasesEncode encoding r (Constructor (Argument x)) =
    let
      codec = Record.get (Proxy @name) r ∷ JsonCodec a
      name = reflectSymbol @name Proxy ∷ String
    in
      encodeCase encoding name [ CA.encode codec x ]

  gCasesDecode ∷ Encoding → Record r → Json → Either JsonDecodeError (Constructor name (Argument a))
  gCasesDecode encoding r json = do
    let name = reflectSymbol @name Proxy ∷ String

    field ← parseSingleField encoding json name ∷ _ Json
    let codec = Record.get (Proxy @name) r ∷ JsonCodec a
    result ← CA.decode codec field ∷ _ a
    pure $ Constructor (Argument result)

else instance gCasesConstructorManyArgs ∷
  ( Row.Cons name codecs () r
  , GFields codecs args
  , IsSymbol name
  ) ⇒
  GCases r (Constructor name args) where
  gCasesEncode ∷ Encoding → Record r → Constructor name args → Json
  gCasesEncode encoding r (Constructor rep) =
    let
      codecs = Record.get (Proxy @name) r ∷ codecs
      name = reflectSymbol @name Proxy ∷ String
      jsons = gFieldsEncode encoding codecs rep ∷ Array Json
    in
      encodeCase encoding name jsons

  gCasesDecode ∷ Encoding → Record r → Json → Either JsonDecodeError (Constructor name args)
  gCasesDecode encoding r json = do
    let name = reflectSymbol @name Proxy ∷ String

    jsons ← parseManyFields encoding json name ∷ _ (Array Json)
    let codecs = Record.get (Proxy @name) r ∷ codecs
    result ← gFieldsDecode encoding codecs jsons ∷ _ args
    pure $ Constructor result

instance gCasesSum ∷
  ( GCases r1 (Constructor name lhs)
  , GCases r2 rhs
  , Row.Cons name codecs1 () r1
  , Row.Cons name codecs1 r2 r
  , Row.Union r1 r2 r
  , Row.Lacks name r2
  , IsSymbol name
  ) ⇒
  GCases r (Sum (Constructor name lhs) rhs) where
  gCasesEncode ∷ Encoding → Record r → Sum (Constructor name lhs) rhs → Json
  gCasesEncode encoding r =
    let
      codecs1 = Record.get (Proxy @name) r ∷ codecs1
      r1 = Record.insert (Proxy @name) codecs1 {} ∷ Record r1
      r2 = Record.delete (Proxy @name) r ∷ Record r2
    in
      case _ of
        Inl lhs → gCasesEncode encoding r1 lhs
        Inr rhs → gCasesEncode encoding r2 rhs

  gCasesDecode ∷ Encoding → Record r → Json → Either JsonDecodeError (Sum (Constructor name lhs) rhs)
  gCasesDecode encoding r tagged = do
    let
      codecs1 = Record.get (Proxy @name) r ∷ codecs1
      r1 = Record.insert (Proxy @name) codecs1 {} ∷ Record r1
      r2 = Record.delete (Proxy @name) r ∷ Record r2
    let
      lhs = gCasesDecode encoding r1 tagged ∷ _ (Constructor name lhs)
      rhs = gCasesDecode encoding r2 tagged ∷ _ rhs
    (Inl <$> lhs) <|> (Inr <$> rhs)

--------------------------------------------------------------------------------

class GFields ∷ Type → Type → Constraint
class GFields codecs rep where
  gFieldsEncode ∷ Encoding → codecs → rep → Array Json
  gFieldsDecode ∷ Encoding → codecs → Array Json → Either JsonDecodeError rep

instance gFieldsArgument ∷ GFields (JsonCodec a) (Argument a) where
  gFieldsEncode ∷ Encoding → JsonCodec a → Argument a → Array Json
  gFieldsEncode _ codec (Argument val) = [ CA.encode codec val ]

  gFieldsDecode ∷ Encoding → JsonCodec a → Array Json → Either JsonDecodeError (Argument a)
  gFieldsDecode _ codec jsons = do
    json ←
      ( case jsons of
          [ head ] → pure head
          _ → Left $ TypeMismatch "Expecting exactly one element"
      ) ∷ _ Json
    res ← CA.decode codec json ∷ _ a
    pure $ Argument res

instance gFieldsProduct ∷
  ( GFields codec rep
  , GFields codecs reps
  ) ⇒
  GFields (codec /\ codecs) (Product rep reps) where
  gFieldsEncode ∷ Encoding → (codec /\ codecs) → Product rep reps → Array Json
  gFieldsEncode encoding (codec /\ codecs) (Product rep reps) =
    let
      r1 = gFieldsEncode encoding codec rep ∷ Array Json
      r2 = gFieldsEncode encoding codecs reps ∷ Array Json
    in
      r1 <> r2

  gFieldsDecode ∷ Encoding → (codec /\ codecs) → Array Json → Either JsonDecodeError (Product rep reps)
  gFieldsDecode encoding (codec /\ codecs) jsons = do
    { head, tail } ←
      (Array.uncons jsons # note (TypeMismatch "Expecting at least one element"))
        ∷ _ { head ∷ Json, tail ∷ Array Json }
    rep ← gFieldsDecode encoding codec [ head ] ∷ _ rep
    reps ← gFieldsDecode encoding codecs tail ∷ _ reps
    pure $ Product rep reps

--------------------------------------------------------------------------------

checkTag ∷ String → Object Json → String → Either JsonDecodeError Unit
checkTag tagKey obj expectedTag = do
  val ←
    ( Obj.lookup tagKey obj
        # note (TypeMismatch ("Expecting a tag property `" <> tagKey <> "`"))
    ) ∷ _ Json
  tag ← CA.decode CA.string val ∷ _ String
  unless (tag == expectedTag)
    $ throwError
    $ TypeMismatch ("Expecting tag `" <> expectedTag <> "`, got `" <> tag <> "`")

parseSingleField ∷ Encoding → Json → String → Either JsonDecodeError Json
parseSingleField encoding json expectedTag = case encoding of
  EncodeCtorAsTag { unwrapSingleArguments } → unsafeCoerce "todo"
  EncodeTagValue { tagKey, valuesKey, unwrapSingleArguments } → do
    obj ← CA.decode jobject json
    checkTag tagKey obj expectedTag
    val ←
      ( Obj.lookup valuesKey obj
          # note (TypeMismatch ("Expecting a value property `" <> valuesKey <> "`"))
      ) ∷ _ Json
    if unwrapSingleArguments then
      pure val
    else do
      fields ← CA.decode CA.jarray val
      case fields of
        [ head ] → pure head
        _ → throwError $ TypeMismatch "Expecting exactly one element"

parseNoFields ∷ Encoding → Json → String → Either JsonDecodeError Unit
parseNoFields encoding json expectedTag =
  case encoding of
    EncodeCtorAsTag { unwrapSingleArguments } → unsafeCoerce "todo"
    EncodeTagValue { tagKey, valuesKey, omitEmptyArguments } →
      do
        obj ← CA.decode jobject json
        checkTag tagKey obj expectedTag
        when (not omitEmptyArguments) do
          val ←
            ( Obj.lookup valuesKey obj
                # note (TypeMismatch ("Expecting a value property `" <> valuesKey <> "`"))
            ) ∷ _ Json
          fields ← CA.decode CA.jarray val ∷ _ (Array Json)
          when (fields /= [])
            $ throwError
            $ TypeMismatch "Expecting an empty array"

parseManyFields ∷ Encoding → Json → String → Either JsonDecodeError (Array Json)
parseManyFields encoding json expectedTag =
  case encoding of
    EncodeCtorAsTag { unwrapSingleArguments } → unsafeCoerce "todo"
    EncodeTagValue { tagKey, valuesKey } → do
      obj ← CA.decode jobject json
      checkTag tagKey obj expectedTag
      val ←
        ( Obj.lookup valuesKey obj
            # note (TypeMismatch ("Expecting a value property `" <> valuesKey <> "`"))
        ) ∷ _ Json
      CA.decode CA.jarray val

encodeCase ∷ Encoding → String → Array Json → Json
encodeCase encoding tag jsons =
  case encoding of
    EncodeCtorAsTag { unwrapSingleArguments } → unsafeCoerce "todo"
    EncodeTagValue { tagKey, valuesKey, unwrapSingleArguments, omitEmptyArguments } →
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