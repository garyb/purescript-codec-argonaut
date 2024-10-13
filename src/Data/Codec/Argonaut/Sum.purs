module Data.Codec.Argonaut.Sum
  ( Encoding
  , class GProduct
  , class GSum
  , defaultEncoding
  , enumSum
  , gSumDecode
  , gSumEncode
  , sum
  , sumWith
  , taggedSum
  , gProductDecode
  , gProductEncode
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

type Encoding =
  { tagKey ∷ String
  , valuesKey ∷ String
  , unwrapSingleArguments ∷ Boolean
  , omitEmptyArguments ∷ Boolean
  }

defaultEncoding ∷ Encoding
defaultEncoding =
  { tagKey: "tag"
  , valuesKey: "values"
  , unwrapSingleArguments: false
  , omitEmptyArguments: false
  }

--------------------------------------------------------------------------------

sum ∷ ∀ r rep a. Generic a rep ⇒ GSum r rep ⇒ String → Record r → JsonCodec a
sum = sumWith defaultEncoding

sumWith ∷ ∀ r rep a. GSum r rep ⇒ Generic a rep ⇒ Encoding → String → Record r → JsonCodec a
sumWith encoding name r =
  dimap from to $ codec' decode encode
  where
  decode = gSumDecode encoding r >>> (lmap $ Named name)
  encode = gSumEncode encoding r

--------------------------------------------------------------------------------

class GSum ∷ Row Type → Type → Constraint
class
  GSum r rep
  where
  gSumEncode ∷ Encoding → Record r → rep → Json
  gSumDecode ∷ Encoding → Record r → Json → Either JsonDecodeError rep

instance gSumConstructorNoArgs ∷
  ( Row.Cons name Unit () r
  , IsSymbol name
  ) ⇒
  GSum r (Constructor name NoArguments) where
  gSumEncode ∷ Encoding → Record r → Constructor name NoArguments → Json
  gSumEncode encoding _ _ =
    encodeTagged encoding (reflectSymbol (Proxy ∷ Proxy name))
      ( if encoding.omitEmptyArguments then
          Nothing
        else
          Just $ CA.encode CA.jarray []
      )

  gSumDecode ∷ Encoding → Record r → Json → Either JsonDecodeError (Constructor name NoArguments)
  gSumDecode encoding _ json = do
    obj ← CA.decode jobject json ∷ _ (Object Json)

    checkTag encoding obj (reflectSymbol (Proxy ∷ Proxy name))

    parseNoFields encoding obj

    pure $ Constructor NoArguments

else instance gSumConstructorSingleArg ∷
  ( Row.Cons name (JsonCodec a) () r
  , IsSymbol name
  ) ⇒
  GSum r (Constructor name (Argument a)) where
  gSumEncode ∷ Encoding → Record r → Constructor name (Argument a) → Json
  gSumEncode encoding r (Constructor (Argument x)) =
    let
      codec = Record.get (Proxy ∷ Proxy name) r ∷ JsonCodec a
    in
      encodeTagged encoding (reflectSymbol (Proxy ∷ Proxy name))
        ( Just $
            if encoding.unwrapSingleArguments then
              CA.encode codec x
            else
              CA.encode CA.jarray [ CA.encode codec x ]
        )

  gSumDecode ∷ Encoding → Record r → Json → Either JsonDecodeError (Constructor name (Argument a))
  gSumDecode encoding r json = do
    obj ← CA.decode jobject json ∷ _ (Object Json)
    checkTag encoding obj (reflectSymbol (Proxy ∷ Proxy name))

    field ← parseSingleField encoding obj ∷ _ Json

    let codec = Record.get (Proxy ∷ Proxy name) r ∷ JsonCodec a

    result ← CA.decode codec field ∷ _ a

    pure $ Constructor (Argument result)

else instance gSumConstructorManyArgs ∷
  ( Row.Cons name codecs () r
  , GProduct codecs args
  , IsSymbol name
  ) ⇒
  GSum r (Constructor name args) where
  gSumEncode ∷ Encoding → Record r → Constructor name args → Json
  gSumEncode encoding r (Constructor rep) =
    let
      codecs = Record.get (Proxy ∷ Proxy name) r ∷ codecs

      jsons = gProductEncode encoding codecs rep ∷ Array Json
    in
      encodeTagged encoding (reflectSymbol (Proxy ∷ Proxy name))
        (Just $ CA.encode CA.jarray jsons)

  gSumDecode ∷ Encoding → Record r → Json → Either JsonDecodeError (Constructor name args)
  gSumDecode encoding r json = do
    obj ← CA.decode jobject json ∷ _ (Object Json)
    checkTag encoding obj (reflectSymbol (Proxy ∷ Proxy name))

    jsons ← parseManyFields encoding obj ∷ _ (Array Json)

    let codecs = Record.get (Proxy ∷ Proxy name) r ∷ codecs

    result ← gProductDecode encoding codecs jsons ∷ _ args

    pure $ Constructor result

instance gSumSum ∷
  ( GSum r1 (Constructor name lhs)
  , GSum r2 rhs
  , Row.Cons name codecs1 () r1
  , Row.Cons name codecs1 r2 r
  , Row.Union r1 r2 r
  , Row.Lacks name r2
  , IsSymbol name
  ) ⇒
  GSum r (Sum (Constructor name lhs) rhs) where
  gSumEncode ∷ Encoding → Record r → Sum (Constructor name lhs) rhs → Json
  gSumEncode encoding r =
    let
      codecs1 = Record.get (Proxy ∷ Proxy name) r ∷ codecs1
      r1 = Record.insert (Proxy ∷ Proxy name) codecs1 {} ∷ Record r1
      r2 = Record.delete (Proxy ∷ Proxy name) r ∷ Record r2
    in
      case _ of
        Inl lhs → gSumEncode encoding r1 lhs
        Inr rhs → gSumEncode encoding r2 rhs

  gSumDecode ∷ Encoding → Record r → Json → Either JsonDecodeError (Sum (Constructor name lhs) rhs)
  gSumDecode encoding r tagged = do
    let
      codecs1 = Record.get (Proxy ∷ Proxy name) r ∷ codecs1
      r1 = Record.insert (Proxy ∷ Proxy name) codecs1 {} ∷ Record r1
      r2 = Record.delete (Proxy ∷ Proxy name) r ∷ Record r2

      lhs = gSumDecode encoding r1 tagged ∷ _ (Constructor name lhs)
      rhs = gSumDecode encoding r2 tagged ∷ _ rhs
    (Inl <$> lhs) <|> (Inr <$> rhs)

--------------------------------------------------------------------------------

class GProduct ∷ Type → Type → Constraint
class GProduct codecs rep where
  gProductEncode ∷ Encoding → codecs → rep → Array Json
  gProductDecode ∷ Encoding → codecs → Array Json → Either JsonDecodeError rep

instance gProductArgument ∷ GProduct (JsonCodec a) (Argument a) where
  gProductEncode ∷ Encoding → JsonCodec a → Argument a → Array Json
  gProductEncode _ codec (Argument val) = [ CA.encode codec val ]

  gProductDecode ∷ Encoding → JsonCodec a → Array Json → Either JsonDecodeError (Argument a)
  gProductDecode _ codec jsons = do
    json ← case jsons of
      [ head ] → pure head
      _ → Left $ TypeMismatch "Expecting exactly one element"

    res ← CA.decode codec json ∷ _ a

    pure $ Argument res

instance gProductProduct ∷
  ( GProduct codec rep
  , GProduct codecs reps
  ) ⇒
  GProduct (codec /\ codecs) (Product rep reps) where
  gProductEncode ∷ Encoding → (codec /\ codecs) → Product rep reps → Array Json
  gProductEncode encoding (codec /\ codecs) (Product rep reps) =
    let
      r1 = gProductEncode encoding codec rep ∷ Array Json
      r2 = gProductEncode encoding codecs reps ∷ Array Json
    in
      r1 <> r2

  gProductDecode ∷ Encoding → (codec /\ codecs) → Array Json → Either JsonDecodeError (Product rep reps)
  gProductDecode encoding (codec /\ codecs) jsons = do
    { head, tail } ← Array.uncons jsons # note (TypeMismatch "Expecting at least one element") ∷ _ { head ∷ Json, tail ∷ Array Json }
    rep ← gProductDecode encoding codec [ head ] ∷ _ rep
    reps ← gProductDecode encoding codecs tail ∷ _ reps
    pure $ Product rep reps

--------------------------------------------------------------------------------

checkTag ∷ Encoding → Object Json → String → Either JsonDecodeError Unit
checkTag encoding obj expectedTag = do
  val ←
    ( Obj.lookup encoding.tagKey obj
        # note (TypeMismatch ("Expecting a tag property `" <> encoding.tagKey <> "`"))
    ) ∷ _ Json
  tag ← CA.decode CA.string val ∷ _ String
  unless (tag == expectedTag)
    $ throwError
    $ TypeMismatch ("Expecting tag `" <> expectedTag <> "`, got `" <> tag <> "`")

parseSingleField ∷ Encoding → Object Json → Either JsonDecodeError Json
parseSingleField encoding obj = do
  val ←
    ( Obj.lookup encoding.valuesKey obj
        # note (TypeMismatch ("Expecting a value property `" <> encoding.valuesKey <> "`"))
    ) ∷ _ Json
  if encoding.unwrapSingleArguments then
    pure val
  else do
    fields ← CA.decode CA.jarray val
    case fields of
      [ head ] → pure head
      _ → throwError $ TypeMismatch "Expecting exactly one element"

parseNoFields ∷ Encoding → Object Json → Either JsonDecodeError Unit
parseNoFields encoding obj = do
  when (not encoding.omitEmptyArguments) do
    val ←
      ( Obj.lookup encoding.valuesKey obj
          # note (TypeMismatch ("Expecting a value property `" <> encoding.valuesKey <> "`"))
      ) ∷ _ Json
    fields ← CA.decode CA.jarray val
    when (fields /= [])
      $ throwError
      $ TypeMismatch "Expecting an empty array"

parseManyFields ∷ Encoding → Object Json → Either JsonDecodeError (Array Json)
parseManyFields encoding obj = do
  val ←
    ( Obj.lookup encoding.valuesKey obj
        # note (TypeMismatch ("Expecting a value property `" <> encoding.valuesKey <> "`"))
    ) ∷ _ Json
  CA.decode CA.jarray val

encodeTagged ∷ Encoding → String → Maybe Json → Json
encodeTagged encoding tag maybeJson = encode jobject $ Obj.fromFoldable $ catMaybes
  [ Just (encoding.tagKey /\ CA.encode CA.string tag)
  , map (\json → encoding.valuesKey /\ json) maybeJson
  ]