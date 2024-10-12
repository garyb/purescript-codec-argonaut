module Data.Codec.Argonaut.Sum
  ( class Count
  , class GSum
  , enumSum
  , gSumDecode
  , gSumEncode
  , sum
  , sumWith
  , taggedSum
  )
  where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core (Json, fromString) as J
import Data.Argonaut.Core (Json, fromString) as J
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec (Codec', codec', decode, encode)
import Data.Codec as C
import Data.Codec as Codec
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..))
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..), from, to)
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe (Maybe(..), maybe)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object (Object)
import Foreign.Object as FO
import Foreign.Object as Obj
import Foreign.Object.ST as FOST
import Prim.Int (class Add)
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

sum ∷ ∀ r rep a. Generic a rep ⇒ GSum (Record r) (Object Json) rep ⇒ String → Record r → JsonCodec a
sum = sumWith defaultEncoding

sumWith ∷ ∀ r rep a. GSum (Record r) (Object Json) rep ⇒ Generic a rep ⇒ Encoding → String → Record r → JsonCodec a
sumWith encoding name r = codec' decode encode
  where
  encode ∷ a → Json
  encode val = C.encode CA.jobject $ gSumEncode encoding r $ from val

  decode ∷ Json → Either JsonDecodeError a
  decode json = do
    obj ← C.decode CA.jobject json
    rep ← gSumDecode encoding r obj
    pure $ to rep

codecTagged ∷ Encoding → Int → Codec'' (Object Json) Tagged
codecTagged encoding n = codec' decode encode
  where
  encode ∷ Tagged → Object Json
  encode { tag, fields } =
    Obj.fromFoldable
      case Array.uncons fields of
        Nothing | encoding.omitEmptyArguments →
          [ (encoding.tagKey /\ CA.encode CA.string tag) ]
        Just { head, tail: [] } | encoding.unwrapSingleArguments →
          [ (encoding.tagKey /\ CA.encode CA.string tag)
          , (encoding.valuesKey /\ head)
          ]
        _ →
          [ (encoding.tagKey /\ encodeString tag)
          , (encoding.valuesKey /\ CA.encode CA.jarray fields)
          ]

  decode ∷ Object Json → Either JsonDecodeError Tagged
  decode obj = do
    tag ← note (TypeMismatch "String") $ Obj.lookup encoding.tagKey obj
    tagStr ∷ String ← CA.decode CA.string tag

    fields ← case n of
      0 | encoding.omitEmptyArguments → pure []
      1 | encoding.unwrapSingleArguments → do
        val ← note (TypeMismatch "") $ Obj.lookup encoding.valuesKey obj
        pure [ val ]
      _ → do
        val ← note (TypeMismatch "") $ Obj.lookup encoding.valuesKey obj
        CA.decode CA.jarray val

    pure { tag: tagStr, fields }

type Tagged =
  { tag ∷ String
  , fields ∷ Array Json
  }

class GSum ∷ Type → Type → Type → Constraint
class
  GSum codecs lo hi
  | codecs hi → lo
  where
  gSumEncode ∷ Encoding → codecs → hi → lo
  gSumDecode ∷ Encoding → codecs → lo → Either JsonDecodeError hi

instance gSumSum ∷
  ( GSum (Record r) (Object Json) lhs
  , GSum (Record r) (Object Json) rhs
  ) ⇒
  GSum (Record r) (Object Json) (Sum lhs rhs) where
  gSumEncode ∷ Encoding → Record r → Sum lhs rhs → Object Json
  gSumEncode encoding r = case _ of
    Inl lhs → gSumEncode encoding r lhs
    Inr rhs → gSumEncode encoding r rhs

  gSumDecode ∷ Encoding → Record r → Object Json → Either JsonDecodeError (Sum lhs rhs)
  gSumDecode encoding r tagged = (Inl <$> gSumDecode encoding r tagged) <|> (Inr <$> gSumDecode encoding r tagged)

instance gSumConstructor ∷
  ( Row.Cons name codecs r' r
  , GSum codecs (Array Json) args
  , IsSymbol name
  , Count codecs countFields
  , Reflectable countFields Int
  ) ⇒
  GSum (Record r) (Object Json) (Constructor name args) where
  gSumEncode ∷ Encoding → Record r → Constructor name args → Object Json
  gSumEncode encoding r (Constructor rep) = C.encode (codecTagged encoding countFields)
    { tag: reflectSymbol (Proxy ∷ Proxy name)
    , fields: gSumEncode encoding val rep
    }

    where
    countFields ∷ Int
    countFields = reflectType (Proxy ∷ Proxy countFields)

    val ∷ codecs
    val = Record.get (Proxy ∷ Proxy name) r

  gSumDecode ∷ Encoding → Record r → Object Json → Either JsonDecodeError (Constructor name args)
  gSumDecode encoding r obj = do
    { tag, fields } ← C.decode (codecTagged encoding countFields) obj

    unless (tag == reflectSymbol (Proxy ∷ Proxy name))
      $ throwError
      $ TypeMismatch "Wrong tag"

    rep ← gSumDecode encoding val fields
    pure $ Constructor rep

    where
    countFields ∷ Int
    countFields = reflectType (Proxy ∷ Proxy countFields)

    val ∷ codecs
    val = Record.get (Proxy ∷ Proxy name) r

instance gSumProduct ∷
  ( GSum codec (Array Json) rep
  , GSum codecs (Array Json) reps
  ) ⇒
  GSum (codec /\ codecs) (Array Json) (Product rep reps) where
  gSumEncode ∷ Encoding → (codec /\ codecs) → Product rep reps → Array Json
  gSumEncode encoding (codec /\ codecs) (Product rep reps) =
    gSumEncode encoding codec rep <> gSumEncode encoding codecs reps

  gSumDecode ∷ Encoding → (codec /\ codecs) → Array Json → Either JsonDecodeError (Product rep reps)
  gSumDecode encoding (codec /\ codecs) jsons = do
    { head, tail } ← note (TypeMismatch "Expecting at least one element") $ Array.uncons jsons
    rep ← gSumDecode encoding codec [ head ]
    reps ← gSumDecode encoding codecs tail
    pure $ Product rep reps

instance gSumArgument ∷ GSum (JsonCodec a) (Array Json) (Argument a) where
  gSumEncode ∷ Encoding → JsonCodec a → Argument a → Array Json
  gSumEncode _ codec (Argument val) = [ encode codec val ]

  gSumDecode ∷ Encoding → Codec'' Json a → Array Json → Either JsonDecodeError (Argument a)
  gSumDecode _ codec jsons = do
    case Array.uncons jsons of
      Just { head, tail: [] } → Argument <$> decode codec head
      _ → throwError $ TypeMismatch "Expecting exactly one element"

instance gSumNoArguments ∷ GSum Unit (Array Json) NoArguments where
  gSumEncode ∷ Encoding → Unit → NoArguments → Array Json
  gSumEncode _ _ _ = []

  gSumDecode ∷ Encoding → Unit → Array Json → Either JsonDecodeError NoArguments
  gSumDecode _ _ jsons = do
    when (Array.length jsons /= 0)
      $ throwError
      $ TypeMismatch "Expecting an empty array"
    pure NoArguments

type Codec'' lo hi = Codec' (Either JsonDecodeError) lo hi

class Count ∷ Type → Int → Constraint
class Count xs n | xs → n

instance countZero ∷ Count Unit 0
else instance countN ∷ (Count xs n, Add n 1 n') ⇒ Count (JsonCodec a /\ xs) n'
else instance countOne ∷ Count a 1