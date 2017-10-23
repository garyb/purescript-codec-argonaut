module Data.Codec.Argonaut
  ( JsonCodec
  , JsonDecodeError(..)
  , printJsonDecodeError
  , json
  , null
  , boolean
  , number
  , int
  , string
  , char
  , jarray
  , jobject
  , array
  , JIndexedCodec
  , indexedArray
  , index
  , JPropCodec
  , object
  , prop
  , record
  , recordProp
  , fix
  , prismaticCodec
  , module Exports
  ) where

import Prelude

import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Writer (Writer, writer, mapWriter)
import Data.Argonaut.Core as J
import Data.Array as A
import Data.Bifunctor as BF
import Data.Codec (BasicCodec, Codec, GCodec(..), basicCodec, bihoistGCodec, decode, encode)
import Data.Codec (decode, encode, (~), (<~<)) as Exports
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as I
import Data.List ((:))
import Data.List as L
import Data.Maybe (Maybe(..), maybe, fromJust)
import Data.Profunctor.Star (Star(..))
import Data.StrMap as SM
import Data.String as S
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

-- | Codec type for `Json` values.
type JsonCodec a = BasicCodec (Either JsonDecodeError) J.Json a

-- | Error type for failures while decoding.
data JsonDecodeError
  = TypeMismatch String
  | UnexpectedValue J.Json
  | AtIndex Int JsonDecodeError
  | AtKey String JsonDecodeError
  | Named String JsonDecodeError
  | MissingValue

derive instance eqJsonDecodeError ∷ Eq JsonDecodeError
derive instance ordJsonDecodeError ∷ Ord JsonDecodeError
derive instance genericJsonDecodeError ∷ Generic JsonDecodeError _

instance showJsonDecodeError ∷ Show JsonDecodeError where
  show err = genericShow err

-- | Prints a `JsonDecodeError` as a somewhat readable error message.
printJsonDecodeError ∷ JsonDecodeError → String
printJsonDecodeError err =
  "An error occurred while decoding a JSON value:\n" <> go err
  where
    go = case _ of
      TypeMismatch ty → "  Expected value of type '" <> ty <> "'."
      UnexpectedValue val → "  Unexpected value " <> J.stringify val <> "."
      AtIndex ix inner → "  At array index " <> show ix <> ":\n" <> go inner
      AtKey key inner → "  At object key " <> key <> ":\n" <> go inner
      Named name inner → "  Under '" <> name <> "':\n" <> go inner
      MissingValue → "  No value was found."

-- | The "identity codec" for `Json` values.
json ∷ JsonCodec J.Json
json = basicCodec pure id

-- | A codec for `null` values in `Json`.
null ∷ JsonCodec J.JNull
null = jsonPrimCodec "Null" J.toNull J.fromNull

-- | A codec for `Boolean` values in `Json`.
boolean ∷ JsonCodec Boolean
boolean = jsonPrimCodec "Boolean" J.toBoolean J.fromBoolean

-- | A codec for `Number` values in `Json`.
number ∷ JsonCodec Number
number = jsonPrimCodec "Number" J.toNumber J.fromNumber

-- | A codec for `Int` values in `Json`.
int ∷ JsonCodec Int
int = jsonPrimCodec "Int" (I.fromNumber <=< J.toNumber) (J.fromNumber <<< I.toNumber)

-- | A codec for `String` values in `Json`.
string ∷ JsonCodec String
string = jsonPrimCodec "String" J.toString J.fromString

-- | A codec for `Char` values in `Json`.
char ∷ JsonCodec Char
char = jsonPrimCodec "Char" (S.toChar <=< J.toString) (J.fromString <<< S.singleton)

-- | A codec for `Void` values.
void ∷ JsonCodec Void
void = jsonPrimCodec "Void" (const Nothing) absurd

-- | A codec for a `JArray` values in `Json`. This does not decode the values
-- | of the array, for that use `array` for a general array decoder, or
-- | `indexedArray` with `index` to decode fixed length array encodings.
jarray ∷ JsonCodec J.JArray
jarray = jsonPrimCodec "Array" J.toArray J.fromArray

-- | A codec for `JObject` values in `Json`.
jobject ∷ JsonCodec J.JObject
jobject = jsonPrimCodec "Object" J.toObject J.fromObject

-- | A codec for `Array` values.
-- |```purescript
-- | decodeIntArray ∷ Json → Either JsonDecodeError (Array Int)
-- | decodeIntArray = decode (array int)
-- |```

array ∷ ∀ a. JsonCodec a → JsonCodec (Array a)
array codec = GCodec dec enc
  where
  dec = ReaderT \j →
    traverse (\(Tuple ix j') → BF.lmap (AtIndex ix) (decode codec j'))
      <<< A.mapWithIndex Tuple
      =<< decode jarray j
  enc = Star \xs → writer $ Tuple xs (J.fromArray (map (encode codec) xs))

-- | Codec type for specifically indexed `JArray` elements.
type JIndexedCodec a =
  Codec
    (Either JsonDecodeError)
    J.JArray
    (L.List J.Json)
    a a

-- | A codec for types that are encoded as an array with a specific layout.
indexedArray ∷ ∀ a. String → JIndexedCodec a → JsonCodec a
indexedArray name =
  bihoistGCodec
    (\r → ReaderT (BF.lmap (Named name) <<< runReaderT r <=< decode jarray))
    (mapWriter (BF.rmap (J.fromArray <<< A.fromFoldable)))

-- | A codec for an item in an `indexedArray`.
index ∷ ∀ a. Int → JsonCodec a → JIndexedCodec a
index ix codec = GCodec dec enc
  where
  dec = ReaderT \xs →
    BF.lmap (AtIndex ix) case A.index xs ix of
      Just val → decode codec val
      Nothing → Left MissingValue
  enc = Star \val → writer $ Tuple val (pure (encode codec val))

-- | Codec type for `JObject` prop/value pairs.
type JPropCodec a =
  Codec
    (Either JsonDecodeError)
    J.JObject
    (L.List J.JAssoc)
    a a

-- | A codec for objects that are encoded with specific properties.
object ∷ ∀ a. String → JPropCodec a → JsonCodec a
object name =
  bihoistGCodec
    (\r → ReaderT (BF.lmap (Named name) <<< runReaderT r <=< decode jobject))
    (mapWriter (BF.rmap (J.fromObject <<< SM.fromFoldable)))

-- | A codec for a property of an object.
prop ∷ ∀ a. String → JsonCodec a → JPropCodec a
prop key codec = GCodec dec enc
  where
  dec ∷ ReaderT J.JObject (Either JsonDecodeError) a
  dec = ReaderT \obj →
    BF.lmap (AtKey key) case SM.lookup key obj of
      Just val → decode codec val
      Nothing → Left MissingValue
  enc ∷ Star (Writer (L.List J.JAssoc)) a a
  enc = Star \val → writer $ Tuple val (pure (Tuple key (encode codec val)))

-- | The starting value for a object-record codec. Used with `recordProp` it
-- | provides a convenient method for defining codecs for record types that
-- | encode into JSON objects of the same shape.
-- |
-- | For example:
-- | ```
-- | myRecordCodec =
-- |   object "MyRecord" $ record
-- |     # recordProp (SProxy :: SProxy "tag") tagCodec
-- |     # recordProp (SProxy :: SProxy "value") valueCodec
-- | ```
record ∷ JPropCodec {}
record = GCodec (pure {}) (Star \val → writer (Tuple val L.Nil))

-- | Used with `record` to define codecs for record types that encode into JSON
-- | objects of the same shape. See the comment on `record` for an example.
recordProp
  ∷ ∀ p a r r'
  . IsSymbol p
  ⇒ RowCons p a r r'
  ⇒ SProxy p
  → JsonCodec a
  → JPropCodec (Record r)
  → JPropCodec (Record r')
recordProp p codecA codecR =
  let key = reflectSymbol p in GCodec (dec' key) (enc' key)
  where
    dec' ∷ String → ReaderT J.JObject (Either JsonDecodeError) (Record r')
    dec' key = ReaderT \obj → do
      r ← decode codecR obj
      a ← BF.lmap (AtKey key) case SM.lookup key obj of
        Just val → decode codecA val
        Nothing → Left MissingValue
      pure $ unsafeSet key a r
    enc' ∷ String → Star (Writer (L.List J.JAssoc)) (Record r') (Record r')
    enc' key = Star \val →
      writer $ Tuple val
        $ Tuple key (encode codecA (unsafeGet key val))
        : encode codecR (unsafeForget val)
    unsafeForget ∷ Record r' → Record r
    unsafeForget = unsafeCoerce
    unsafeSet ∷ String → a → Record r → Record r'
    unsafeSet key a = unsafeCoerce <<< SM.insert key a <<< unsafeCoerce
    unsafeGet ∷ String → Record r' → a
    unsafeGet s = unsafePartial fromJust <<< SM.lookup s <<< unsafeCoerce

jsonPrimCodec
  ∷ ∀ a
   . String
  → (J.Json → Maybe a)
  → (a → J.Json)
  → JsonCodec a
jsonPrimCodec ty f =
  basicCodec (maybe (Left (TypeMismatch ty)) pure <<< f)

-- | Helper function for defining recursive codecs.
fix ∷ ∀ a. (JsonCodec a → JsonCodec a) → JsonCodec a
fix f =
  basicCodec
    (\x → decode (f (fix f)) x)
    (\x → encode (f (fix f)) x)

-- | Adapts an existing codec with a pair of functions to allow a value to be
-- | further refined. If the inner decoder fails an `UnexpectedValue` error will
-- | be raised for JSON input.
-- |
-- | This function is named as such as the pair of functions it accepts
-- | correspond with the `preview` and `view` functions of a `Prism`-style lens.
-- |
-- | For example, in order to parse a mapping from an enum to strings, which
-- | doesn't match up nicely with `Data.Codec.Argonaut.Sum.enumSum` we can use
-- | prismaticCodec:
-- |
-- | ```purescript
-- | data Direction = North | South | West | East
-- |
-- | directionCodec :: JsonCodec Direction
-- | directionCodec = prismaticCodec dec enc string
-- |   where
-- |     dec = case _ of
-- |       "N" -> Just North
-- |       "S" -> Just South
-- |       "W" -> Just West
-- |       "E" -> Just East
-- |       _ -> Nothing
-- |
-- |     enc = case _ of
-- |       North -> "N"
-- |       South -> "S"
-- |       West -> "W"
-- |       East -> "E"
-- | ```
prismaticCodec ∷ ∀ a b. (a → Maybe b) → (b → a) → JsonCodec a → JsonCodec b
prismaticCodec f g orig =
  basicCodec
    (\json' → note (UnexpectedValue json') <<< f =<< decode orig json')
    (encode orig <<< g)
