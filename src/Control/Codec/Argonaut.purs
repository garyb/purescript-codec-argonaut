module Control.Monad.Codec.Argonaut
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
  , module Exports
  ) where

import Prelude

import Control.Monad.Codec (BasicCodec, Codec, GCodec(..), basicCodec, bihoistGCodec, decode, encode)
import Control.Monad.Codec (decode, encode, (~), (<~<)) as Exports
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Writer (writer, mapWriter)
import Data.Argonaut.Core as J
import Data.Array as A
import Data.Bifunctor as BF
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as I
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.String as S
import Data.StrMap as SM
import Data.Traversable (traverse)
import Data.Profunctor.Star (Star(..))
import Data.Tuple (Tuple(..))

-- | Codec type for `Json` values.
type JsonCodec a = BasicCodec (Either JsonDecodeError) J.Json a

-- | Error type for failures while decoding.
data JsonDecodeError
  = TypeMismatch String
  | UnexpectedValue String
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
      UnexpectedValue val → "  Unexpected value '" <> val <> "'."
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
  enc = Star \val → writer $ Tuple val (pure (Tuple key (encode codec val)))

jsonPrimCodec
  ∷ ∀ a
   . String
  → (J.Json → Maybe a)
  → (a → J.Json)
  → JsonCodec a
jsonPrimCodec ty f =
  basicCodec (maybe (Left (TypeMismatch ty)) pure <<< f)
