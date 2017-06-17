module Control.Monad.Codec.Argonaut
  ( JsonCodec
  , JObjectCodec
  , JArrayCodec
  , JsonDecodeError(..)
  , printJsonDecodeError
  , json
  , null
  , boolean
  , number
  , string
  , jarray
  , jobject
  , index
  , array
  , prop
  , object
  , module Exports
  ) where

import Prelude

import Control.Monad.Codec (BasicCodec, Codec, GCodec(..), basicCodec, bihoistGCodec, decode, encode)
import Control.Monad.Codec (decode, encode, (~), (<~<)) as Exports
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Writer (mapWriter, tell)
import Data.Argonaut.Core as J
import Data.Array as A
import Data.Bifunctor as BF
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int as I
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.String as S
import Data.StrMap as SM
import Data.Tuple (Tuple(..))

-- | Codec type for `Json` values.
type JsonCodec a = BasicCodec (Either JsonDecodeError) J.Json a

-- | Codec type for `JObject` prop/value pairs.
type JObjectCodec a =
  Codec
    (Either JsonDecodeError)
    J.JObject
    (L.List J.JAssoc)
    a a

-- | Codec type for `JArray` elements.
type JArrayCodec a =
  Codec
    (Either JsonDecodeError)
    J.JArray
    (L.List J.Json)
    a a

data JsonDecodeError
  = TypeMismatch String
  | UnexpectedValue String
  | AtIndex Int JsonDecodeError
  | AtKey String JsonDecodeError
  | Named String JsonDecodeError
  | MissingValue
  | SumDecodeFailed (L.List JsonDecodeError)

derive instance eqJsonDecodeError ∷ Eq JsonDecodeError
derive instance ordJsonDecodeError ∷ Ord JsonDecodeError
derive instance genericJsonDecodeError ∷ Generic JsonDecodeError _

instance semigroupJsonDecodeError ∷ Semigroup JsonDecodeError where
  append x y = SumDecodeFailed (x L.: y L.: L.Nil)

instance showJsonDecodeError ∷ Show JsonDecodeError where
  show = case _ of
    TypeMismatch ty → "(TypeMismatch " <> show ty <> ")"
    UnexpectedValue val → "(UnexpectedValue " <> show val <> ")"
    AtIndex i err → "(AtIndex " <> show i <> " " <> show err <> ")"
    AtKey k err → "(AtKey " <> show k <> " " <> show err <> ")"
    Named name err → "(Named " <> show name <> " " <> show err <> ")"
    MissingValue → "MissingValue"
    SumDecodeFailed errs → "(SumDecodeFailed " <> show errs <> ")"

printJsonDecodeError ∷ JsonDecodeError → String
printJsonDecodeError err =
  "An error occurred while decoding a JSON value:\n" <> go err
  where
    go = case _ of
      TypeMismatch ty → "  Expected value of type '" <> ty <> "'."
      UnexpectedValue val → "  Unexpected value '" <> val <> "'."
      AtIndex ix inner → "  At array index " <> show ix <> ":\n" <> go inner
      AtKey key inner → "  At object key " <> key <> ":\n" <> go inner
      Named name inner → "  Under a '" <> name <> "':\n" <> go inner
      MissingValue → "  No value was found."
      SumDecodeFailed cases →
        "  A sum type failed to decode. Case failures are as follows:\n"
        <> S.joinWith "\n" (A.fromFoldable (go <$> cases))

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

-- | A codec for an `Array` values in `Json`.
jarray ∷ JsonCodec J.JArray
jarray = jsonPrimCodec "Array" J.toArray J.fromArray

-- | A codec for `Object` values in `Json`.
jobject ∷ JsonCodec J.JObject
jobject = jsonPrimCodec "Object" J.toObject J.fromObject

index ∷ ∀ a. Int → JsonCodec a → JArrayCodec a
index ix codec = GCodec dec enc
  where
  dec = ReaderT \xs →
    BF.lmap (AtIndex ix) case A.index xs ix of
      Just val → decode codec val
      Nothing → Left MissingValue
  enc val = do
    tell (pure (encode codec val))
    pure val

array ∷ ∀ a. String → JArrayCodec a → JsonCodec a
array name =
  bihoistGCodec
    (\r → ReaderT (runReaderT r <=< decode jarray))
    (mapWriter (BF.rmap (encode jarray <<< A.fromFoldable)))

prop ∷ ∀ a. String → JsonCodec a → JObjectCodec a
prop key codec = GCodec dec enc
  where
  dec ∷ ReaderT J.JObject (Either JsonDecodeError) a
  dec = ReaderT \obj →
    BF.lmap (AtKey key) case SM.lookup key obj of
      Just val → decode codec val
      Nothing → Left MissingValue
  enc val = do
    tell (pure (Tuple key (encode codec val)))
    pure val

object ∷ ∀ a. String → JObjectCodec a → JsonCodec a
object name =
  bihoistGCodec
    (\r → ReaderT (runReaderT r <=< decode jobject))
    (mapWriter (BF.rmap (encode jobject <<< SM.fromFoldable)))

jsonPrimCodec
  ∷ ∀ a
   . String
  → (J.Json → Maybe a)
  → (a → J.Json)
  → JsonCodec a
jsonPrimCodec ty f =
  basicCodec (maybe (Left (TypeMismatch ty)) pure <<< f)
