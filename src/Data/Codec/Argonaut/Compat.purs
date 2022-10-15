-- | Codecs that are compatible with `purescript-argonaut-codecs`.
module Data.Codec.Argonaut.Compat
  ( module Data.Codec.Argonaut.Compat
  , module Data.Codec.Argonaut.Common
  ) where

import Prelude hiding (void)

import Data.Argonaut.Core as J
import Data.Bifunctor as BF
import Data.Codec (basicCodec, mapCodec)
import Data.Codec.Argonaut.Common (JIndexedCodec, JPropCodec, JsonCodec, JsonDecodeError(..), array, boolean, char, codePoint, coercible, decode, either, encode, fix, index, indexedArray, int, jarray, jobject, json, list, named, nonEmptyArray, nonEmptyList, nonEmptySet, nonEmptyString, null, number, object, printJsonDecodeError, prismaticCodec, prop, record, recordProp, recordPropOptional, set, string, tuple, void, (<~<), (>~>), (~))
import Data.Either (Either)
import Data.Functor as F
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO

-- | A codec for `Maybe` values.
-- |
-- | Encodes and decodes `Nothing` as `null`
-- |
-- | Note: this codec cannot represent nested `Maybe` values in a lossless
-- | manner.
maybe ∷ ∀ a. JsonCodec a → JsonCodec (Maybe a)
maybe codec = basicCodec dec enc
  where
  dec ∷ J.Json → Either JsonDecodeError (Maybe a)
  dec j
    | J.isNull j = pure Nothing
    | otherwise = BF.bimap (Named "Maybe") Just ((decode codec j))

  enc ∷ Maybe a → J.Json
  enc = case _ of
    Nothing → J.jsonNull
    Just a → encode codec a

-- | A codec for `StrMap` values.
-- |
-- | Encodes as a JSON object with the keys as properties.
-- |
-- | ```purescript
-- | encode (foreignObject int) (Foreign.Object.fromFoldable [Tuple "a" 1, Tuple "b" 2]) == "{ \"a\": 1, \"b\": 2}"
-- | ```
foreignObject ∷ ∀ a. JsonCodec a → JsonCodec (FO.Object a)
foreignObject codec =
  mapCodec
    (BF.lmap (Named "StrMap") <<< F.map fromArray <<< traverse decodeItem <<< FO.toUnfoldable)
    (F.map (encode codec))
    jobject
  where
  fromArray ∷ ∀ v. Array (Tuple String v) → FO.Object v
  fromArray = FO.fromFoldable

  decodeItem ∷ Tuple String J.Json → Either JsonDecodeError (Tuple String a)
  decodeItem (Tuple key value) =
    BF.bimap (AtKey key) (Tuple key) (decode codec value)
