-- | Codecs that are compatible with `purescript-argonaut-codecs`.
module Data.Codec.Argonaut.Compat
  ( module Data.Codec.Argonaut.Compat
  , module Data.Codec.Argonaut
  , module Common
  ) where

import Prelude

import Data.Argonaut.Core as J
import Data.Bifunctor as BF
import Data.Codec (basicCodec, mapCodec)
import Data.Codec.Argonaut (JIndexedCodec, JPropCodec, JsonCodec, JsonDecodeError(..), array, boolean, char, decode, encode, index, indexedArray, int, jarray, jobject, json, null, number, object, printJsonDecodeError, prop, record, recordProp, string, (<~<), (~))
import Data.Codec.Argonaut.Common (either, list, map, tuple) as Common
import Data.Either (Either)
import Data.Functor as F
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

-- | A codec for `Maybe` values.
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
strMap ∷ ∀ a. JsonCodec a → JsonCodec (SM.StrMap a)
strMap codec =
  mapCodec
    (BF.lmap (Named "StrMap") <<< F.map fromArray <<< traverse decodeItem <<< SM.toUnfoldable)
    (F.map (encode codec))
    jobject
  where
  fromArray ∷ ∀ v. Array (Tuple String v) → SM.StrMap v
  fromArray = SM.fromFoldable
  decodeItem ∷ Tuple String J.Json → Either JsonDecodeError (Tuple String a)
  decodeItem (Tuple key value) =
    BF.bimap (AtKey key) (Tuple key) (decode codec value)
