-- | Codecs that are compatible with `purescript-argonaut-codecs`.
module Data.Codec.Argonaut.Compat
  ( module Data.Codec.Argonaut.Compat
  , module Data.Codec.Argonaut
  , module Common 
  ) where

import Prelude

import Control.Monad.Except (ExceptT, withExceptT)
import Data.Argonaut.Core as J
import Data.Codec (basicCodec, mapCodec)
import Data.Codec as C
import Data.Codec.Argonaut (JIndexedCodec, JIndexedCodecT, JPropCodec, JPropCodecT, JsonCodec, JsonCodecT, JsonDecodeError(..), array, boolean, char, codePoint, decode, encode, fix, index, indexedArray, int, jarray, jobject, json, null, number, object, printJsonDecodeError, prismaticCodec, prop, record, recordProp, recordPropOptional, string, (<~<), (>~>), (~))
import Data.Codec.Argonaut.Common (either, list, map, tuple) as Common
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
maybe ∷ ∀ m a. Monad m ⇒ JsonCodecT m a → JsonCodecT m (Maybe a)
maybe codec = basicCodec dec enc
  where
  dec ∷ J.Json → ExceptT JsonDecodeError m (Maybe a)
  dec j
    | J.isNull j = pure Nothing
    | otherwise = withExceptT (Named "Maybe") (Just <$> (C.decode codec j))
  enc ∷ Maybe a → J.Json
  enc = case _ of
    Nothing → J.jsonNull
    Just a → C.encode codec a

-- | A codec for `StrMap` values.
-- |
-- | Encodes as a JSON object with the keys as properties.
-- |
-- | ```purescript
-- | encode (foreignObject int) (Foreign.Object.fromFoldable [Tuple "a" 1, Tuple "b" 2]) == "{ \"a\": 1, \"b\": 2}"
-- | ```
foreignObject ∷ ∀ m a. Monad m ⇒ JsonCodecT m a → JsonCodecT m (FO.Object a)
foreignObject codec =
  mapCodec
    (withExceptT (Named "StrMap") <<< F.map fromArray <<< traverse decodeItem <<< FO.toUnfoldable)
    (F.map (C.encode codec))
    jobject
  where
  fromArray ∷ ∀ v. Array (Tuple String v) → FO.Object v
  fromArray = FO.fromFoldable
  decodeItem ∷ Tuple String J.Json → ExceptT JsonDecodeError m (Tuple String a)
  decodeItem (Tuple key value) =
    withExceptT (AtKey key) (Tuple key <$> C.decode codec value)
