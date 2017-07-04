module Data.Codec.Argonaut.Common
  ( module Data.Codec.Argonaut.Common
  , module Data.Codec.Argonaut
  ) where

import Prelude hiding (map)

import Data.Argonaut.Core as J
import Data.Array as A
import Data.Bifunctor as BF
import Data.Codec (basicCodec)
import Data.Codec.Argonaut (JIndexedCodec, JPropCodec, JsonCodec, JsonDecodeError(..), array, boolean, char, decode, encode, index, indexedArray, int, jarray, jobject, json, null, number, object, printJsonDecodeError, prop, record, recordProp, string, (<~<), (~))
import Data.Codec.Argonaut.Sum (Tag(..), taggedSum)
import Data.Either (Either(..))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.StrMap as SM
import Data.StrMap.ST as SMST
import Data.Tuple (Tuple(..), fst, snd)

-- | A codec for `Maybe` values.
maybe ∷ ∀ a. JsonCodec a → JsonCodec (Maybe a)
maybe codec = basicCodec dec enc
  where
  dec j = do
    obj ← decode jobject j
    tag ← decode (prop "tag" string) obj
    case tag of
      "Just" → Just <$> decode (prop "value" codec) obj
      "Nothing" → pure Nothing
      _ → Left (AtKey "tag" (UnexpectedValue tag))
  enc x = encode jobject $ SM.pureST do
    obj ← SMST.new
    case x of
      Nothing →
        SMST.poke obj "tag" (J.fromString "Nothing")
      Just a → do
        _ ← SMST.poke obj "tag" (J.fromString "Just")
        SMST.poke obj "value" (encode codec a)

-- | A codec for `Tuple` values.
-- |
-- | Encodes as a two-element array in JSON.
tuple ∷ ∀ a b. JsonCodec a → JsonCodec b → JsonCodec (Tuple a b)
tuple codecA codecB = indexedArray "Tuple" $
  Tuple
    <$> fst ~ index 0 codecA
    <*> snd ~ index 1 codecB

-- | A codec for `Either` values.
either ∷ ∀ a b. JsonCodec a → JsonCodec b → JsonCodec (Either a b)
either codecA codecB = taggedSum dec enc
  where
  dec tag json = case tag of
    Tag "Left" → BF.bimap (AtKey "value") Left (decode codecA json)
    Tag "Right" → BF.bimap (AtKey "value") Right (decode codecB json)
    Tag t → Left (AtKey "tag" (UnexpectedValue t))
  enc = case _ of
    Left a → Tuple (Tag "Left") (encode codecA a)
    Right b → Tuple (Tag "Right") (encode codecB b)

-- | A codec for `List` values.
-- |
-- | Encodes as an array in JSON.
list ∷ ∀ a. JsonCodec a → JsonCodec (L.List a)
list = dimap A.fromFoldable L.fromFoldable <<< array

-- | A codec for `Map` values.
-- |
-- | Encodes as an array of two-element key/value arrays in JSON.
map ∷ ∀ a b. Ord a ⇒ JsonCodec a → JsonCodec b → JsonCodec (M.Map a b)
map codecA = dimap M.toUnfoldable M.fromFoldable <<< array <<< tuple codecA

-- | A codec for `StrMap` values.
-- |
-- | Encodes as an array of two-element key/value arrays in JSON.
strMap ∷ ∀ a. JsonCodec a → JsonCodec (SM.StrMap a)
strMap = dimap SM.toUnfoldable SM.fromFoldable <<< array <<< tuple string
