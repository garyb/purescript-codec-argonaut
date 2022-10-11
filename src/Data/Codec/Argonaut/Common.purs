module Data.Codec.Argonaut.Common
  ( module Data.Codec.Argonaut.Common
  , module Data.Codec.Argonaut
  ) where

import Prelude hiding (map)

import Data.Array as A
import Data.Codec.Argonaut (JIndexedCodec, JPropCodec, JsonCodec, JsonDecodeError(..), array, boolean, char, decode, encode, fix, index, indexedArray, int, jarray, jobject, json, null, number, object, printJsonDecodeError, prop, record, recordProp, recordPropOptional, string, (<~<), (~))
import Data.Codec.Argonaut.Sum (taggedSum)
import Data.Either (Either(..))
import Data.Functor as F
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.Tuple (Tuple(..), fst, snd)
import Foreign.Object as FO

-- | A codec for `Maybe` values.
-- |
-- | NOTE: This is not suitable to en/decode null values. If you need these kinds of codecs,
-- | look into `Data.Codec.Argonaut.Compat`
maybe ∷ ∀ a. JsonCodec a → JsonCodec (Maybe a)
maybe codec = taggedSum "Maybe" printTag parseTag dec enc
  where
  printTag = case _ of
    false → "Nothing"
    true → "Just"
  parseTag = case _ of
    "Nothing" → Just false
    "Just" → Just true
    _ → Nothing
  dec = case _ of
    false → Left Nothing
    true → Right (F.map Just <<< decode codec)
  enc = case _ of
    Nothing → Tuple false Nothing
    Just a → Tuple true (Just (encode codec a))

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
either codecA codecB = taggedSum "Either" printTag parseTag dec enc
  where
  printTag = case _ of
    true → "Left"
    false → "Right"
  parseTag = case _ of
    "Left" → Just true
    "Right" → Just false
    _ → Nothing
  dec = case _ of
    true → Right (F.map Left <<< decode codecA)
    false → Right (F.map Right <<< decode codecB)
  enc = case _ of
    Left a → Tuple true (Just (encode codecA a))
    Right b → Tuple false (Just (encode codecB b))

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
foreignObject ∷ ∀ a. JsonCodec a → JsonCodec (FO.Object a)
foreignObject = dimap FO.toUnfoldable FO.fromFoldable <<< array <<< tuple string
