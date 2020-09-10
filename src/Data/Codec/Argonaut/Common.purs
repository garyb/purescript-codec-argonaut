module Data.Codec.Argonaut.Common
  ( module Data.Codec.Argonaut.Common
  , module Data.Codec.Argonaut
  ) where

import Prelude hiding (map)

import Data.Array as A
import Data.Codec as C
import Data.Codec.Argonaut (JIndexedCodec, JIndexedCodecT, JPropCodec, JPropCodecT, JsonCodec, JsonCodecT, JsonDecodeError(..), array, boolean, char, codePoint, decode, encode, fix, index, indexedArray, int, jarray, jobject, json, null, number, object, printJsonDecodeError, prismaticCodec, prop, record, recordProp, recordPropOptional, string, (<~<), (>~>), (~))
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
maybe ∷ ∀ m a. Monad m ⇒ JsonCodecT m a → JsonCodecT m (Maybe a)
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
    true → Right (F.map Just <<< C.decode codec)
  enc = case _ of
    Nothing → Tuple false Nothing
    Just a → Tuple true (Just (C.encode codec a))

-- | A codec for `Tuple` values.
-- |
-- | Encodes as a two-element array in JSON.
tuple ∷ ∀ m a b. Monad m ⇒ JsonCodecT m a → JsonCodecT m b → JsonCodecT m (Tuple a b)
tuple codecA codecB = indexedArray "Tuple" $
  Tuple
    <$> fst ~ index 0 codecA
    <*> snd ~ index 1 codecB

-- | A codec for `Either` values.
either ∷ ∀ m a b. Monad m ⇒ JsonCodecT m a → JsonCodecT m b → JsonCodecT m (Either a b)
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
    true → Right (F.map Left <<< C.decode codecA)
    false → Right (F.map Right <<< C.decode codecB)
  enc = case _ of
    Left a → Tuple true (Just (C.encode codecA a))
    Right b → Tuple false (Just (C.encode codecB b))

-- | A codec for `List` values.
-- |
-- | Encodes as an array in JSON.
list ∷ ∀ m a. Monad m ⇒ JsonCodecT m a → JsonCodecT m (L.List a)
list = dimap A.fromFoldable L.fromFoldable <<< array

-- | A codec for `Map` values.
-- |
-- | Encodes as an array of two-element key/value arrays in JSON.
map ∷ ∀ m a b. Monad m ⇒ Ord a ⇒ JsonCodecT m a → JsonCodecT m b → JsonCodecT m (M.Map a b)
map codecA = dimap M.toUnfoldable M.fromFoldable <<< array <<< tuple codecA

-- | A codec for `StrMap` values.
-- |
-- | Encodes as an array of two-element key/value arrays in JSON.
foreignObject ∷ ∀ m a. Monad m ⇒ JsonCodecT m a → JsonCodecT m (FO.Object a)
foreignObject = dimap FO.toUnfoldable FO.fromFoldable <<< array <<< tuple string
