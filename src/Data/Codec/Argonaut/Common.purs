module Data.Codec.Argonaut.Common
  ( either
  , foreignObject
  , list
  , map
  , maybe
  , module Data.Codec.Argonaut
  , nonEmptyArray
  , nonEmptyList
  , nonEmptySet
  , nonEmptyString
  , set
  , strMap
  , tuple
  ) where

import Prelude hiding (map, void)

import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.Argonaut (JIndexedCodec, JPropCodec, JsonCodec, JsonDecodeError(..), array, boolean, char, codePoint, coercible, decode, encode, fix, index, indexedArray, int, jarray, jobject, json, named, null, number, object, printJsonDecodeError, prismaticCodec, prop, record, recordProp, recordPropOptional, string, void, (<~<), (>~>), (~))
import Data.Codec.Argonaut.Sum (taggedSum)
import Data.Either (Either(..))
import Data.FoldableWithIndex (forWithIndex_)
import Data.Functor as F
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.Set as Set
import Data.Set.NonEmpty as NESet
import Data.String.NonEmpty as NEString
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Foreign.Object as Object
import Foreign.Object.ST as ObjectST

-- | A codec for `NonEmptyString` values.
-- |
-- | Encodes as the standard type in JSON, but will fail to decode if the string is empty.
nonEmptyString ∷ JsonCodec NEString.NonEmptyString
nonEmptyString = prismaticCodec "NonEmptyString" NEString.fromString NEString.toString string

-- | A codec for `NonEmptyArray` values.
-- |
-- | Encodes as the standard type in JSON, but will fail to decode if the array is empty.
nonEmptyArray ∷ ∀ a. JsonCodec a → JsonCodec (NEA.NonEmptyArray a)
nonEmptyArray codec = prismaticCodec "NonEmptyArray" NEA.fromArray NEA.toArray (array codec)

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
list ∷ ∀ a. JsonCodec a → JsonCodec (List.List a)
list codec = dimap Array.fromFoldable List.fromFoldable (named "List" (array codec))

-- | A codec for `NonEmptyList` values.
-- |
-- | Encodes as an array in JSON.
nonEmptyList ∷ ∀ a. JsonCodec a → JsonCodec (NEL.NonEmptyList a)
nonEmptyList codec = prismaticCodec "NonEmptyList" NEL.fromFoldable Array.fromFoldable (array codec)

-- | A codec for `Map` values.
-- |
-- | Encodes as an array of two-element key/value arrays in JSON.
map ∷ ∀ a b. Ord a ⇒ JsonCodec a → JsonCodec b → JsonCodec (Map.Map a b)
map codecA codecB = dimap Map.toUnfoldable (Map.fromFoldable) (named "Map" (array (tuple codecA codecB)))

-- | A codec for `Map` values which have string keys.
-- |
-- | Encodes as an object in JSON.
strMap ∷ ∀ a. JsonCodec a → JsonCodec (Map.Map String a)
strMap codec = Codec.basicCodec decode encode
  where
  encode ∷ Map.Map String a → Json
  encode msa = Codec.encode jobject $ Object.runST do
    obj ← ObjectST.new
    forWithIndex_ msa \k v → ObjectST.poke k (Codec.encode codec v) obj
    pure obj

  decode ∷ Json → Either JsonDecodeError (Map.Map String a)
  decode json = do
    r ← Map.fromFoldableWithIndex <$> Codec.decode jobject json
    traverseWithIndex (\k v → lmap (AtKey k) (Codec.decode codec v)) r

-- | A codec for `Set` values.
-- |
-- | Encodes as an array in JSON.
set ∷ ∀ a. Ord a ⇒ JsonCodec a → JsonCodec (Set.Set a)
set codec = dimap Array.fromFoldable Set.fromFoldable (named "Set" (array codec))

-- | A codec for `NonEmptySet` values.
-- |
-- | Encodes as an array in JSON.
nonEmptySet ∷ ∀ a. Ord a ⇒ JsonCodec a → JsonCodec (NESet.NonEmptySet a)
nonEmptySet codec = prismaticCodec "NonEmptySet" NESet.fromFoldable NESet.toUnfoldable (array codec)

-- | A codec for `Object` values.
-- |
-- | Encodes as an array of two-element key/value arrays in JSON.
foreignObject ∷ ∀ a. JsonCodec a → JsonCodec (Object.Object a)
foreignObject = dimap Object.toUnfoldable Object.fromFoldable <<< array <<< tuple string
