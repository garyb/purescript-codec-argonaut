-- | Codecs that are compatible with `purescript-argonaut-codecs`.
module Data.Codec.Argonaut.Compat
  ( module Data.Codec.Argonaut.Compat
  , module Data.Codec.Argonaut.Common
  ) where

import Prelude hiding (identity, map, void)

import Data.Argonaut.Core as J
import Data.Bifunctor as BF
import Data.Codec as Codec
import Data.Codec.Argonaut.Common (Codec(..), Codec', JIndexedCodec, JPropCodec, JsonCodec, JsonDecodeError(..), array, boolean, char, codePoint, codec, codec', coercible, decode, either, encode, fix, hoist, identity, index, indexedArray, int, jarray, jobject, json, list, map, named, nonEmptyArray, nonEmptyList, nonEmptySet, nonEmptyString, null, number, object, printJsonDecodeError, prismaticCodec, prop, record, recordProp, recordPropOptional, set, strMap, string, tuple, void, (<~<), (>~>), (~))
import Data.Either (Either(..))
import Data.Functor as Functor
import Data.List as L
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
maybe codec = Codec.codec' dec enc
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
  Codec.codec'
    (BF.lmap (Named "StrMap") <<< Functor.map fromArray <<< traverse decodeItem <<< FO.toUnfoldable <=< decode jobject)
    (encode jobject <<< Functor.map (encode codec))
  where
  fromArray ∷ ∀ v. Array (Tuple String v) → FO.Object v
  fromArray = FO.fromFoldable

  decodeItem ∷ Tuple String J.Json → Either JsonDecodeError (Tuple String a)
  decodeItem (Tuple key value) = BF.bimap (AtKey key) (Tuple key) (decode codec value)

data NothingEncoding
  = EmitNull
  | OmitKey

{- propOptional produces a codec that can handle keys which can be both
   optional and nullable.

   When a key is missing or its value is `null` then a `Nothing` value
   is produced during decoding.

   The encoding behavior is specified by the `NothingEncoding`
   argument. `EmitNull` means that a `Nothing` value will have its key
   present in the JSON with a `null` and `OmitKey` means that there will
   not be a corresponding key in the JSON for a `Nothing` value.
-}
propOptional ∷ ∀ a. NothingEncoding → String → JsonCodec a → JPropCodec (Maybe a)
propOptional nothingEncoding key codec =
  let
    decodeForeignObject mbObj =
      case mbObj of
        Just obj -> Codec.decode (maybe codec) obj
        Nothing -> Right Nothing

    decoder parentObj =
      BF.lmap
        (AtKey key)
        (decodeForeignObject $ FO.lookup key parentObj)

    encoder =
      case nothingEncoding of
        EmitNull -> emitNullEncoder
        OmitKey -> omitKeyEncoder

    emitNullEncoder mbObj =
      L.singleton $ Tuple key $ Codec.encode (maybe codec) mbObj

    omitKeyEncoder mbObj =
      case mbObj of
        Just obj ->
          L.singleton $ Tuple key $ Codec.encode codec obj
        Nothing ->
          L.Nil

  in Codec.codec decoder encoder
