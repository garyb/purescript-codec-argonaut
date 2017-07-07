module Data.Codec.Argonaut.Migration where

import Prelude

import Data.Argonaut.Core as J
import Data.Codec (basicCodec)
import Data.Codec.Argonaut (JsonCodec)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (uncurry)
import Data.StrMap as SM

-- | When dealing with a JSON object that may be missing a field, this codec
-- | can be used to alter the JSON before parsing to ensure a default value is
-- | present instead.
addDefaultField ∷ String → J.Json → JsonCodec J.Json
addDefaultField field defaultValue = basicCodec (pure <<< dec) id
  where
  dec ∷ J.Json → J.Json
  dec j = J.foldJsonObject j (J.fromObject <<< setDefault) j
  setDefault ∷ J.JObject → J.JObject
  setDefault = SM.alter (maybe (Just defaultValue) Just) field

-- | When dealing with a JSON object that has had a field name changed, this
-- | codec can be used to alter the JSON before parsing to ensure the new field
-- | name is used instead
renameField ∷ String → String → JsonCodec J.Json
renameField oldName newName = basicCodec (pure <<< dec) id
  where
  dec ∷ J.Json → J.Json
  dec j = J.foldJsonObject j (J.fromObject <<< rename) j
  rename ∷ J.JObject → J.JObject
  rename obj = maybe obj (uncurry (SM.insert newName)) (SM.pop oldName obj)
