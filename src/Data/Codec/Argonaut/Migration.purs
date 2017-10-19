module Data.Codec.Argonaut.Migration
  ( addDefaultField
  , updateField
  , addDefaultOrUpdateField
  , renameField
  , nestForTagged
  ) where

import Prelude

import Data.Argonaut.Core as J
import Data.Codec (basicCodec)
import Data.Codec.Argonaut (JsonCodec)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.StrMap as SM
import Data.StrMap.ST as SMST
import Data.Tuple (Tuple(..), uncurry)

-- | When dealing with a JSON object that may be missing a field, this codec
-- | can be used to alter the JSON before parsing to ensure a default value is
-- | present instead.
addDefaultField ∷ String → J.Json → JsonCodec J.Json
addDefaultField field = addDefaultOrUpdateField field <<< fromMaybe

-- | Re-maps the value of a field in a JSON object.
updateField ∷ String → (J.Json → J.Json) → JsonCodec J.Json
updateField field = alterField field <<< map

-- | When dealing with a JSON object that may be missing a field, this codec
-- | can be used to alter the JSON before parsing to ensure a default value is
-- | present instead. Similar to `addDefaultField`, but allows existing values
-- | to be modified also.
addDefaultOrUpdateField ∷ String → (Maybe J.Json → J.Json) → JsonCodec J.Json
addDefaultOrUpdateField field = alterField field <<< map Just

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

-- | Prepares an object from a legacy codec for use in a `Variant` or
-- | `taggedSum` codec.
-- |
-- | For an input like:
-- | ```{ "tag": "tag", "x": 1, "y": 2, "z": 3 }```
-- | the result will be:
-- | ```{ "tag": "tag", "value": { "x": 1, "y": 2, "z": 3 } }```
-- |
-- | For an input like:
-- | ```{ "tag": "tag", "value": 1, "foo": 2 }```
-- | the result will be:
-- | ```{ "tag": "tag", "value": { "value": 1, "foo": 2 }```
-- |
-- | If the value is already in the expected form, where there is only `value`
-- | and no other keys (aside from `tag`):
-- | ```{ "tag": "tag", "value": true }```
-- | the result will be the same as the input.
-- |
-- | If the tag field is missing from the input, it will also be missing in the
-- | output.
nestForTagged ∷ JsonCodec J.Json
nestForTagged = basicCodec (pure <<< dec) id
  where
  dec ∷ J.Json → J.Json
  dec j = J.foldJsonObject j (J.fromObject <<< rewrite) j
  rewrite ∷ J.JObject → J.JObject
  rewrite obj =
    case SM.pop "tag" obj of
      Nothing → SM.pureST do
        result ← SMST.new
        SMST.poke result "value" (mkValue obj)
      Just (Tuple tagValue obj') → SM.pureST do
        result ← SMST.new
        _ ← SMST.poke result "tag" tagValue
        SMST.poke result "value" (mkValue obj')
  mkValue ∷ J.JObject → J.Json
  mkValue obj = case SM.pop "value" obj of
    Just (Tuple valueValue obj') | SM.isEmpty obj' → valueValue
    _ → J.fromObject obj

alterField ∷ String → (Maybe J.Json → Maybe J.Json) → JsonCodec J.Json
alterField field f = basicCodec (pure <<< dec) id
  where
  dec ∷ J.Json → J.Json
  dec j = J.foldJsonObject j (J.fromObject <<< setDefault) j
  setDefault ∷ J.JObject → J.JObject
  setDefault = SM.alter f field
