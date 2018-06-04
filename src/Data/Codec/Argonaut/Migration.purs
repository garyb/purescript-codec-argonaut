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
import Foreign.Object as FO
import Foreign.Object.ST as FOST
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
renameField oldName newName = basicCodec (pure <<< dec) identity
  where
  dec ∷ J.Json → J.Json
  dec j = J.caseJsonObject j (J.fromObject <<< rename) j
  rename ∷ FO.Object J.Json → FO.Object J.Json
  rename obj = maybe obj (uncurry (FO.insert newName)) (FO.pop oldName obj)

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
nestForTagged = basicCodec (pure <<< dec) identity
  where
  dec ∷ J.Json → J.Json
  dec j = J.caseJsonObject j (J.fromObject <<< rewrite) j
  rewrite ∷ FO.Object J.Json → FO.Object J.Json
  rewrite obj =
    case FO.pop "tag" obj of
      Nothing → FO.runST do
        result ← FOST.new
        FOST.poke "value" (mkValue obj) result
      Just (Tuple tagValue obj') → FO.runST do
        result ← FOST.new
        _ ← FOST.poke "tag" tagValue result
        FOST.poke "value" (mkValue obj') result
  mkValue ∷ FO.Object J.Json → J.Json
  mkValue obj = case FO.pop "value" obj of
    Just (Tuple valueValue obj') | FO.isEmpty obj' → valueValue
    _ → J.fromObject obj

alterField ∷ String → (Maybe J.Json → Maybe J.Json) → JsonCodec J.Json
alterField field f = basicCodec (pure <<< dec) identity
  where
  dec ∷ J.Json → J.Json
  dec j = J.caseJsonObject j (J.fromObject <<< setDefault) j
  setDefault ∷ FO.Object J.Json → FO.Object J.Json
  setDefault = FO.alter f field
