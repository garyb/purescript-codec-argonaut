module Test.Migration where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.Argonaut.Core as J
import Data.Argonaut.Gen as GenJ
import Data.Codec ((<~<))
import Data.Codec.Argonaut.Common as JA
import Data.Codec.Argonaut.Migration as JAM
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.StrMap as SM
import Data.StrMap.Gen as GenSM
import Data.String.Gen (genAsciiString)
import Data.Tuple (Tuple(..))
import Test.QuickCheck (Result(..), QC, quickCheck, (===))
import Test.QuickCheck.Gen (Gen)
import Test.Util (genJObject, propCodec)

main :: QC () Unit
main = do
  log "Checking addDefaultField adds a field if it is missing"
  quickCheck propDefaultFieldAdded

  log "Checking addDefaultField preserves an existing value if it's already present"
  quickCheck propDefaultFieldPreservesOriginal

  log "Checking updateField updates an existing value if it's already present"
  quickCheck propUpdateFieldAltersOriginal

  log "Checking addDefaultOrUpdateField adds a field if it is missing"
  quickCheck propAddDefaultOrUpdateField

  log "Checking addDefaultOrUpdateField updates an existing value if it's already present"
  quickCheck propAddDefaultOrUpdateFieldAltersOriginal

  log "Checking renameField renames a field"
  quickCheck propDefaultFieldPreservesOriginal

  log "Checking nestForTagged moves all expected fields under `value`"
  quickCheck propNestForTaggedMovesUnderValue

  log "Checking nestForTagged is idempotent"
  quickCheck propNestForTaggedIdempotent

propDefaultFieldAdded ∷ Gen Result
propDefaultFieldAdded = do
  let expectedValue = J.fromString "it's here"
  missingKey ← genAsciiString
  input ← SM.delete missingKey <$> genJObject
  pure $ testMigrationCodec { key: missingKey, expectedValue, input }
    $ JA.jobject <~< JAM.addDefaultField missingKey expectedValue

propDefaultFieldPreservesOriginal ∷ Gen Result
propDefaultFieldPreservesOriginal = do
  let expectedValue = J.fromString "it's here"
  let unexpectedValue = J.fromString "it shouldn't be here"
  missingKey ← genAsciiString
  input ← SM.insert missingKey expectedValue <$> genJObject
  pure $ testMigrationCodec { key: missingKey, expectedValue, input }
    $ JA.jobject <~< JAM.addDefaultField missingKey unexpectedValue

propUpdateFieldAltersOriginal ∷ Gen Result
propUpdateFieldAltersOriginal = do
  let expectedValue = J.fromString "it's here"
  let unexpectedValue = J.fromString "it shouldn't be here"
  updateKey ← genAsciiString
  input ← SM.insert updateKey unexpectedValue <$> genJObject
  pure $ testMigrationCodec { key: updateKey, expectedValue, input }
    $ JA.jobject <~< JAM.updateField updateKey (const expectedValue)

propAddDefaultOrUpdateField ∷ Gen Result
propAddDefaultOrUpdateField = do
  let expectedValue = J.fromString "it's here"
  missingKey ← genAsciiString
  input ← SM.delete missingKey <$> genJObject
  pure $ testMigrationCodec { key: missingKey, expectedValue, input }
    $ JA.jobject <~< JAM.addDefaultOrUpdateField missingKey (fromMaybe expectedValue)

propAddDefaultOrUpdateFieldAltersOriginal ∷ Gen Result
propAddDefaultOrUpdateFieldAltersOriginal = do
  let expectedValue = J.fromString "it's here"
  let unexpectedValue = J.fromString "it shouldn't be here"
  updateKey ← genAsciiString
  input ← SM.insert updateKey unexpectedValue <$> genJObject
  pure $ testMigrationCodec { key: updateKey, expectedValue, input }
    $ JA.jobject <~< JAM.addDefaultOrUpdateField updateKey (maybe unexpectedValue (const expectedValue))

propRenameField ∷ Gen Result
propRenameField = do
  let expectedValue = J.fromString "it's here"
  oldKey ← genAsciiString
  newKey ← genAsciiString
  input ← SM.insert oldKey expectedValue <$> genJObject
  pure $ testMigrationCodec { key: newKey, expectedValue, input }
    $ JA.jobject <~< JAM.renameField oldKey newKey

propNestForTaggedMovesUnderValue ∷ Gen Result
propNestForTaggedMovesUnderValue = do
  values ← GenSM.genStrMap genAsciiString GenJ.genJson
  -- TODO: only-value
  let expectedValue = J.fromObject (SM.delete "tag" values)
  pure $ testMigrationCodec { key: "value", expectedValue, input: values }
    $ JA.jobject <~< JAM.nestForTagged

propNestForTaggedIdempotent ∷ Gen Result
propNestForTaggedIdempotent = do
  propCodec genTagged JAM.nestForTagged
  where
    genTagged = do
      tag ← genAsciiString
      expectedValue ← GenJ.genJson
      pure $ J.fromObject $
        SM.fromFoldable
          [ Tuple "tag" (J.fromString tag)
          , Tuple "value" expectedValue
          ]

testMigrationCodec
  ∷ { key ∷ String
    , expectedValue ∷ J.Json
    , input ∷ J.JObject
    }
  → JA.JsonCodec J.JObject
  → Result
testMigrationCodec { key, expectedValue, input } codec =
  case JA.decode codec (J.fromObject input) of
    Left err → Failed (JA.printJsonDecodeError err)
    Right obj →
      case SM.lookup key obj of
        Just value → value === expectedValue
        Nothing → Failed (JA.printJsonDecodeError (JA.AtKey key JA.MissingValue))
