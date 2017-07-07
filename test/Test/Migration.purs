module Test.Migration where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.Argonaut.Core as J
import Data.Codec ((<~<))
import Data.Codec.Argonaut.Common as JA
import Data.Codec.Argonaut.Migration as JAM
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Gen (genAsciiString)
import Data.StrMap as SM
import Test.QuickCheck (Result(..), QC, quickCheck, (===))
import Test.QuickCheck.Gen (Gen)
import Test.Util (genJObject)

main :: QC () Unit
main = do
  log "Checking addDefaultField adds a field if it is missing"
  quickCheck propDefaultFieldAdded

  log "Checking addDefaultField preserves an existing value if it's already present"
  quickCheck propDefaultFieldPreservesOriginal

  log "Checking renameField renames a field"
  quickCheck propDefaultFieldPreservesOriginal

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

propRenameField ∷ Gen Result
propRenameField = do
  let expectedValue = J.fromString "it's here"
  oldKey ← genAsciiString
  newKey ← genAsciiString
  input ← SM.insert oldKey expectedValue <$> genJObject
  pure $ testMigrationCodec { key: newKey, expectedValue, input }
    $ JA.jobject <~< JAM.renameField oldKey newKey

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
