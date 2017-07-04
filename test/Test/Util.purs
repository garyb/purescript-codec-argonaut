module Test.Util where

import Prelude

import Control.Monad.Gen as Gen
import Data.Codec.Argonaut.Common as JA
import Data.Either (Either(..))
import Test.QuickCheck (Result(..), (<?>))
import Test.QuickCheck.Gen (Gen)

propCodec' ∷ ∀ a. (a → a → Boolean) → (a → String) → Gen a → JA.JsonCodec a → Gen Result
propCodec' eq' show' gen codec = do
  x ← gen
  pure case JA.decode codec (JA.encode codec x) of
    Left err →
      Failed
        $ "Decoding " <> show' x <> " failed with error: " <> JA.printJsonDecodeError err
    Right y →
      x `eq'` y <?> "Decoded result:\n" <> show' x <> "\n\nDid not match input:\n" <> show' y

propCodec ∷ ∀ a. Eq a ⇒ Show a ⇒ Gen a → JA.JsonCodec a → Gen Result
propCodec = propCodec' eq show

genInt ∷ Gen Int
genInt = Gen.chooseInt (-100000) 100000
