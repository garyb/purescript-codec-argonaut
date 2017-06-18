module Test.Util where

import Prelude

import Control.Monad.Codec.Argonaut.Common as JA
import Control.Monad.Gen as Gen
import Data.Either (Either(..))
import Test.QuickCheck (Result, (===))
import Test.QuickCheck.Gen (Gen)

propCodec ∷ ∀ a. Eq a ⇒ Show a ⇒ Gen a → JA.JsonCodec a → Gen Result
propCodec gen codec = do
  x ← gen
  pure $ Right x === JA.decode codec (JA.encode codec x)

genInt ∷ Gen Int
genInt = Gen.chooseInt (-100000) 100000
