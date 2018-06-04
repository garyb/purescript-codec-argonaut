module Test.Generic where

import Prelude

import Data.Codec.Argonaut.Generic (nullarySum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.Arbitrary (genericArbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.Util (propCodec)

data MySum = Ctor1 | Ctor2 | MoarCtors

derive instance eqMySum ∷ Eq MySum
derive instance genericMySum ∷ Generic MySum _

instance showMySum ∷ Show MySum where
  show = genericShow

genMySum ∷ Gen MySum
genMySum = genericArbitrary

main ∷ Effect Unit
main = do
  log "Check nullarySum"
  quickCheck (propCodec genMySum (nullarySum "MySum"))
