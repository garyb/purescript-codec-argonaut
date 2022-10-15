module Test.Example.Sum1 where

import Prelude

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Variant as CAV
import Data.Either (Either(..))
import Data.Variant as V

type SomeValue = V.Variant
  ( str ∷ String
  , int ∷ Int
  , neither ∷ Unit
  )

codec ∷ CA.JsonCodec SomeValue
codec = CAV.variantMatch
  { str: Right CA.string
  , int: Right CA.int
  , neither: Left unit
  }
