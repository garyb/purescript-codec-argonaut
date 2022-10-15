module Test.Example.Object2 where

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR

type Person = { name ∷ String, age ∷ Int, active ∷ Boolean }

codec ∷ CA.JsonCodec Person
codec =
  CA.object "Person"
    (CAR.record
      { name: CA.string
      , age: CA.int
      , active: CA.boolean
      })
