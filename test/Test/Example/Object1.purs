module Test.Example.Object1 where

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR

type Person = { "Name" ∷ String, age ∷ Int, "is active" ∷ Boolean }

codec ∷ CA.JsonCodec Person
codec =
  CA.object "Person"
    (CAR.record
      { "Name": CA.string
      , age: CA.int
      , "is active": CA.boolean
      })
