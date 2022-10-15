module Test.Example.Object3 where

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe)

type Person =
  { name ∷ String
  , age ∷ Int
  , active ∷ Boolean
  , email ∷ Maybe String
  }

codec ∷ CA.JsonCodec Person
codec =
  CA.object "Person"
    ( CAR.record
        { name: CA.string
        , age: CA.int
        , active: CA.boolean
        , email: CAR.optional CA.string
        }
    )
