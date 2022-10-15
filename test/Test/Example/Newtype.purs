module Test.Example.Newtype where

import Data.Codec.Argonaut.Common as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)

type PersonRec = { "Name" ∷ String, age ∷ Int, "is active" ∷ Boolean }

newtype Person = Person PersonRec

derive instance newtypePerson ∷ Newtype Person _

codec ∷ CA.JsonCodec Person
codec =
  wrapIso Person
    (CAR.object "Person"
      { "Name": CA.string
      , age: CA.int
      , "is active": CA.boolean
      })
