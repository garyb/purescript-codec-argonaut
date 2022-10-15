module Test.Example.Prismatic1 where

import Data.Codec.Argonaut as CA
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES

codec ∷ CA.JsonCodec NonEmptyString
codec = CA.prismaticCodec "NonEmptyString" NES.fromString NES.toString CA.string
