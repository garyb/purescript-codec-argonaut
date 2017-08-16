module Test.Record where

import Prelude

import Control.Monad.Eff.Console (log)
import Control.Monad.Gen as Gen
import Control.Monad.Gen.Common as GenC
import Data.Codec.Argonaut.Common as JA
import Data.Codec.Argonaut.Record as JAR
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (dimap)
import Data.String.Gen (genAsciiString)
import Test.QuickCheck (QC, quickCheck)
import Test.QuickCheck.Gen (Gen)
import Test.Util (genInt, propCodec)

type OuterR =
  { a ∷ Int
  , b ∷ String
  , c ∷ Maybe InnerR
  }

type InnerR =
  { n ∷ Int
  , m ∷ Boolean
  }

newtype Outer = Outer OuterR

derive instance newtypeOuter ∷ Newtype Outer _

instance showOuter ∷ Show Outer where
  show (Outer r) = "Outer " <> show (JA.encode outerCodec r)

instance eqOuter ∷ Eq Outer where
  eq (Outer o1) (Outer o2) =
    o1.a == o2.a
    && o1.b == o2.b
    && case o1.c, o2.c of
        Nothing, Nothing → true
        Just i1, Just i2 → i1.n == i2.n && i1.m == i2.m
        _, _ → false

outerCodec ∷ JA.JsonCodec OuterR
outerCodec =
  JA.object "Outer" $ JAR.record
    { a: JA.int
    , b: JA.string
    , c: JA.maybe innerCodec
    }

innerCodec ∷ JA.JsonCodec InnerR
innerCodec =
  JA.object "Inner" $ JAR.record
    { n: JA.int
    , m: JA.boolean
    }

genOuter ∷ Gen OuterR
genOuter = do
  a ← genInt
  b ← genAsciiString
  c ← GenC.genMaybe genInner
  pure { a, b, c }

genInner ∷ Gen InnerR
genInner = do
  n ← genInt
  m ← Gen.chooseBool
  pure { n, m }

main ∷ QC () Unit
main = do
  log "Checking record codec"
  quickCheck $ propCodec (Outer <$> genOuter) (dimap unwrap wrap outerCodec)
