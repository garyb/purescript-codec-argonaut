module Test.Prim where

import Prelude

import Control.Monad.Codec.Argonaut.Common as JA
import Control.Monad.Eff.Console (log)
import Control.Monad.Gen as Gen
import Data.Argonaut.Core as J
import Data.Argonaut.Gen (genJson)
import Data.Char.Gen (genAsciiChar)
import Data.String.Gen (genAsciiString)
import Data.StrMap.Gen (genStrMap)
import Test.QuickCheck (QC, Result, quickCheck)
import Test.QuickCheck.Gen (Gen)
import Test.Util (propCodec)

main :: QC () Unit
main = do
  log "Checking JNull codec"
  quickCheck propNull

  log "Checking Boolean codec"
  quickCheck propBoolean

  log "Checking Number codec"
  quickCheck propNumber

  log "Checking Int codec"
  quickCheck propInt

  log "Checking String codec"
  quickCheck propString

  log "Checking Char codec"
  quickCheck propChar

  log "Checking JArray codec"
  quickCheck propJArray

  log "Checking JObject codec"
  quickCheck propJObject

propNull ∷ Gen Result
propNull = propCodec (pure J.jNull) JA.null

propBoolean ∷ Gen Result
propBoolean = propCodec Gen.chooseBool JA.boolean

propNumber ∷ Gen Result
propNumber = propCodec (Gen.chooseFloat (-100000.0) 100000.0) JA.number

propInt ∷ Gen Result
propInt = propCodec (Gen.chooseInt (-100000) 100000) JA.int

propString ∷ Gen Result
propString = propCodec genAsciiString JA.string

propChar ∷ Gen Result
propChar = propCodec genAsciiChar JA.char

propJArray ∷ Gen Result
propJArray = propCodec (Gen.unfoldable genJson) JA.jarray

propJObject ∷ Gen Result
propJObject = propCodec (genStrMap genAsciiString genJson) JA.jobject
