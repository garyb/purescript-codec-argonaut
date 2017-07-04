module Test.Prim where

import Prelude

import Control.Monad.Eff.Console (log)
import Control.Monad.Gen as Gen
import Data.Argonaut.Core as J
import Data.Argonaut.Gen (genJson)
import Data.Char.Gen (genAsciiChar)
import Data.Codec.Argonaut.Common ((~))
import Data.Codec.Argonaut.Common as JA
import Data.String.Gen (genAsciiString)
import Data.StrMap.Gen (genStrMap)
import Data.Symbol (SProxy(..))
import Test.QuickCheck (QC, Result, quickCheck)
import Test.QuickCheck.Gen (Gen)
import Test.Util (propCodec, propCodec', genInt)

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

  log "Checking object codec"
  quickCheck (propTestRecord codecObject)

  log "Checking record codec"
  quickCheck (propTestRecord codecRecord)

propNull ∷ Gen Result
propNull = propCodec (pure J.jNull) JA.null

propBoolean ∷ Gen Result
propBoolean = propCodec Gen.chooseBool JA.boolean

propNumber ∷ Gen Result
propNumber = propCodec (Gen.chooseFloat (-100000.0) 100000.0) JA.number

propInt ∷ Gen Result
propInt = propCodec genInt JA.int

propString ∷ Gen Result
propString = propCodec genAsciiString JA.string

propChar ∷ Gen Result
propChar = propCodec genAsciiChar JA.char

propJArray ∷ Gen Result
propJArray = propCodec (Gen.unfoldable genJson) JA.jarray

propJObject ∷ Gen Result
propJObject = propCodec (genStrMap genAsciiString genJson) JA.jobject

type TestRecord = { tag ∷ String, x ∷ Int, y ∷ Boolean }

genRecord ∷ Gen TestRecord
genRecord =
  { tag: _, x: _, y: _ }
    <$> genAsciiString
    <*> genInt
    <*> Gen.chooseBool

codecObject ∷ JA.JsonCodec TestRecord
codecObject =
  JA.object "Test Object" $
    { tag: _, x: _, y: _ }
      <$> _.tag ~ JA.prop "tag" JA.string
      <*> _.x ~ JA.prop "x" JA.int
      <*> _.y ~ JA.prop "y" JA.boolean

codecRecord ∷ JA.JsonCodec TestRecord
codecRecord =
  JA.object "Test Record" $ JA.record
    # JA.recordProp (SProxy ∷ SProxy "tag") JA.string
    # JA.recordProp (SProxy ∷ SProxy "x") JA.int
    # JA.recordProp (SProxy ∷ SProxy "y") JA.boolean

propTestRecord ∷ JA.JsonCodec TestRecord → Gen Result
propTestRecord = propCodec' checkEq print genRecord
  where
  checkEq r1 r2 = r1.tag == r2.tag && r1.x == r2.x && r1.y == r2.y
  print { tag, x, y } =
    "{ tag: " <> show tag <> ", x: " <> show x <> ", y: " <> show y <> " }"
