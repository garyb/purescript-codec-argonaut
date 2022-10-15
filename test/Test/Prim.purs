module Test.Prim where

import Prelude

import Control.Monad.Gen as Gen
import Control.Monad.Gen.Common as GenC
import Data.Argonaut.Core as J
import Data.Argonaut.Gen (genJson)
import Data.Char.Gen (genAsciiChar)
import Data.Codec.Argonaut.Common ((~))
import Data.Codec.Argonaut.Common as CA
import Data.Either (Either(..), either, note)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (dimap)
import Data.Show.Generic (genericShow)
import Data.String.Gen (genAsciiString)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as Object
import Foreign.Object.Gen (genForeignObject)
import Test.QuickCheck (Result(..), quickCheck)
import Test.QuickCheck.Gen (Gen)
import Test.Util (genInt, propCodec, propCodec', propCodec'')
import Type.Proxy (Proxy(..))

main ∷ Effect Unit
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

  log "Checking CArray codec"
  quickCheck propCArray

  log "Checking JObject codec"
  quickCheck propJObject

  log "Checking object codec"
  quickCheck (propTestRecord codecObject)

  log "Checking record codec"
  quickCheck (propTestRecord codecRecord)

  log "Checking record codec with optional field"
  quickCheck propTestRecordOptional

  log "Checking record codec with optional field does include the field"
  quickCheck propPresentOptionalField

  log "Checking record codec with optional field does omit the field entirely"
  quickCheck propMissingOptionalField

  log "Checking fixed-point codec"
  quickCheck propFix

propNull ∷ Gen Result
propNull = propCodec (pure unit) CA.null

propBoolean ∷ Gen Result
propBoolean = propCodec Gen.chooseBool CA.boolean

propNumber ∷ Gen Result
propNumber = propCodec (Gen.chooseFloat (-100000.0) 100000.0) CA.number

propInt ∷ Gen Result
propInt = propCodec genInt CA.int

propString ∷ Gen Result
propString = propCodec genAsciiString CA.string

propChar ∷ Gen Result
propChar = propCodec genAsciiChar CA.char

propCArray ∷ Gen Result
propCArray = propCodec'' (show <<< map J.stringify) (Gen.unfoldable genJson) CA.jarray

propJObject ∷ Gen Result
propJObject = propCodec'' (show <<< map J.stringify) (genForeignObject genAsciiString genJson) CA.jobject

type TestRecord = { tag ∷ String, x ∷ Int, y ∷ Boolean }

genRecord ∷ Gen TestRecord
genRecord =
  { tag: _, x: _, y: _ }
    <$> genAsciiString
    <*> genInt
    <*> Gen.chooseBool

codecObject ∷ CA.JsonCodec TestRecord
codecObject =
  CA.object "Test Object" $
    { tag: _, x: _, y: _ }
      <$> _.tag ~ CA.prop "tag" CA.string
      <*> _.x ~ CA.prop "x" CA.int
      <*> _.y ~ CA.prop "y" CA.boolean

codecRecord ∷ CA.JsonCodec TestRecord
codecRecord =
  CA.object "Test Record" $ CA.record
    # CA.recordProp (Proxy ∷ Proxy "tag") CA.string
    # CA.recordProp (Proxy ∷ Proxy "x") CA.int
    # CA.recordProp (Proxy ∷ Proxy "y") CA.boolean

propTestRecord ∷ CA.JsonCodec TestRecord → Gen Result
propTestRecord = propCodec' checkEq print genRecord
  where
  checkEq r1 r2 = r1.tag == r2.tag && r1.x == r2.x && r1.y == r2.y
  print { tag, x, y } =
    "{ tag: " <> show tag <> ", x: " <> show x <> ", y: " <> show y <> " }"

type TestRecordOptional = { tag ∷ String, x ∷ Maybe Int }

genRecordOptional ∷ Gen TestRecordOptional
genRecordOptional =
  { tag: _, x: _ }
    <$> genAsciiString
    <*> GenC.genMaybe genInt

codecRecordOptional ∷ CA.JsonCodec TestRecordOptional
codecRecordOptional =
  CA.object "Test record with optional field" $ CA.record
    # CA.recordProp (Proxy ∷ Proxy "tag") CA.string
    # CA.recordPropOptional (Proxy ∷ Proxy "x") CA.int

propTestRecordOptional ∷ Gen Result
propTestRecordOptional = propCodec' checkEq print genRecordOptional codecRecordOptional
  where
  checkEq r1 r2 = r1.tag == r2.tag && r1.x == r2.x
  print { tag, x } =
    case x of
      Just _ → "{ tag: " <> show tag <> ", x: " <> show x <> " }"
      Nothing → "{ tag: " <> show tag <> " }"

propPresentOptionalField ∷ Gen Result
propPresentOptionalField = do
  tag ← genAsciiString
  x ← genInt
  let value = { tag, x: Just x }
  let json = CA.encode codecRecordOptional value
  pure $ either Failed (pure Success) do
    obj ← note "Encoded JSON is not an object" $ J.toObject json
    prop ← note "Optional property unexpectedly missing in object" $ Object.lookup "x" obj
    n ← note "x value is not a plain number" $ J.toNumber prop
    if n == Int.toNumber x then pure unit
    else Left "x value is wrong"

propMissingOptionalField ∷ Gen Result
propMissingOptionalField = do
  tag ← genAsciiString
  let value = { tag, x: Nothing }
  let json = CA.encode codecRecordOptional value
  pure $ either Failed (pure Success) do
    obj ← note "Encoded JSON is not an object" $ J.toObject json
    maybe (Right Success) (\_ → Left "Optional property unexpectedly appeared in object") $ Object.lookup "x" obj

newtype FixTest = FixTest (Maybe FixTest)

derive instance newtypeFixTest ∷ Newtype FixTest _
derive instance genericFixTest ∷ Generic FixTest _
instance eqFixTest ∷ Eq FixTest where
  eq (FixTest x) (FixTest y) = x == y

instance showFixTest ∷ Show FixTest where
  show x = genericShow x

genFixTest ∷ Gen FixTest
genFixTest = Gen.sized \n →
  if n <= 1 then pure $ FixTest Nothing
  else FixTest <$> Gen.resize (_ - 1) (GenC.genMaybe genFixTest)

codecFixTest ∷ CA.JsonCodec FixTest
codecFixTest = CA.fix \codec →
  dimap unwrap wrap (CA.maybe codec)

propFix ∷ Gen Result
propFix = propCodec genFixTest codecFixTest
