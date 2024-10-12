module Test.Sum where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Data.Argonaut.Core (Json, stringifyWithIndent)
import Data.Argonaut.Decode (parseJson)
import Data.Bifunctor (lmap)
import Data.Codec (Codec, codec, decode, encode, (>~>))
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..), codec, json, prismaticCodec)
import Data.Codec.Argonaut as C
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Sum (sum)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.Show.Generic (genericShow)
import Data.String as Str
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (error, throw)
import Test.QuickCheck (assertEquals, quickCheck)
import Test.QuickCheck.Arbitrary (genericArbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.Util (propCodec)

data Sample
  = Foo
  | Bar Int
  | Baz Boolean String Int

derive instance Generic Sample _
derive instance Eq Sample

genMySum ∷ Gen Sample
genMySum = genericArbitrary

instance Show Sample where
  show = genericShow

codecSample ∷ JsonCodec Sample
codecSample = sum "Sample"
  { "Foo": unit
  , "Bar": C.int
  , "Baz": C.boolean /\ C.string /\ C.int
  }

check ∷ ∀ a. Show a ⇒ Eq a ⇒ JsonCodec a → a → String → Effect Unit
check codec val expectEncoded = do
  let encodedStr = stringifyWithIndent 2 $ encode codec val
  when (encodedStr /= expectEncoded) $
    throw ("check failed, expected: " <> expectEncoded <> ", got: " <> encodedStr)

  json ← liftEither $ lmap (show >>> error) $ parseJson encodedStr

  decoded ← liftEither $ lmap (show >>> error) $ decode codec json

  when (decoded /= val) $
    throw ("check failed, expected: " <> show val <> ", got: " <> show decoded)

main ∷ Effect Unit
main = do
  log "Check sum"

  check codecSample Foo $ Str.joinWith "\n"
    [ "{"
    , "  \"tag\": \"Foo\","
    , "  \"values\": []"
    , "}"
    ]

  check codecSample (Bar 42) $ Str.joinWith "\n"
    [ "{"
    , "  \"tag\": \"Bar\","
    , "  \"values\": ["
    , "    42"
    , "  ]"
    , "}"
    ]

  check codecSample (Baz true "hello" 42)
    $ Str.joinWith "\n"
    [ "{"
    , "  \"tag\": \"Baz\","
    , "  \"values\": ["
    , "    true,"
    , "    \"hello\","
    , "    42"
    , "  ]"
    , "}"
    ]

  quickCheck (propCodec genMySum codecSample)

