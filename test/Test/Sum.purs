module Test.Sum where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Data.Argonaut.Core (stringifyWithIndent)
import Data.Argonaut.Decode (parseJson)
import Data.Bifunctor (lmap)
import Data.Codec (decode, encode)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as C
import Data.Codec.Argonaut.Sum (Encoding(..), defaultEncoding, sumWith)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String as Str
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (error, throw)
import Test.QuickCheck (quickCheck)
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

codecSample ∷ Encoding → JsonCodec Sample
codecSample encoding = sumWith encoding "Sample"
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

  log "  - Default encoding"
  do

    -- Encode/Decode constructor without arguments
    check (codecSample defaultEncoding) Foo
      $ Str.joinWith "\n"
          [ "{"
          , "  \"tag\": \"Foo\","
          , "  \"values\": []"
          , "}"
          ]

    -- Encode/Decode constructor with single argument
    check (codecSample defaultEncoding) (Bar 42)
      $ Str.joinWith "\n"
          [ "{"
          , "  \"tag\": \"Bar\","
          , "  \"values\": ["
          , "    42"
          , "  ]"
          , "}"
          ]

    -- Encode/Decode constructor with multiple arguments
    check (codecSample defaultEncoding) (Baz true "hello" 42)
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

  log "  - EncodeTagValue"
  do
    log "    - Custom tag and values keys"
    do
      let
        opts = EncodeTagValue
          { tagKey: "customTag"
          , valuesKey: "customValues"
          , omitEmptyArguments: false
          , unwrapSingleArguments: false
          }

      check
        (codecSample opts)
        Foo
        $ Str.joinWith "\n"
            [ "{"
            , "  \"customTag\": \"Foo\","
            , "  \"customValues\": []"
            , "}"
            ]

      check
        (codecSample opts)
        (Bar 42)
        $ Str.joinWith "\n"
            [ "{"
            , "  \"customTag\": \"Bar\","
            , "  \"customValues\": ["
            , "    42"
            , "  ]"
            , "}"
            ]

      check
        (codecSample opts)
        (Baz true "hello" 42)
        $ Str.joinWith "\n"
            [ "{"
            , "  \"customTag\": \"Baz\","
            , "  \"customValues\": ["
            , "    true,"
            , "    \"hello\","
            , "    42"
            , "  ]"
            , "}"
            ]

    log "    - Option: Omit empty arguments"
    do
      let
        opts = EncodeTagValue
          { tagKey: "tag"
          , valuesKey: "values"
          , omitEmptyArguments: true
          , unwrapSingleArguments: false
          }

      check
        (codecSample opts)
        Foo
        $ Str.joinWith "\n"
            [ "{"
            , "  \"tag\": \"Foo\""
            , "}"
            ]

      check
        (codecSample opts)
        (Bar 42)
        $ Str.joinWith "\n"
            [ "{"
            , "  \"tag\": \"Bar\","
            , "  \"values\": ["
            , "    42"
            , "  ]"
            , "}"
            ]

      check
        (codecSample opts)
        (Baz true "hello" 42)
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

    log "    - Option: Unwrap single arguments"
    do
      let
        opts = EncodeTagValue
          { tagKey: "tag"
          , valuesKey: "values"
          , omitEmptyArguments: false
          , unwrapSingleArguments: true
          }

      check
        (codecSample opts)
        Foo
        $ Str.joinWith "\n"
            [ "{"
            , "  \"tag\": \"Foo\","
            , "  \"values\": []"
            , "}"
            ]

      check
        (codecSample opts)
        (Bar 42)
        $ Str.joinWith "\n"
            [ "{"
            , "  \"tag\": \"Bar\","
            , "  \"values\": 42"
            , "}"
            ]

      check
        (codecSample opts)
        (Baz true "hello" 42)
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

  log "  - EncodeCtorAsTag"
  do
    log "    - default"
    do
      let
        opts = EncodeCtorAsTag
          { unwrapSingleArguments: false
          }

      check
        (codecSample opts)
        Foo
        $ Str.joinWith "\n"
            [ "{"
            , "  \"Foo\": []"
            , "}"
            ]

      check
        (codecSample opts)
        (Bar 42)
        $ Str.joinWith "\n"
            [ "{"
            , "  \"Bar\": ["
            , "    42"
            , "  ]"
            , "}"
            ]

      check
        (codecSample opts)
        (Baz true "hello" 42)
        $ Str.joinWith "\n"
            [ "{"
            , "  \"Baz\": ["
            , "    true,"
            , "    \"hello\","
            , "    42"
            , "  ]"
            , "}"
            ]

    log "    - Option: Unwrap single arguments"
    do
      let
        opts = EncodeCtorAsTag
          { unwrapSingleArguments: true
          }

      check
        (codecSample opts)
        Foo
        $ Str.joinWith "\n"
            [ "{"
            , "  \"Foo\": []"
            , "}"
            ]

      check
        (codecSample opts)
        (Bar 42)
        $ Str.joinWith "\n"
            [ "{"
            , "  \"Bar\": 42"
            , "}"
            ]

      check
        (codecSample opts)
        (Baz true "hello" 42)
        $ Str.joinWith "\n"
            [ "{"
            , "  \"Baz\": ["
            , "    true,"
            , "    \"hello\","
            , "    42"
            , "  ]"
            , "}"
            ]

  quickCheck (propCodec genMySum (codecSample defaultEncoding))

