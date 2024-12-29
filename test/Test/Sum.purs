module Test.Sum where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Data.Argonaut.Core (fromString, stringifyWithIndent)
import Data.Argonaut.Decode (parseJson)
import Data.Bifunctor (lmap)
import Data.Codec (decode, encode)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..))
import Data.Codec.Argonaut as C
import Data.Codec.Argonaut.Record as CR
import Data.Codec.Argonaut.Sum (Encoding(..), defaultEncoding, sumFlatWith, sumWith)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String as Str
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (error, throw)
import Test.QuickCheck (class Arbitrary, arbitrary, quickCheck)
import Test.QuickCheck.Arbitrary (genericArbitrary)
import Test.Util (propCodec)
import Type.Prelude (Proxy(..))

--------------------------------------------------------------------------------

data Sample
  = Foo
  | Bar Int
  | Baz Boolean String Int

derive instance Generic Sample _
derive instance Eq Sample

instance Arbitrary Sample where
  arbitrary = genericArbitrary

instance Show Sample where
  show = genericShow

codecSample ∷ Encoding → JsonCodec Sample
codecSample encoding = sumWith encoding "Sample"
  { "Foo": unit
  , "Bar": C.int
  , "Baz": C.boolean /\ C.string /\ C.int
  }

--------------------------------------------------------------------------------

data SampleFlat
  = FlatFoo
  | FlatBar { errors ∷ Int }
  | FlatBaz
      { active ∷ Boolean
      , name ∷ String
      , pos ∷ { x ∷ Int, y ∷ Int }
      }

derive instance Generic SampleFlat _
derive instance Eq SampleFlat

instance Arbitrary SampleFlat where
  arbitrary = genericArbitrary

instance Show SampleFlat where
  show = genericShow

codecSampleFlat ∷ JsonCodec SampleFlat
codecSampleFlat = sumFlatWith { tag: Proxy @"tag" } "Sample"
  { "FlatFoo": unit
  , "FlatBar": CR.record { errors: C.int }
  , "FlatBaz": CR.record
      { active: C.boolean
      , name: C.string
      , pos: CR.object "Pos"
          { x: C.int
          , y: C.int
          }
      }
  }

--------------------------------------------------------------------------------

check ∷ ∀ a. Show a ⇒ Eq a ⇒ JsonCodec a → a → String → Effect Unit
check codec val expectEncoded = do
  let encodedStr = stringifyWithIndent 2 $ encode codec val
  when (encodedStr /= expectEncoded) $
    throw ("check failed, expected: " <> expectEncoded <> ", got: " <> encodedStr)

  json ← liftEither $ lmap (show >>> error) $ parseJson encodedStr

  decoded ← liftEither $ lmap (show >>> error) $ decode codec json

  when (decoded /= val) $
    throw ("check failed, expected: " <> show val <> ", got: " <> show decoded)

checkError ∷ ∀ a. Show a ⇒ JsonCodec a → JsonDecodeError → String → Effect Unit
checkError codec expectError encodedStr = do

  json ← liftEither $ lmap (show >>> error) $ parseJson encodedStr

  decoded ← liftEither $ lmap (show >>> error) $ either Right Left $ decode codec json

  when (decoded /= expectError) $
    throw ("decode error check failed, expected: " <> show expectError <> ", got: " <> show decoded)

main ∷ Effect Unit
main = do
  log "Check sum"

  log "  - Default encoding"
  let
    noMathErr keys =
      Named "Sample" (TypeMismatch $ "No match for sum cases in nested keys: " <> keys)
  do
    checkError (codecSample defaultEncoding)
      (Named "Sample" (TypeMismatch "Object"))
      $ Str.joinWith "\n"
          [ "42"
          ]

    checkError (codecSample defaultEncoding)
      (Named "Sample" (AtKey "tag" (UnexpectedValue $ fromString "Zoo")))
      $ Str.joinWith "\n"
          [ "{"
          , "  \"tag\": \"Zoo\""
          , "}"
          ]

    checkError (codecSample defaultEncoding)
      (Named "Sample" (AtKey "tag" MissingValue))
      $ Str.joinWith "\n"
          [ "{"
          , "  \"type\": \"Boo\""
          , "}"
          ]

    -- Encode/Decode constructor without arguments
    check (codecSample defaultEncoding) Foo
      $ Str.joinWith "\n"
          [ "{"
          , "  \"tag\": \"Foo\","
          , "  \"values\": []"
          , "}"
          ]

    checkError (codecSample defaultEncoding)
      (Named "Sample" (Named "case Foo" (TypeMismatch "Expecting a value property `values`")))
      $ Str.joinWith "\n"
          [ "{"
          , "  \"tag\": \"Foo\""
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

    checkError (codecSample defaultEncoding)
      (Named "Sample" (Named "case Bar" (TypeMismatch "Expecting exactly one element")))
      $ Str.joinWith "\n"
          [ "{"
          , "  \"tag\": \"Bar\","
          , "  \"values\": ["
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

  log "  - EncodeTagged"
  do
    log "    - Custom tag and values keys"
    do
      let
        opts = EncodeTagged
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
        opts = EncodeTagged
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
        opts = EncodeTagged
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

      checkError
        (codecSample opts)
        (Named "Sample" (Named "case Bar" (TypeMismatch "Int")))
        $ Str.joinWith "\n"
            [ "{"
            , "  \"tag\": \"Bar\","
            , "  \"values\": \"42\""
            --, "  \"values\": 42"
            , "}"
            ]
      --pure unit

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

      checkError
        (codecSample opts)
        (Named "Sample" (Named "case Baz" (AtIndex 2 (TypeMismatch "Int"))))
        $ Str.joinWith "\n"
            [ "{"
            , "  \"tag\": \"Baz\","
            , "  \"values\": ["
            , "    true,"
            , "    \"hello\","
            , "    \"42\""
            , "  ]"
            , "}"
            ]

  log "  - EncodeNested"
  do
    log "    - default"
    do
      let
        opts = EncodeNested
          { unwrapSingleArguments: false
          }

      checkError
        (codecSample opts)
        (noMathErr "`Zoo`")
        $ Str.joinWith "\n"
            [ "{"
            , "  \"Zoo\": [42]"
            , "}"
            ]

      check
        (codecSample opts)
        Foo
        $ Str.joinWith "\n"
            [ "{"
            , "  \"Foo\": []"
            , "}"
            ]

      checkError
        (codecSample opts)
        (Named "Sample" (Named "case Foo" (TypeMismatch "Expecting an empty array")))
        $ Str.joinWith "\n"
            [ "{"
            , "  \"Foo\": [42]"
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

      checkError
        (codecSample opts)
        (Named "Sample" (Named "case Bar" (TypeMismatch "Int")))
        $ Str.joinWith "\n"
            [ "{"
            , "  \"Bar\": ["
            , "    \"42\""
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
      opts = EncodeNested
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

  quickCheck (propCodec arbitrary (codecSample defaultEncoding))

  log "Check sum flat"
  do
    checkError codecSampleFlat
      (Named "Sample" (AtKey "tag" MissingValue))
      $ Str.joinWith "\n"
          [ "{"
          , "  \"x\": \"FlatFoo\""
          , "}"
          ]

    checkError codecSampleFlat
      (Named "Sample" (AtKey "tag" (UnexpectedValue $ fromString "FlatZoo")))
      $ Str.joinWith "\n"
          [ "{"
          , "  \"tag\": \"FlatZoo\""
          , "}"
          ]

    check codecSampleFlat FlatFoo
      $ Str.joinWith "\n"
          [ "{"
          , "  \"tag\": \"FlatFoo\""
          , "}"
          ]

    check codecSampleFlat (FlatBar { errors: 42 })
      $ Str.joinWith "\n"
          [ "{"
          , "  \"tag\": \"FlatBar\","
          , "  \"errors\": 42"
          , "}"
          ]

    checkError codecSampleFlat
      (Named "Sample" (Named "case FlatBar" (AtKey "errors" (TypeMismatch "Int"))))
      $ Str.joinWith "\n"
          [ "{"
          , "  \"tag\": \"FlatBar\","
          , "  \"errors\": \"42\""
          , "}"
          ]

    check codecSampleFlat (FlatBaz { active: true, name: "hello", pos: { x: 42, y: 42 } })
      $ Str.joinWith "\n"
          [ "{"
          , "  \"tag\": \"FlatBaz\","
          , "  \"active\": true,"
          , "  \"name\": \"hello\","
          , "  \"pos\": {"
          , "    \"x\": 42,"
          , "    \"y\": 42"
          , "  }"
          , "}"
          ]

    checkError codecSampleFlat
      (Named "Sample" (Named "case FlatBaz" (AtKey "pos" (Named "Pos" (AtKey "y" MissingValue)))))
      $ Str.joinWith "\n"
          [ "{"
          , "  \"tag\": \"FlatBaz\","
          , "  \"active\": true,"
          , "  \"name\": \"hello\","
          , "  \"pos\": {"
          , "    \"x\": 42"
          , "  }"
          , "}"
          ]

    quickCheck (propCodec arbitrary codecSampleFlat)
