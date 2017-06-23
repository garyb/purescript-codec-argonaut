module Test.Compat where

import Prelude

import Control.Monad.Eff.Console (log)
import Control.Monad.Gen.Common as GenC
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Codec.Argonaut.Compat as JA
import Data.Either (Either(..))
import Data.String.Gen (genAsciiString)
import Data.StrMap.Gen (genStrMap)
import Test.QuickCheck (QC, Result, quickCheck, (===))
import Test.QuickCheck.Gen (Gen)
import Test.Util (genInt, propCodec)

main ∷ QC () Unit
main = do
  log "Checking Maybe codec"
  quickCheck propMaybeCodec

  log "Checking Maybe codec compatibility (encoding)"
  quickCheck propMaybeCodecEncodeCompat

  log "Checking Maybe codec compatibility (decoding)"
  quickCheck propMaybeCodecDecodeCompat

  log "Checking StrMap codec"
  quickCheck propStrMapCodec

  log "Checking StrMap codec compatibility (encoding)"
  quickCheck propStrMapCodecEncodeCompat

  log "Checking StrMap codec compatibility (decoding)"
  quickCheck propStrMapCodecDecodeCompat

propMaybeCodec ∷ Gen Result
propMaybeCodec =
  propCodec
    (GenC.genMaybe genInt)
    (JA.maybe JA.int)

propMaybeCodecEncodeCompat ∷ Gen Result
propMaybeCodecEncodeCompat =
  propCodecEncodeCompat
    (GenC.genMaybe genInt)
    (JA.maybe JA.int)

propMaybeCodecDecodeCompat ∷ Gen Result
propMaybeCodecDecodeCompat =
  propCodecDecodeCompat
    (GenC.genMaybe genInt)
    (JA.maybe JA.int)

propStrMapCodec ∷ Gen Result
propStrMapCodec =
  propCodec
    (genStrMap genAsciiString genInt)
    (JA.strMap JA.int)

propStrMapCodecEncodeCompat ∷ Gen Result
propStrMapCodecEncodeCompat =
  propCodecEncodeCompat
    (genStrMap genAsciiString genInt)
    (JA.strMap JA.int)

propStrMapCodecDecodeCompat ∷ Gen Result
propStrMapCodecDecodeCompat =
  propCodecDecodeCompat
    (genStrMap genAsciiString genInt)
    (JA.strMap JA.int)

propCodecEncodeCompat ∷ ∀ a. Eq a ⇒ Show a ⇒ DecodeJson a ⇒ Gen a → JA.JsonCodec a → Gen Result
propCodecEncodeCompat gen codec = do
  x ← gen
  pure $ Right x === decodeJson (JA.encode codec x)

propCodecDecodeCompat ∷ ∀ a. Eq a ⇒ Show a ⇒ EncodeJson a ⇒ Gen a → JA.JsonCodec a → Gen Result
propCodecDecodeCompat gen codec = do
  x ← gen
  pure $ Right x === JA.decode codec (encodeJson x)
