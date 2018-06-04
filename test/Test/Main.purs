module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Common as Common
import Test.Compat as Compat
import Test.Generic as Generic
import Test.Migration as Migration
import Test.Prim as TestPrim
import Test.Record as Record
import Test.Variant as Variant

main :: Effect Unit
main = do
  log "Checking Prim codecs"
  log "------------------------------------------------------------"
  TestPrim.main
  log ""
  log "Checking Common codecs"
  log "------------------------------------------------------------"
  Common.main
  log ""
  log "Checking Compat codecs"
  log "------------------------------------------------------------"
  Compat.main
  log ""
  log "Checking Variant codecs"
  log "------------------------------------------------------------"
  Variant.main
  log ""
  log "Checking Record codecs"
  log "------------------------------------------------------------"
  Record.main
  log ""
  log "Checking Migration codecs"
  log "------------------------------------------------------------"
  Migration.main
  log ""
  log "Checking Generic codecs"
  log "------------------------------------------------------------"
  Generic.main
