module Test.Main where

import Prelude

import Control.Monad.Eff.Console (log)
import Test.Common as Common
import Test.Compat as Compat
import Test.Prim as Prim
import Test.QuickCheck (QC)
import Test.Variant as Variant

main :: QC () Unit
main = do
  log "Checking Prim codecs"
  log "------------------------------------------------------------"
  Prim.main
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
