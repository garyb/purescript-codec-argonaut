module Test.Main where

import Prelude

import Control.Monad.Eff.Console (log)
import Test.Prim as Prim
import Test.Common as Common
import Test.Compat as Compat
import Test.QuickCheck (QC)

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
