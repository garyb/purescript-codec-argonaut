module Test.Variant where

import Prelude

import Control.Monad.Eff.Console (log)
import Control.Monad.Gen.Common as GenC
import Data.Codec.Argonaut.Common as JA
import Data.Codec.Argonaut.Variant as JAV
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.String.Gen (genAsciiString)
import Data.Symbol (SProxy(..))
import Data.Variant as V
import Test.QuickCheck (QC, quickCheck)
import Test.Util (genInt, propCodec)

main :: QC () Unit
main = do
  log "Checking Maybe-variant codec"
  quickCheck $
    propCodec
      (GenC.genMaybe genAsciiString)
      (codecMaybe JA.string)

  log "Checking Either-variant codec"
  quickCheck $
    propCodec
      (GenC.genEither genAsciiString genInt)
      (codecEither JA.string JA.int)

codecMaybe ∷ ∀ a. JA.JsonCodec a → JA.JsonCodec (Maybe a)
codecMaybe codecA =
  dimap toVariant fromVariant
    (JAV.variant
      # JAV.variantCase _Just (Right codecA)
      # JAV.variantCase _Nothing (Left unit))
  where
  toVariant = case _ of
    Just a → V.inj _Just a
    Nothing → V.inj _Nothing unit
  fromVariant = V.case_
    # V.on _Just Just
    # V.on _Nothing (const Nothing)
  _Just = SProxy ∷ SProxy "just"
  _Nothing = SProxy ∷ SProxy "nothing"

codecEither ∷ ∀ a b. JA.JsonCodec a → JA.JsonCodec b → JA.JsonCodec (Either a b)
codecEither codecA codecB =
  dimap toVariant fromVariant
    (JAV.variant
      # JAV.variantCase _Left (Right codecA)
      # JAV.variantCase _Right (Right codecB))
  where
  toVariant = case _ of
    Left a → V.inj _Left a
    Right b → V.inj _Right b
  fromVariant = V.case_
    # V.on _Left Left
    # V.on _Right Right
  _Left = SProxy ∷ SProxy "left"
  _Right = SProxy ∷ SProxy "right"
