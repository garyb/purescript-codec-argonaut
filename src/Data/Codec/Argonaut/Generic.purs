module Data.Codec.Argonaut.Generic where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core as J
import Data.Codec as C
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), from, to)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

nullarySum ∷ ∀ a r. Generic a r ⇒ NullarySumCodec r ⇒ String → CA.JsonCodec a
nullarySum name =
  C.basicCodec
    (map to <<< nullarySumDecode name)
    (nullarySumEncode <<< from)

class NullarySumCodec r where
  nullarySumEncode ∷ r → J.Json
  nullarySumDecode ∷ String → J.Json → Either CA.JsonDecodeError r

instance nullarySumCodecSum ∷ (NullarySumCodec a, NullarySumCodec b) ⇒ NullarySumCodec (Sum a b) where
  nullarySumEncode = case _ of
    Inl a → nullarySumEncode a
    Inr b → nullarySumEncode b
  nullarySumDecode name j
    = Inl <$> nullarySumDecode name j
    <|> Inr <$> nullarySumDecode name j

instance nullarySumCodecCtor ∷ IsSymbol name ⇒ NullarySumCodec (Constructor name NoArguments) where
  nullarySumEncode _ =
    J.fromString $ reflectSymbol (SProxy ∷ SProxy name)
  nullarySumDecode name j = do
    tag ← note (CA.Named name (CA.TypeMismatch "String")) (J.toString j)
    if tag /= reflectSymbol (SProxy ∷ SProxy name)
      then Left (CA.Named name (CA.UnexpectedValue j))
      else Right (Constructor NoArguments)
