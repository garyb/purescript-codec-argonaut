module Data.Codec.Argonaut.Generic where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core as J
import Data.Codec as C
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), from, to)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

-- | Encodes nullary sums with a Generic instance as strings that match the constructor names.
-- |
-- | ```purescript
-- | import Data.Argonaut as J
-- |
-- | data MySum = Ctor1 | Ctor2 | MoarCtors
-- | derive instance genericMySum ∷ Generic MySum _
-- |
-- | encode (nullarySum "MySum") Ctor1 == J.fromString "Ctor1"
-- | decode (nullarySum "MySum") (J.fromString "MoarCtors") == Right MoarCtors
-- |```
nullarySum ∷ ∀ a r. Generic a r ⇒ NullarySumCodec r ⇒ String → CA.JsonCodec a
nullarySum name = nullarySumWith defaultNullarySumEncoding name

type NullarySumEncoding =
  { mapTag ∷ String → String
  }

defaultNullarySumEncoding ∷ NullarySumEncoding
defaultNullarySumEncoding =
  { mapTag: identity
  }

-- | Like nullarySum, but allows customizing the encoding with options.
-- |
-- | ```purescript
-- | import Data.Argonaut as J
-- |
-- | data MySum = Ctor1 | Ctor2 | MoarCtors
-- | derive instance genericMySum ∷ Generic MySum _
-- |
-- | let opts = { mapTag: \tag → "My" <> tag }
-- |
-- | encode (nullarySumWith opts "MySum") Ctor1 == J.fromString "MyCtor1"
-- | decode (nullarySumWith opts "MySum") (J.fromString "MyMoarCtors") == Right MoarCtors
-- |```
nullarySumWith ∷ ∀ a r. Generic a r ⇒ NullarySumCodec r ⇒ NullarySumEncoding → String → CA.JsonCodec a
nullarySumWith encoding name =
  C.codec'
    (map to <<< nullarySumDecode encoding name)
    (nullarySumEncode encoding <<< from)

class NullarySumCodec r where
  nullarySumEncode ∷ NullarySumEncoding → r → J.Json
  nullarySumDecode ∷ NullarySumEncoding → String → J.Json → Either CA.JsonDecodeError r

instance nullarySumCodecSum ∷ (NullarySumCodec a, NullarySumCodec b) ⇒ NullarySumCodec (Sum a b) where
  nullarySumEncode encoding = case _ of
    Inl a → nullarySumEncode encoding a
    Inr b → nullarySumEncode encoding b
  nullarySumDecode encoding name j = Inl <$> nullarySumDecode encoding name j
    <|> Inr <$> nullarySumDecode encoding name j

instance nullarySumCodecCtor ∷ IsSymbol name ⇒ NullarySumCodec (Constructor name NoArguments) where
  nullarySumEncode encoding _ =
    let
      tagRaw = reflectSymbol (Proxy ∷ Proxy name)
      tag = encoding.mapTag tagRaw
    in
      J.fromString $ tag
  nullarySumDecode encoding name j = do
    tagRaw ← note (CA.Named name (CA.TypeMismatch "String")) (J.toString j)
    let tag = encoding.mapTag tagRaw
    if tag /= reflectSymbol (Proxy ∷ Proxy name) then
      Left (CA.Named name (CA.UnexpectedValue j))
    else
      Right (Constructor NoArguments)
