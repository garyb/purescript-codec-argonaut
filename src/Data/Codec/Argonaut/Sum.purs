module Data.Codec.Argonaut.Sum
  ( Tag(..)
  , taggedSum
  ) where

import Prelude

import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Writer (Writer, writer)
import Data.Argonaut.Core as J
import Data.Codec (GCodec(..), decode, encode)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError, jobject, json, prop, string)
import Data.Either as E
import Data.Newtype (class Newtype)
import Data.Profunctor.Star (Star(..))
import Data.StrMap as SM
import Data.StrMap.ST as SMST
import Data.Tuple (Tuple(..))

-- | A tag value for a case in a sum type.
newtype Tag = Tag String

derive newtype instance eqTag ∷ Eq Tag
derive newtype instance ordTag ∷ Ord Tag
derive instance newtypeTag ∷ Newtype Tag _

-- | A helper for defining JSON codecs for sum types.
-- |
-- | - The first function attempts to decode a case, using the specified tag.
-- | - The second function encodes a case, returning an appropriate tag and
-- |   encoded value.
taggedSum
  ∷ ∀ a
  . (Tag → J.Json → E.Either JsonDecodeError a)
  → (a → Tuple Tag J.Json)
  → JsonCodec a
taggedSum f g = GCodec (decodeCase f) (encodeCase g)

decodeCase
  ∷ ∀ a
  . (Tag → J.Json → E.Either JsonDecodeError a)
  → ReaderT J.Json (E.Either JsonDecodeError) a
decodeCase f = ReaderT \j → do
  obj ← decode jobject j
  tag ← decode (prop "tag" string) obj
  value ← decode (prop "value" json) obj
  f (Tag tag) value

encodeCase
  ∷ ∀ a
  . (a → Tuple Tag J.Json)
  → Star (Writer J.Json) a a
encodeCase f = Star case _ of
  a | Tuple (Tag tag) value ← f a →
    writer $ Tuple a $ encode jobject $
      SM.pureST do
        obj ← SMST.new
        _ ← SMST.poke obj "tag" (encode string tag)
        SMST.poke obj "value" value
