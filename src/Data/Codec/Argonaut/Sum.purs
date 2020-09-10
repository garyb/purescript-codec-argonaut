module Data.Codec.Argonaut.Sum
  ( enumSum
  , taggedSum
  ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT, except, withExceptT)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Writer (Writer, writer)
import Data.Argonaut.Core as J
import Data.Codec (GCodec(..), decode)
import Data.Codec.Argonaut (JsonCodecT, JsonDecodeError(..), encode, jobject, json, prop, string)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor.Star (Star(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Foreign.Object.ST as FOST

-- | A helper for defining JSON codecs for "enum" sum types, where every
-- | constructor is nullary, and the type will be encoded as a string.
enumSum
  ∷ ∀ m a
  . Monad m
  ⇒ (a → String)
  → (String → Maybe a)
  → JsonCodecT m a
enumSum printTag parseTag = GCodec dec enc
  where
  dec ∷ ReaderT J.Json (ExceptT JsonDecodeError m) a
  dec = ReaderT \j → do
    value ← decode string j
    except case parseTag value of
      Just a → Right a
      Nothing → Left (UnexpectedValue j)
  enc ∷ Star (Writer J.Json) a a
  enc = Star \a → writer $ Tuple a (encode string (printTag a))

-- | A helper for defining JSON codecs for sum types. To ensure exhaustivity
-- | there needs to be a mapping to and from a tag type for the type to be
-- | encoded.
-- |
-- | - The first argument is the name of the type being decoded, for error
-- |   message purposes.
-- | - The second argument maps a tag value to a string to use in the encoding.
-- | - The second argument maps a string back to a tag value during decoding.
-- | - The third argument returns either a constant value or a decoder function
-- |   based on a tag value.
-- | - The fourth argument returns a tag value and optional encoded value to
-- |   store for a constructor of the sum.
taggedSum
  ∷ ∀ m tag a
  . Monad m
  ⇒ String
  → (tag → String)
  → (String → Maybe tag)
  → (tag → Either a (J.Json → ExceptT JsonDecodeError m a))
  → (a → Tuple tag (Maybe J.Json))
  → JsonCodecT m a
taggedSum name printTag parseTag f g = GCodec decodeCase encodeCase
  where
  decodeCase ∷ ReaderT J.Json (ExceptT JsonDecodeError m) a
  decodeCase = ReaderT \j → withExceptT (Named name) do
    obj ← decode jobject j
    tag ← decode (prop "tag" string) obj
    case parseTag tag of
      Nothing → except (Left (AtKey "tag" (UnexpectedValue (J.fromString tag))))
      Just t →
        case f t of
          Left a → pure a
          Right decoder → do
            value ← decode (prop "value" json) obj
            withExceptT (AtKey "value") (decoder value)
  encodeCase ∷ Star (Writer J.Json) a a
  encodeCase = Star case _ of
    a | Tuple tag value ← g a →
      writer $ Tuple a $ encode jobject $
        FO.runST do
          obj ← FOST.new
          _ ← FOST.poke "tag" (encode string (printTag tag)) obj
          maybe (pure obj) (\v → FOST.poke "value" v obj) value
