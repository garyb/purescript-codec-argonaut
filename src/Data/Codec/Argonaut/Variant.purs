module Data.Codec.Argonaut.Variant where

import Prelude

import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Writer (Writer, writer)
import Data.Argonaut.Core as J
import Data.Codec (GCodec(..))
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..), decode, encode, jobject, json, prop, string)
import Data.Either (Either(..))
import Data.Newtype (un)
import Data.Profunctor.Star (Star(..))
import Data.StrMap as SM
import Data.StrMap.ST as SMST
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy, Variant, case_, inj, on)
import Unsafe.Coerce (unsafeCoerce)

variant ∷ JsonCodec (Variant ())
variant = GCodec (ReaderT (Left <<< UnexpectedValue)) (Star case_)

variantCase
  ∷ ∀ l a r r'
  . IsSymbol l
  ⇒ RowCons l a r r'
  ⇒ SProxy l
  → Either a (JsonCodec a)
  → JsonCodec (Variant r)
  → JsonCodec (Variant r')
variantCase proxy eacodec (GCodec dec enc) = GCodec dec' enc'
  where

  dec' ∷ ReaderT J.Json (Either JsonDecodeError) (Variant r')
  dec' = ReaderT \j → do
    obj ← decode jobject j
    tag ← decode (prop "tag" string) obj
    if tag == reflectSymbol proxy
      then case eacodec of
        Left a → pure (inj proxy a)
        Right codec → do
          value ← decode (prop "value" json) obj
          inj proxy <$> decode codec value
      else coerceR <$> runReaderT dec j

  enc' ∷ Star (Writer J.Json) (Variant r') (Variant r')
  enc' = Star \v →
    on proxy
      (\v' → writer $ Tuple v $ encode jobject $
        SM.pureST do
          obj ← SMST.new
          _ ← SMST.poke obj "tag" (encode string (reflectSymbol proxy))
          case eacodec of
            Left _ → pure obj
            Right codec → SMST.poke obj "value" (encode codec v'))
      (\v' → un Star enc v' $> v) v

  coerceR ∷ Variant r → Variant r'
  coerceR = unsafeCoerce
