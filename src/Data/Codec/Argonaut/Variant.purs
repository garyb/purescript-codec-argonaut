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
import Data.Record as Rec
import Data.StrMap as SM
import Data.StrMap.ST as SMST
import Data.Symbol (class IsSymbol, reflectSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy, Variant, case_, inj, on)
import Type.Equality as TE
import Type.Row as R
import Unsafe.Coerce (unsafeCoerce)

-- | Allows building codecs for variants in combination with variantCase.
-- |
-- | Commonly used to write decoders for sum-types, by providing a mapping from
-- | and to a Variant from that type and then using `dimap`.
-- |
-- |```purescript
-- | codecMaybe ∷ ∀ a. JA.JsonCodec a → JA.JsonCodec (Maybe a)
-- | codecMaybe codecA =
-- |   dimap toVariant fromVariant
-- |     (JAV.variant
-- |       # JAV.variantCase _Just (Right codecA)
-- |       # JAV.variantCase _Nothing (Left unit))
-- |   where
-- |   toVariant = case _ of
-- |     Just a → V.inj _Just a
-- |     Nothing → V.inj _Nothing unit
-- |   fromVariant = V.case_
-- |     # V.on _Just Just
-- |     # V.on _Nothing (const Nothing)
-- |   _Just = SProxy ∷ SProxy "just"
-- |   _Nothing = SProxy ∷ SProxy "nothing"
-- |```
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

class VariantCodec (rl ∷ R.RowList) (ri ∷ # Type) (ro ∷ # Type) | rl → ri ro where
  variantCodec ∷ R.RLProxy rl → Record ri → JsonCodec (Variant ro)

instance variantCodecNil ∷ VariantCodec R.Nil () () where
  variantCodec _ _ = variant

instance variantCodecCons ∷
  ( VariantCodec rs ri' ro'
  , RowCons sym (Either a (JsonCodec a)) ri' ri
  , RowCons sym a ro' ro
  , IsSymbol sym
  , TE.TypeEquals co (Either a (JsonCodec a))
  ) ⇒ VariantCodec (R.Cons sym co rs) ri ro where
  variantCodec _ codecs =
    variantCase (SProxy ∷ SProxy sym) codec tail
    where
    codec ∷ Either a (JsonCodec a)
    codec = TE.from (Rec.get (SProxy ∷ SProxy sym) codecs)

    tail ∷ JsonCodec (Variant ro')
    tail = variantCodec (R.RLProxy ∷ R.RLProxy rs) ((unsafeCoerce ∷ Record ri → Record ri') codecs)

variantMatch
  ∷ ∀ rl ri ro
  . R.RowToList ri rl
  ⇒ VariantCodec rl ri ro
  ⇒ Record ri
  → JsonCodec (Variant ro)
variantMatch = variantCodec (R.RLProxy ∷ R.RLProxy rl)
