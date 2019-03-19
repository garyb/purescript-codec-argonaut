module Data.Codec.Argonaut.Variant where

import Prelude

import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Writer (Writer, writer)
import Data.Argonaut.Core as J
import Data.Codec (GCodec(..))
import Data.Codec.Argonaut (JPropCodec, JsonCodec, JsonDecodeError(..), decode, encode, jobject, prop, string)
import Data.Either (Either(..), either)
import Data.List ((:))
import Data.Newtype (un)
import Data.Profunctor.Star (Star(..))
import Data.Symbol (class IsSymbol, reflectSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy, Variant, case_, inj, on)
import Foreign.Object as FO
import Prim.Row as Row
import Record as Rec
import Type.Equality as TE
import Type.Row as R
import Unsafe.Coerce (unsafeCoerce)

-- | Constructs a variant codec from a record.
-- |
-- | Commonly used to write decoders for sum-types, by providing a mapping from
-- | and to a Variant from that type and then using `dimap`.
-- |
-- |```purescript
-- | codecMaybe ∷ ∀ a. JA.JsonCodec a → JA.JsonCodec (Maybe a)
-- | codecMaybe codecA =
-- |   dimap toVariant fromVariant
-- |     (JAV.variantMatch
-- |       { just: Right codecA
-- |       , nothing: Left unit
-- |       })
-- |   where
-- |   toVariant = case _ of
-- |     Just a → V.inj (SProxy ∷ _ "just") a
-- |     Nothing → V.inj (SProxy ∷ _ "nothing") unit
-- |   fromVariant = V.match
-- |     { just: Just
-- |     , nothing: \_ → Nothing
-- |     }
-- |```
variantMatch
  ∷ ∀ rl ri ro
  . R.RowToList ri rl
  ⇒ VariantCodec rl ri ro
  ⇒ Record ri
  → JsonCodec (Variant ro)
variantMatch = variantCodec (R.RLProxy ∷ R.RLProxy rl)

-- | Allows building codecs for variants in combination with `variantCase` or
-- | `variantCase'`.
-- |
-- | Usually the `variantMatch` version is preferrable, but this is required for
-- | situations where the codec is built dynamically, for example, or where the
-- | discrimnator for the cases is not a simple mapping from a "tag" property
-- | and `variantCase'` must be used.
variant ∷ JsonCodec (Variant ())
variant = GCodec (ReaderT (Left <<< UnexpectedValue)) (Star case_)

variantCase'
  ∷ ∀ v l a r r'
  . IsSymbol l
  ⇒ Row.Cons l a r r'
  ⇒ Eq v
  ⇒ { prop ∷ String, value ∷ v, codec ∷ JsonCodec v }
  → SProxy l
  → Either a (JPropCodec a)
  → JsonCodec (Variant r)
  → JsonCodec (Variant r')
variantCase' discrim proxy eacodec (GCodec dec enc) = GCodec dec' enc'
  where

  dec' ∷ ReaderT J.Json (Either JsonDecodeError) (Variant r')
  dec' = ReaderT \j → do
    obj ← decode jobject j
    tag ← decode (prop discrim.prop discrim.codec) obj
    if tag == discrim.value
      then case eacodec of
        Left a → pure (inj proxy a)
        Right codec → inj proxy <$> decode codec obj
      else coerceR <$> runReaderT dec j

  enc' ∷ Star (Writer J.Json) (Variant r') (Variant r')
  enc' = Star \v →
    on proxy
      (\v' → writer $ Tuple v $ encode jobject $
        let
          caseEntry = Tuple discrim.prop (encode discrim.codec discrim.value)
        in
          FO.fromFoldable $
            either
              (const (pure caseEntry))
              (\codec → caseEntry : encode codec v')
              eacodec)
      (\v' → un Star enc v' $> v) v

  coerceR ∷ Variant r → Variant r'
  coerceR = unsafeCoerce

variantCase
  ∷ ∀ l a r r'
  . IsSymbol l
  ⇒ Row.Cons l a r r'
  ⇒ SProxy l
  → Either a (JsonCodec a)
  → JsonCodec (Variant r)
  → JsonCodec (Variant r')
variantCase proxy eacodec =
  variantCase'
    { prop: "tag", value: reflectSymbol proxy, codec: string }
    proxy
    (map (prop "value") eacodec)

class VariantCodec (rl ∷ R.RowList) (ri ∷ # Type) (ro ∷ # Type) | rl → ri ro where
  variantCodec ∷ R.RLProxy rl → Record ri → JsonCodec (Variant ro)

instance variantCodecNil ∷ VariantCodec R.Nil () () where
  variantCodec _ _ = variant

instance variantCodecCons ∷
  ( VariantCodec rs ri' ro'
  , Row.Cons sym (Either a (JsonCodec a)) ri' ri
  , Row.Cons sym a ro' ro
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
