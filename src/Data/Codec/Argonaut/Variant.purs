module Data.Codec.Argonaut.Variant where

import Prelude

import Control.Monad.Except (ExceptT, except)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Writer (Writer, writer)
import Data.Argonaut.Core as J
import Data.Codec (GCodec(..))
import Data.Codec as C
import Data.Codec.Argonaut (JsonCodecT, JsonDecodeError(..), encode, jobject, json, prop, string)
import Data.Either (Either(..))
import Data.Newtype (un)
import Data.Profunctor.Star (Star(..))
import Data.Symbol (class IsSymbol, reflectSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy, Variant, case_, inj, on)
import Foreign.Object as FO
import Foreign.Object.ST as FOST
import Prim.Row as R
import Prim.RowList as RL
import Record as Rec
import Type.Data.RowList (RLProxy(..))
import Type.Equality as TE
import Unsafe.Coerce (unsafeCoerce)

-- | Builds a codec for a variant from a record, similar to the way
-- | `Variant.match` works to pattern match on a variant.
-- |
-- | Commonly used to write decoders for sum-types, by providing a mapping from
-- | and to a Variant from that type and then using `dimap`.
-- |
-- | Each field in the record accepts an `Either`, where `Right` is used to
-- | specify a codec used for the constructor, and `Left` is used to specify a
-- | static value (generally as `Left unit` for nullary constructors).
-- |
-- | The variant will be encoded as a JSON object of the form
-- | `{ "tag": <name>, "value": <value> }`, where `<name>` is the name of the
-- | variant case, and `<value>` is the associated value (omitted in the case
-- | of static `Left`-defined values).
-- |
-- |```purescript
-- | codecMaybeMatch ∷ ∀ a. JA.JsonCodec a → JA.JsonCodec (Maybe a)
-- | codecMaybeMatch codecA =
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
  ∷ ∀ m rl ri ro
  . Monad m
  ⇒ RL.RowToList ri rl
  ⇒ VariantCodec rl ri ro m
  ⇒ Record ri
  → JsonCodecT m (Variant ro)
variantMatch = variantCodec (RLProxy ∷ RLProxy rl)

-- | Builds codecs for variants in combination with `variantCase`.
-- |
-- | Provides an alternative means of building variant codecs to that of
-- | `variantMatch`, often for cases where the codec is being constructed
-- | with a fold or some other similar technique.
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
variant ∷ ∀ m. Monad m ⇒ JsonCodecT m (Variant ())
variant = GCodec (ReaderT \j → except (Left (UnexpectedValue j))) (Star case_)

variantCase
  ∷ ∀ m l a r r'
  . Monad m
  ⇒ IsSymbol l
  ⇒ R.Cons l a r r'
  ⇒ SProxy l
  → Either a (JsonCodecT m a)
  → JsonCodecT m (Variant r)
  → JsonCodecT m (Variant r')
variantCase proxy eacodec (GCodec dec enc) = GCodec dec' enc'
  where

  dec' ∷ ReaderT J.Json (ExceptT JsonDecodeError m) (Variant r')
  dec' = ReaderT \j → do
    obj ← C.decode jobject j
    tag ← C.decode (prop "tag" string) obj
    if tag == reflectSymbol proxy
      then case eacodec of
        Left a → pure (inj proxy a)
        Right codec → do
          value ← C.decode (prop "value" json) obj
          inj proxy <$> C.decode codec value
      else coerceR <$> runReaderT dec j

  enc' ∷ Star (Writer J.Json) (Variant r') (Variant r')
  enc' = Star \v →
    on proxy
      (\v' → writer $ Tuple v $ encode jobject $
        FO.runST do
          obj ← FOST.new
          _ ← FOST.poke "tag" (encode string (reflectSymbol proxy)) obj
          case eacodec of
            Left _ → pure obj
            Right codec → FOST.poke "value" (C.encode codec v') obj)
      (\v' → un Star enc v' $> v) v

  coerceR ∷ Variant r → Variant r'
  coerceR = unsafeCoerce

-- | The class used to enable the building of `Variant` codecs from a record of
-- | codecs.
class VariantCodec (rl ∷ RL.RowList) (ri ∷ # Type) (ro ∷ # Type) m | rl → ri ro where
  variantCodec ∷ RLProxy rl → Record ri → JsonCodecT m (Variant ro)

instance variantCodecNil ∷ Monad m ⇒ VariantCodec RL.Nil () () m where
  variantCodec _ _ = variant

instance variantCodecCons ∷
  ( Monad m
  , VariantCodec rs ri' ro' m
  , R.Cons sym (Either a (JsonCodecT m a)) ri' ri
  , R.Cons sym a ro' ro
  , IsSymbol sym
  , TE.TypeEquals co (Either a (JsonCodecT m a))
  ) ⇒ VariantCodec (RL.Cons sym co rs) ri ro m where
  variantCodec _ codecs =
    variantCase (SProxy ∷ SProxy sym) codec tail
    where
    codec ∷ Either a (JsonCodecT m a)
    codec = TE.from (Rec.get (SProxy ∷ SProxy sym) codecs)

    tail ∷ JsonCodecT m (Variant ro')
    tail = variantCodec (RLProxy ∷ RLProxy rs) ((unsafeCoerce ∷ Record ri → Record ri') codecs)
