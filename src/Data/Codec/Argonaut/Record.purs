module Data.Codec.Argonaut.Record
  ( class RowListCodec
  , rowListCodec
  , record
  ) where

import Data.Codec.Argonaut as CA
import Data.Record as Rec
import Data.Symbol (class IsSymbol, SProxy(..))
import Type.Equality as TE
import Type.Row as R
import Unsafe.Coerce (unsafeCoerce)

class RowListCodec (rl ∷ R.RowList) (ri ∷ # Type) (ro ∷ # Type) | rl → ri ro where
  rowListCodec ∷ R.RLProxy rl → Record ri → CA.JPropCodec (Record ro)

instance rowListCodecNil ∷ RowListCodec R.Nil () () where
  rowListCodec _ _ = CA.record

instance rowListCodecCons ∷
  ( RowListCodec rs ri' ro'
  , RowCons sym (CA.JsonCodec a) ri' ri
  , RowCons sym a ro' ro
  , IsSymbol sym
  , TE.TypeEquals co (CA.JsonCodec a)
  ) ⇒ RowListCodec (R.Cons sym co rs) ri ro where
  rowListCodec _ codecs =
    CA.recordProp (SProxy ∷ SProxy sym) codec tail
    where
    codec ∷ CA.JsonCodec a
    codec = TE.from (Rec.get (SProxy ∷ SProxy sym) codecs)

    tail ∷ CA.JPropCodec (Record ro')
    tail = rowListCodec (R.RLProxy ∷ R.RLProxy rs) ((unsafeCoerce ∷ Record ri → Record ri') codecs)

-- | Constructs a record codec from a record of codecs.
record
  ∷ ∀ ri ro rl
  . R.RowToList ri rl
  ⇒ RowListCodec rl ri ro
  ⇒ Record ri
  → CA.JPropCodec (Record ro)
record = rowListCodec (R.RLProxy ∷ R.RLProxy rl)
