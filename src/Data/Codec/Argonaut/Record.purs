module Data.Codec.Argonaut.Record where

import Prelude

import Data.Codec.Argonaut as CA
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as R
import Prim.RowList as RL
import Record as Rec
import Type.Data.RowList (RLProxy(..))
import Type.Equality as TE
import Unsafe.Coerce (unsafeCoerce)

-- | Constructs a `JsonCodec` for a `Record` from a name and a record of codecs.
-- | The name is used in the error message produced when decoding fails.
-- |
-- | ```purescript
-- | type Person = { name ∷ String, age ∷ Int }
-- |
-- | personCodec ∷ CA.JsonCodec Person
-- | personCodec = CAR.object "Person" { name: CA.string, age: CA.int }
-- | ```
object
  ∷ ∀ m ri ro rl
  . Monad m
  ⇒ RL.RowToList ri rl
  ⇒ RowListCodec rl ri ro m
  ⇒ String
  → Record ri
  → CA.JsonCodecT m (Record ro)
object name rec = CA.object name (record rec)

-- | Constructs a `JPropCodec` for a `Record` from a record of codecs. Commonly
-- | the `object` function in this module will be the preferred choice, as that
-- | produces a `JsonCodec` instead.
record
  ∷ ∀ m ri ro rl
  . Monad m
  ⇒ RL.RowToList ri rl
  ⇒ RowListCodec rl ri ro m
  ⇒ Record ri
  → CA.JPropCodecT m (Record ro)
record = rowListCodec (RLProxy ∷ RLProxy rl)

-- | The class used to enable the building of `Record` codecs by providing a
-- | record of codecs.
class RowListCodec (rl ∷ RL.RowList) (ri ∷ # Type) (ro ∷ # Type) m | rl → ri ro where
  rowListCodec ∷ RLProxy rl → Record ri → CA.JPropCodecT m (Record ro)

instance rowListCodecNil ∷ Monad m ⇒ RowListCodec RL.Nil () () m where
  rowListCodec _ _ = CA.record

instance rowListCodecCons ∷
  ( Monad m 
  , RowListCodec rs ri' ro' m
  , R.Cons sym (CA.JsonCodecT m a) ri' ri
  , R.Cons sym a ro' ro
  , IsSymbol sym
  , TE.TypeEquals co (CA.JsonCodec a)
  ) ⇒ RowListCodec (RL.Cons sym co rs) ri ro m where
  rowListCodec _ codecs =
    CA.recordProp (SProxy ∷ SProxy sym) codec tail
    where
    codec ∷ CA.JsonCodecT m a
    codec = TE.from (Rec.get (SProxy ∷ SProxy sym) codecs)

    tail ∷ CA.JPropCodecT m (Record ro')
    tail = rowListCodec (RLProxy ∷ RLProxy rs) ((unsafeCoerce ∷ Record ri → Record ri') codecs)
