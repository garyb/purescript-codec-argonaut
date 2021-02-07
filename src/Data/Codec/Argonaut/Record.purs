module Data.Codec.Argonaut.Record where

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
  ∷ ∀ ri ro rl
  . RL.RowToList ri rl
  ⇒ RowListCodec rl ri ro
  ⇒ String
  → Record ri
  → CA.JsonCodec (Record ro)
object name rec = CA.object name (record rec)

-- | Constructs a `JPropCodec` for a `Record` from a record of codecs. Commonly
-- | the `object` function in this module will be the preferred choice, as that
-- | produces a `JsonCodec` instead.
record
  ∷ ∀ ri ro rl
  . RL.RowToList ri rl
  ⇒ RowListCodec rl ri ro
  ⇒ Record ri
  → CA.JPropCodec (Record ro)
record = rowListCodec (RLProxy ∷ RLProxy rl)

-- | The class used to enable the building of `Record` codecs by providing a
-- | record of codecs.
class RowListCodec (rl ∷ RL.RowList Type) (ri ∷ Row Type) (ro ∷ Row Type) | rl → ri ro where
  rowListCodec ∷ RLProxy rl → Record ri → CA.JPropCodec (Record ro)

instance rowListCodecNil ∷ RowListCodec RL.Nil () () where
  rowListCodec _ _ = CA.record

instance rowListCodecCons ∷
  ( RowListCodec rs ri' ro'
  , R.Cons sym (CA.JsonCodec a) ri' ri
  , R.Cons sym a ro' ro
  , IsSymbol sym
  , TE.TypeEquals co (CA.JsonCodec a)
  ) ⇒ RowListCodec (RL.Cons sym co rs) ri ro where
  rowListCodec _ codecs =
    CA.recordProp (SProxy ∷ SProxy sym) codec tail
    where
    codec ∷ CA.JsonCodec a
    codec = TE.from (Rec.get (SProxy ∷ SProxy sym) codecs)

    tail ∷ CA.JPropCodec (Record ro')
    tail = rowListCodec (RLProxy ∷ RLProxy rs) ((unsafeCoerce ∷ Record ri → Record ri') codecs)
