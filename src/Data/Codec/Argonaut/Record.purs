module Data.Codec.Argonaut.Record
  ( class RowListCodec
  , rowListCodec
  , record
  ) where

import Data.Codec.Argonaut as CA
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as R
import Prim.RowList as RL
import Record as Rec
import Type.Data.RowList (RLProxy(..))
import Type.Equality as TE
import Unsafe.Coerce (unsafeCoerce)

class RowListCodec (rl ∷ RL.RowList) (ri ∷ # Type) (ro ∷ # Type) | rl → ri ro where
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

-- | Constructs a record codec from a record of codecs.
-- |
-- | ```purescript
-- | type Person = { name ∷ String, age ∷ Int }
-- |
-- | personCodec ∷ CA.JsonCodec Person
-- | personCodec = CA.object "Person" (record { name: CA.string, age: CA.int })
-- |
-- | decode personCodec "{ name: \"Carl\", age:\"25\" }" == Right { name: "Carl", age: 25 }
-- | ```

record
  ∷ ∀ ri ro rl
  . RL.RowToList ri rl
  ⇒ RowListCodec rl ri ro
  ⇒ Record ri
  → CA.JPropCodec (Record ro)
record = rowListCodec (RLProxy ∷ RLProxy rl)
