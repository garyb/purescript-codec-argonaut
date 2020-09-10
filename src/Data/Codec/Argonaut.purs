module Data.Codec.Argonaut
  ( JsonCodec
  , encode
  , decode
  , JsonCodecT
  , JsonDecodeError(..)
  , printJsonDecodeError
  , json
  , null
  , boolean
  , number
  , int
  , string
  , codePoint
  , char
  , jarray
  , jobject
  , void
  , array
  , JIndexedCodecT
  , JIndexedCodec
  , indexedArray
  , index
  , JPropCodecT
  , JPropCodec
  , object
  , prop
  , record
  , recordProp
  , recordPropOptional
  , fix
  , prismaticCodec
  , module Exports
  ) 
  where

import Prelude

import Control.Monad.Except (ExceptT, except, runExcept, withExceptT)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Writer (Writer, writer, mapWriter)
import Data.Argonaut.Core as J
import Data.Array as A
import Data.Bifunctor as BF
import Data.Codec (BasicCodec, Codec, GCodec(..), basicCodec, bihoistGCodec, encode, decode) as C
import Data.Codec ((~), (<~<), (>~>)) as Exports
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Int as I
import Data.List ((:))
import Data.List as L
import Data.Maybe (Maybe(..), fromJust)
import Data.Profunctor.Star (Star(..))
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Partial.Unsafe (unsafePartial)
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)

-- | Codec type for `Json` values with a custom transformer for the decoder.
type JsonCodecT m a = C.BasicCodec (ExceptT JsonDecodeError m) J.Json a

-- | Codec type for `Json` values.
type JsonCodec a = JsonCodecT Identity a

encode ∷ ∀ a. JsonCodec a → a → J.Json
encode codec j = C.encode codec j

decode ∷ ∀ a. JsonCodec a → J.Json → Either JsonDecodeError a
decode codec j = runExcept (C.decode codec j)

-- | Error type for failures while decoding.
data JsonDecodeError
  = TypeMismatch String
  | UnexpectedValue J.Json
  | AtIndex Int JsonDecodeError
  | AtKey String JsonDecodeError
  | Named String JsonDecodeError
  | MissingValue

derive instance eqJsonDecodeError ∷ Eq JsonDecodeError
derive instance ordJsonDecodeError ∷ Ord JsonDecodeError
derive instance genericJsonDecodeError ∷ Generic JsonDecodeError _

instance showJsonDecodeError ∷ Show JsonDecodeError where
  show = case _ of
    TypeMismatch s -> "(TypeMismatch " <> show s <> ")"
    UnexpectedValue j -> "(UnexpectedValue " <> J.stringify j <> ")"
    AtIndex i e -> "(AtIndex " <> show i <> " " <> show e <> ")"
    AtKey k e -> "(AtKey " <> show k <> " " <> show e <> ")"
    Named s e -> "(Named " <> show s <> " " <> show e <> ")"
    MissingValue -> "MissingValue"

-- | Prints a `JsonDecodeError` as a somewhat readable error message.
printJsonDecodeError ∷ JsonDecodeError → String
printJsonDecodeError err =
  "An error occurred while decoding a JSON value:\n" <> go err
  where
    go = case _ of
      TypeMismatch ty → "  Expected value of type '" <> ty <> "'."
      UnexpectedValue val → "  Unexpected value " <> J.stringify val <> "."
      AtIndex ix inner → "  At array index " <> show ix <> ":\n" <> go inner
      AtKey key inner → "  At object key " <> key <> ":\n" <> go inner
      Named name inner → "  Under '" <> name <> "':\n" <> go inner
      MissingValue → "  No value was found."

-- | The "identity codec" for `Json` values.
json ∷ ∀ m. Applicative m ⇒ JsonCodecT m J.Json
json = C.basicCodec (except <<< Right) identity

-- | A codec for `null` values in `Json`.
null ∷ ∀ m. Applicative m ⇒ JsonCodecT m Unit
null = jsonPrimCodec "Null" J.toNull (const J.jsonNull)

-- | A codec for `Boolean` values in `Json`.
boolean ∷ ∀ m. Applicative m ⇒ JsonCodecT m Boolean
boolean = jsonPrimCodec "Boolean" J.toBoolean J.fromBoolean

-- | A codec for `Number` values in `Json`.
number ∷ ∀ m. Applicative m ⇒ JsonCodecT m Number
number = jsonPrimCodec "Number" J.toNumber J.fromNumber

-- | A codec for `Int` values in `Json`.
int ∷ ∀ m. Applicative m ⇒ JsonCodecT m Int
int = jsonPrimCodec "Int" (I.fromNumber <=< J.toNumber) (J.fromNumber <<< I.toNumber)

-- | A codec for `String` values in `Json`.
string ∷ ∀ m. Applicative m ⇒ JsonCodecT m String
string = jsonPrimCodec "String" J.toString J.fromString

-- | A codec for `Codepoint` values in `Json`.
codePoint ∷ ∀ m. Applicative m ⇒ JsonCodecT m S.CodePoint
codePoint = jsonPrimCodec "CodePoint" (S.codePointAt 0 <=< J.toString) (J.fromString <<< S.singleton)

-- | A codec for `Char` values in `Json`.
char ∷ ∀ m. Applicative m ⇒ JsonCodecT m Char
char = jsonPrimCodec "Char" (SCU.toChar <=< J.toString) (J.fromString <<< SCU.singleton)

-- | A codec for `Void` values.
void ∷ ∀ m. Applicative m ⇒ JsonCodecT m Void
void = jsonPrimCodec "Void" (const Nothing) absurd

-- | A codec for `Array Json` values in `Json`. This does not decode the values
-- | of the array, for that use `array` for a general array decoder, or
-- | `indexedArray` with `index` to decode fixed length array encodings.
jarray ∷ ∀ m. Applicative m ⇒ JsonCodecT m (Array J.Json)
jarray = jsonPrimCodec "Array" J.toArray J.fromArray

-- | A codec for `JObject` values in `Json`.
jobject ∷ ∀ m. Applicative m ⇒ JsonCodecT m (FO.Object J.Json)
jobject = jsonPrimCodec "Object" J.toObject J.fromObject

-- | A codec for arbitrary length `Array`s where every item in the array
-- | shares the same type.
-- |
-- | ``` purescript
-- | import Data.Codec.Argonaut as CA
-- |
-- | codecIntArray ∷ CA.JsonCodec (Array Int)
-- | codecIntArray = CA.array CA.int
-- | ```
array ∷ ∀ m a. Monad m ⇒ JsonCodecT m a → JsonCodecT m (Array a)
array codec = C.GCodec dec enc
  where
  dec = ReaderT \j → 
    C.decode jarray j >>= traverseWithIndex \ix j' → 
      withExceptT (AtIndex ix) (C.decode codec j')
  enc = Star \xs → writer $ Tuple xs (J.fromArray (map (C.encode codec) xs))

-- | Codec type for specifically indexed `JArray` elements.
type JIndexedCodecT m a =
  C.Codec
    (ExceptT JsonDecodeError m)
    (Array J.Json)
    (L.List J.Json)
    a a
    
type JIndexedCodec a = JIndexedCodecT Identity a

-- | A codec for types that are encoded as an array with a specific layout.
-- |
-- | For example, if we'd like to encode a `Person` as a 2-element array, like
-- | `["Rashida", 37]`, we could write the following codec:
-- |
-- | ```purescript
-- | import Data.Codec ((~))
-- | import Data.Codec.Argonaut as CA
-- |
-- | type Person = { name ∷ String, age ∷ Int }
-- |
-- | codecPerson ∷ CA.JsonCodec Person
-- | codecPerson = CA.indexedArray "Test Object" $
-- |   { name: _, age: _ }
-- |     <$> _.name ~ CA.index 0 CA.string
-- |     <*> _.age ~ CA.index 1 CA.int
-- | ```
indexedArray ∷ ∀ m a. Monad m ⇒ String → JIndexedCodecT m a → JsonCodecT m a
indexedArray name =
  C.bihoistGCodec
    (\r → ReaderT (withExceptT (Named name) <<< runReaderT r <=< C.decode jarray))
    (mapWriter (BF.rmap (J.fromArray <<< A.fromFoldable)))

-- | A codec for an item in an `indexedArray`.
index ∷ ∀ m a. Applicative m ⇒ Int → JsonCodecT m a → JIndexedCodecT m a
index ix codec = C.GCodec dec enc
  where
  dec = ReaderT \xs →
    withExceptT (AtIndex ix) case A.index xs ix of
      Just val → C.decode codec val
      Nothing → except (Left MissingValue)
  enc = Star \val → writer $ Tuple val (pure (C.encode codec val))

-- | Codec type for `JObject` prop/value pairs.
type JPropCodecT m a =
  C.Codec
    (ExceptT JsonDecodeError m)
    (FO.Object J.Json)
    (L.List (Tuple String J.Json))
    a a
    
type JPropCodec a = JPropCodecT Identity a

-- | A codec for objects that are encoded with specific properties.
-- |
-- | See also `Data.Codec.Argonaut.Record.object` for a more commonly useful
-- | version of this function.
object ∷ ∀ m a. Monad m ⇒ String → JPropCodecT m a → JsonCodecT m a
object name =
  C.bihoistGCodec
    (\r → ReaderT (withExceptT (Named name) <<< runReaderT r <=< C.decode jobject))
    (mapWriter (BF.rmap (J.fromObject <<< FO.fromFoldable)))

-- | A codec for a property of an object.
prop ∷ ∀ m a. Applicative m ⇒ String → JsonCodecT m a → JPropCodecT m a
prop key codec = C.GCodec dec enc
  where
  dec = ReaderT \obj →
    withExceptT (AtKey key) case FO.lookup key obj of
      Just val → C.decode codec val
      Nothing → except (Left MissingValue)
  enc = Star \val → writer $ Tuple val (pure (Tuple key (C.encode codec val)))

-- | The starting value for a object-record codec. Used with `recordProp` it
-- | provides a convenient method for defining codecs for record types that
-- | encode into JSON objects of the same shape.
-- |
-- | For example, to encode a record as the JSON object
-- | `{ "name": "Karl", "age": 25 }` we would define a codec like this:
-- | ```
-- | import Data.Codec.Argonaut as CA
-- | import Data.Symbol (SProxy(..))
-- |
-- | type Person = { name ∷ String, age ∷ Int }
-- |
-- | codecPerson ∷ CA.JsonCodec Person
-- | codecPerson =
-- |   CA.object "Person" $ CA.record
-- |     # CA.recordProp (SProxy :: _ "name") CA.string
-- |     # CA.recordProp (SProxy :: _ "age") CA.int
-- | ```
-- |
-- | See also `Data.Codec.Argonaut.Record.object` for a more commonly useful
-- | version of this function.
record ∷ ∀ m. Monad m ⇒ JPropCodecT m {}
record = C.GCodec (pure {}) (Star \val → writer (Tuple val L.Nil))

-- | Used with `record` to define codecs for record types that encode into JSON
-- | objects of the same shape. See the comment on `record` for an example.
recordProp
  ∷ ∀ m p a r r'
  . Monad m
  ⇒ IsSymbol p
  ⇒ Row.Cons p a r r'
  ⇒ SProxy p
  → JsonCodecT m a
  → JPropCodecT m (Record r)
  → JPropCodecT m (Record r')
recordProp p codecA codecR =
  let key = reflectSymbol p in C.GCodec (dec' key) (enc' key)
  where
    dec' ∷ String → ReaderT (FO.Object J.Json) (ExceptT JsonDecodeError m) (Record r')
    dec' key = ReaderT \obj → do
      r ← C.decode codecR obj
      a ← withExceptT (AtKey key) case FO.lookup key obj of
        Just val → C.decode codecA val
        Nothing → except (Left MissingValue)
      pure $ unsafeSet key a r
    enc' ∷ String → Star (Writer (L.List (Tuple String J.Json))) (Record r') (Record r')
    enc' key = Star \val →
      writer $ Tuple val
        $ Tuple key (C.encode codecA (unsafeGet key val))
        : C.encode codecR (unsafeForget val)
    unsafeForget ∷ Record r' → Record r
    unsafeForget = unsafeCoerce
    unsafeSet ∷ String → a → Record r → Record r'
    unsafeSet key a = unsafeCoerce <<< FO.insert key a <<< unsafeCoerce
    unsafeGet ∷ String → Record r' → a
    unsafeGet s = unsafePartial fromJust <<< FO.lookup s <<< unsafeCoerce

-- | Used with `record` to define an optional field.
-- |
-- | This will only decode the property as `Nothing` if the field does not exist
-- | in the object - having a values such as `null` assigned will need handling 
-- | separately.
-- |
-- | The property will be omitted when encoding and the value is `Nothing`.
recordPropOptional
  ∷ ∀ m p a r r'
  . Monad m
  ⇒ IsSymbol p
  ⇒ Row.Cons p (Maybe a) r r'
  ⇒ SProxy p
  → JsonCodecT m a
  → JPropCodecT m (Record r)
  → JPropCodecT m (Record r')
recordPropOptional p codecA codecR =
  let key = reflectSymbol p in C.GCodec (dec' key) (enc' key)
  where
    dec' ∷ String → ReaderT (FO.Object J.Json) (ExceptT JsonDecodeError m) (Record r')
    dec' key = ReaderT \obj → do
      r ← C.decode codecR obj
      a ← withExceptT (AtKey key) case FO.lookup key obj of
        Just val → Just <$> C.decode codecA val
        _ → pure Nothing
      pure $ unsafeSet key a r
    enc' ∷ String → Star (Writer (L.List (Tuple String J.Json))) (Record r') (Record r')
    enc' key = Star \val → do
      let w = C.encode codecR (unsafeForget val)
      writer $ Tuple val case unsafeGet key val of
        Just a → Tuple key (C.encode codecA a) : w
        Nothing → w
    unsafeForget ∷ Record r' → Record r
    unsafeForget = unsafeCoerce
    unsafeSet ∷ String → Maybe a → Record r → Record r'
    unsafeSet key a = unsafeCoerce <<< FO.insert key a <<< unsafeCoerce
    unsafeGet ∷ String → Record r' → Maybe a
    unsafeGet s = unsafePartial fromJust <<< FO.lookup s <<< unsafeCoerce

jsonPrimCodec
  ∷ ∀ m a
  . Applicative m
  ⇒ String
  → (J.Json → Maybe a)
  → (a → J.Json)
  → JsonCodecT m a
jsonPrimCodec ty f =
  C.basicCodec \j → except case f j of
    Nothing → Left (TypeMismatch ty)
    Just val → Right val

-- | Helper function for defining recursive codecs in situations where the codec
-- | definition causes a _"The value of <codec> is undefined here"_ error.
-- |
-- | ```purescript
-- | import Data.Codec.Argonaut as CA
-- | import Data.Codec.Argonaut.Common as CAC
-- | import Data.Codec.Argonaut.Record as CAR
-- | import Data.Maybe (Maybe)
-- | import Data.Newtype (class Newtype)
-- | import Data.Profunctor (wrapIso)
-- |
-- | newtype IntList = IntList { cell ∷ Int, rest ∷ Maybe IntList }
-- |
-- | derive instance newtypeLoopyList ∷ Newtype IntList _
-- |
-- | codecIntList ∷ CA.JsonCodec IntList
-- | codecIntList =
-- |   CA.fix \codec →
-- |     wrapIso IntList $
-- |       CAR.object "IntList" { cell: CA.int, rest: CAC.maybe codec }
-- | ```
fix ∷ ∀ m a. (JsonCodecT m a → JsonCodecT m a) → JsonCodecT m a
fix f =
  C.basicCodec
    (\x → C.decode (f (fix f)) x)
    (\x → C.encode (f (fix f)) x)

-- | Adapts an existing codec with a pair of functions to allow a value to be
-- | further refined. If the inner decoder fails an `UnexpectedValue` error will
-- | be raised for JSON input.
-- |
-- | This function is named as such as the pair of functions it accepts
-- | correspond with the `preview` and `view` functions of a `Prism`-style lens.
-- |
-- | An example of this would be a codec for `Data.String.NonEmpty.NonEmptyString`:
-- |
-- | ```purescript
-- | nonEmptyString ∷ CA.JsonCodec NES.NonEmptyString
-- | nonEmptyString = CA.prismaticCodec NES.fromString NES.toString CA.string
-- | ```
-- |
-- | Another example might be to handle a mapping from a small sum type to
-- | strings:
-- |
-- | ```purescript
-- | data Direction = North | South | West | East
-- |
-- | directionCodec :: JsonCodec Direction
-- | directionCodec = prismaticCodec dec enc string
-- |   where
-- |     dec = case _ of
-- |       "N" -> Just North
-- |       "S" -> Just South
-- |       "W" -> Just West
-- |       "E" -> Just East
-- |       _ -> Nothing
-- |
-- |     enc = case _ of
-- |       North -> "N"
-- |       South -> "S"
-- |       West -> "W"
-- |       East -> "E"
-- | ```
-- |
-- | Although for this latter case there are some other options too, in the form
-- | of `Data.Codec.Argonaut.Generic.nullarySum` and `Data.Codec.Argonaut.Sum.enumSum`.
prismaticCodec ∷ ∀ m a b. Monad m ⇒ (a → Maybe b) → (b → a) → JsonCodecT m a → JsonCodecT m b
prismaticCodec f g orig =
  C.basicCodec
    (\json' → except <<< note (UnexpectedValue json') <<< f =<< C.decode orig json')
    (C.encode orig <<< g)
