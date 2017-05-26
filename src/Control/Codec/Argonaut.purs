module Control.Monad.Codec.Argonaut
  ( JsonParseCodec
  , JsonCodec
  , JObjectReader
  , JObjectBuilder
  , JObjectCodec
  , JArrayReader
  , JArrayBuilder
  , JArrayCodec
  , jsonParser
  , null
  , boolean
  , number
  , string
  , jarray
  , jobject
  , element
  , array
  , prop
  , object
  , module Exports
  ) where

import Prelude

import Control.Monad.Codec ((<~>)) as Exports
import Control.Monad.Codec (Codec', GCodec(..), GCodec', basicCodec, bihoistGCodec, decode, encode)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.State (StateT(..), evalStateT)
import Control.Monad.Writer (Writer, mapWriter, tell)
import Data.Argonaut.Core as J
import Data.Argonaut.Parser as JP
import Data.Array as A
import Data.Bifunctor as BF
import Data.Either (Either(..))
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))

type JsonParseCodec = Codec' (Either String) String J.Json

type JsonCodec a = Codec' (Either String) J.Json a

type JObjectReader = ReaderT J.JObject (Either String)
type JObjectBuilder = Writer (L.List J.JAssoc)
type JObjectCodec a = GCodec' JObjectReader JObjectBuilder a

type JArrayReader = StateT (Tuple Int J.JArray) (Either String)
type JArrayBuilder = Writer (L.List J.Json)
type JArrayCodec a = GCodec' JArrayReader JArrayBuilder a

jsonParser :: Codec' (Either String) String J.Json
jsonParser = basicCodec JP.jsonParser J.stringify

null :: JsonCodec J.JNull
null = jsonPrimCodec "Null" J.toNull J.fromNull

boolean :: JsonCodec Boolean
boolean = jsonPrimCodec "Boolean" J.toBoolean J.fromBoolean

number :: JsonCodec Number
number = jsonPrimCodec "Number" J.toNumber J.fromNumber

string :: JsonCodec String
string = jsonPrimCodec "String" J.toString J.fromString

jarray :: JsonCodec J.JArray
jarray = jsonPrimCodec "Array" J.toArray J.fromArray

jobject :: JsonCodec J.JObject
jobject = jsonPrimCodec "Object" J.toObject J.fromObject

element :: forall a. String -> JsonCodec a -> JArrayCodec a
element key codec = GCodec dec enc
  where
  dec :: JArrayReader a
  dec = StateT \(Tuple ix xs) ->
    case A.index xs ix of
      Just val -> flip Tuple (Tuple (ix + 1) xs) <$> decode codec val
      Nothing -> Left ("Expected an array element at index " <> show ix)
  enc :: a -> JArrayBuilder a
  enc val = do
    tell (pure (encode codec val))
    pure val

array :: forall a. String -> JArrayCodec a -> JsonCodec a
array name = bihoistGCodec dec enc
  where
  dec :: JArrayReader ~> ReaderT J.Json (Either String)
  dec r = ReaderT (evalStateT r <<< Tuple 0 <=< decode jarray)
  enc :: JArrayBuilder ~> Writer J.Json
  enc = mapWriter (BF.rmap (encode jarray <<< A.fromFoldable))

prop :: forall a. String -> JsonCodec a -> JObjectCodec a
prop key codec = GCodec dec enc
  where
  dec :: JObjectReader a
  dec = ReaderT \obj ->
    case SM.lookup key obj of
      Nothing -> Left ("Expected field '" <> key <> "'")
      Just val -> decode codec val
  enc :: a -> JObjectBuilder a
  enc val = do
    tell (pure (Tuple key (encode codec val)))
    pure val

object :: forall a. String -> JObjectCodec a -> JsonCodec a
object name = bihoistGCodec dec enc
  where
  dec :: JObjectReader ~> ReaderT J.Json (Either String)
  dec r = ReaderT (runReaderT r <=< decode jobject)
  enc :: JObjectBuilder ~> Writer J.Json
  enc = mapWriter (BF.rmap (encode jobject <<< SM.fromFoldable))

jsonPrimCodec
  :: forall a
   . String
  -> (J.Json -> Maybe a)
  -> (a -> J.Json)
  -> JsonCodec a
jsonPrimCodec ty f =
  basicCodec (maybe (Left ("Expected value of type '" <> ty <> "'")) Right <<< f)
