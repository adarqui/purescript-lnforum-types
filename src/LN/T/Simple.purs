module LN.T.Simple where



import Data.Argonaut.Core               (jsonEmptyObject)
import Data.Argonaut.Decode             (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode             (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Argonaut.Printer            (printJson)
import Data.Date.Helpers                (Date)
import Data.Either                      (Either(..))
import Data.Foreign                     (ForeignError(..), fail)
import Data.Foreign.NullOrUndefined     (unNullOrUndefined)
import Data.Foreign.Class               (class Decode, read, readProp)
import Data.Maybe                       (Maybe(..))
import Data.Tuple                       (Tuple(..))
import Purescript.Api.Helpers           (class QueryParam, qp)
import Network.HTTP.Affjax.Request      (class Requestable, toRequest)
import Network.HTTP.Affjax.Response     (class Respondable, ResponseType(..))
import Optic.Core                       ((^.), (..))
import Optic.Types                      (Lens, Lens')
import Prelude                          (class Show, show, class Eq, eq, pure, bind, ($), (<>), (<$>), (<*>), (==), (&&))
import Data.Default

import Purescript.Api.Helpers

newtype SimpleIntRequest = SimpleIntRequest {
  simpleIntRequest :: Int
}


type SimpleIntRequestR = {
  simpleIntRequest :: Int
}


_SimpleIntRequest :: Lens' SimpleIntRequest {
  simpleIntRequest :: Int
}
_SimpleIntRequest f (SimpleIntRequest o) = SimpleIntRequest <$> f o


mkSimpleIntRequest :: Int -> SimpleIntRequest
mkSimpleIntRequest simpleIntRequest =
  SimpleIntRequest{simpleIntRequest}


unwrapSimpleIntRequest :: SimpleIntRequest -> {
  simpleIntRequest :: Int
}
unwrapSimpleIntRequest (SimpleIntRequest r) = r

instance simpleIntRequestEncodeJson :: EncodeJson SimpleIntRequest where
  encodeJson (SimpleIntRequest o) =
       "tag" := "SimpleIntRequest"
    ~> "simple_int_request" := o.simpleIntRequest
    ~> jsonEmptyObject


instance simpleIntRequestDecodeJson :: DecodeJson SimpleIntRequest where
  decodeJson o = do
    obj <- decodeJson o
    simpleIntRequest <- obj .? "simple_int_request"
    pure $ SimpleIntRequest {
      simpleIntRequest
    }


instance simpleIntRequestRequestable :: Requestable SimpleIntRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance simpleIntRequestRespondable :: Respondable SimpleIntRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkSimpleIntRequest
      <$> readProp "simple_int_request" json


instance simpleIntRequestDecode :: Decode SimpleIntRequest where
  read json =
      mkSimpleIntRequest
      <$> readProp "simple_int_request" json


newtype SimpleIntResponse = SimpleIntResponse {
  simpleIntResponse :: Int
}


type SimpleIntResponseR = {
  simpleIntResponse :: Int
}


_SimpleIntResponse :: Lens' SimpleIntResponse {
  simpleIntResponse :: Int
}
_SimpleIntResponse f (SimpleIntResponse o) = SimpleIntResponse <$> f o


mkSimpleIntResponse :: Int -> SimpleIntResponse
mkSimpleIntResponse simpleIntResponse =
  SimpleIntResponse{simpleIntResponse}


unwrapSimpleIntResponse :: SimpleIntResponse -> {
  simpleIntResponse :: Int
}
unwrapSimpleIntResponse (SimpleIntResponse r) = r

instance simpleIntResponseEncodeJson :: EncodeJson SimpleIntResponse where
  encodeJson (SimpleIntResponse o) =
       "tag" := "SimpleIntResponse"
    ~> "simple_int_response" := o.simpleIntResponse
    ~> jsonEmptyObject


instance simpleIntResponseDecodeJson :: DecodeJson SimpleIntResponse where
  decodeJson o = do
    obj <- decodeJson o
    simpleIntResponse <- obj .? "simple_int_response"
    pure $ SimpleIntResponse {
      simpleIntResponse
    }


instance simpleIntResponseRequestable :: Requestable SimpleIntResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance simpleIntResponseRespondable :: Respondable SimpleIntResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkSimpleIntResponse
      <$> readProp "simple_int_response" json


instance simpleIntResponseDecode :: Decode SimpleIntResponse where
  read json =
      mkSimpleIntResponse
      <$> readProp "simple_int_response" json


newtype SimpleIntsRequest = SimpleIntsRequest {
  simpleIntsRequest :: (Array Int)
}


type SimpleIntsRequestR = {
  simpleIntsRequest :: (Array Int)
}


_SimpleIntsRequest :: Lens' SimpleIntsRequest {
  simpleIntsRequest :: (Array Int)
}
_SimpleIntsRequest f (SimpleIntsRequest o) = SimpleIntsRequest <$> f o


mkSimpleIntsRequest :: (Array Int) -> SimpleIntsRequest
mkSimpleIntsRequest simpleIntsRequest =
  SimpleIntsRequest{simpleIntsRequest}


unwrapSimpleIntsRequest :: SimpleIntsRequest -> {
  simpleIntsRequest :: (Array Int)
}
unwrapSimpleIntsRequest (SimpleIntsRequest r) = r

instance simpleIntsRequestEncodeJson :: EncodeJson SimpleIntsRequest where
  encodeJson (SimpleIntsRequest o) =
       "tag" := "SimpleIntsRequest"
    ~> "simple_ints_request" := o.simpleIntsRequest
    ~> jsonEmptyObject


instance simpleIntsRequestDecodeJson :: DecodeJson SimpleIntsRequest where
  decodeJson o = do
    obj <- decodeJson o
    simpleIntsRequest <- obj .? "simple_ints_request"
    pure $ SimpleIntsRequest {
      simpleIntsRequest
    }


instance simpleIntsRequestRequestable :: Requestable SimpleIntsRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance simpleIntsRequestRespondable :: Respondable SimpleIntsRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkSimpleIntsRequest
      <$> readProp "simple_ints_request" json


instance simpleIntsRequestDecode :: Decode SimpleIntsRequest where
  read json =
      mkSimpleIntsRequest
      <$> readProp "simple_ints_request" json


newtype SimpleIntsResponse = SimpleIntsResponse {
  simpleIntsResponse :: (Array Int)
}


type SimpleIntsResponseR = {
  simpleIntsResponse :: (Array Int)
}


_SimpleIntsResponse :: Lens' SimpleIntsResponse {
  simpleIntsResponse :: (Array Int)
}
_SimpleIntsResponse f (SimpleIntsResponse o) = SimpleIntsResponse <$> f o


mkSimpleIntsResponse :: (Array Int) -> SimpleIntsResponse
mkSimpleIntsResponse simpleIntsResponse =
  SimpleIntsResponse{simpleIntsResponse}


unwrapSimpleIntsResponse :: SimpleIntsResponse -> {
  simpleIntsResponse :: (Array Int)
}
unwrapSimpleIntsResponse (SimpleIntsResponse r) = r

instance simpleIntsResponseEncodeJson :: EncodeJson SimpleIntsResponse where
  encodeJson (SimpleIntsResponse o) =
       "tag" := "SimpleIntsResponse"
    ~> "simple_ints_response" := o.simpleIntsResponse
    ~> jsonEmptyObject


instance simpleIntsResponseDecodeJson :: DecodeJson SimpleIntsResponse where
  decodeJson o = do
    obj <- decodeJson o
    simpleIntsResponse <- obj .? "simple_ints_response"
    pure $ SimpleIntsResponse {
      simpleIntsResponse
    }


instance simpleIntsResponseRequestable :: Requestable SimpleIntsResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance simpleIntsResponseRespondable :: Respondable SimpleIntsResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkSimpleIntsResponse
      <$> readProp "simple_ints_response" json


instance simpleIntsResponseDecode :: Decode SimpleIntsResponse where
  read json =
      mkSimpleIntsResponse
      <$> readProp "simple_ints_response" json


newtype SimpleStringRequest = SimpleStringRequest {
  simpleStringRequest :: String
}


type SimpleStringRequestR = {
  simpleStringRequest :: String
}


_SimpleStringRequest :: Lens' SimpleStringRequest {
  simpleStringRequest :: String
}
_SimpleStringRequest f (SimpleStringRequest o) = SimpleStringRequest <$> f o


mkSimpleStringRequest :: String -> SimpleStringRequest
mkSimpleStringRequest simpleStringRequest =
  SimpleStringRequest{simpleStringRequest}


unwrapSimpleStringRequest :: SimpleStringRequest -> {
  simpleStringRequest :: String
}
unwrapSimpleStringRequest (SimpleStringRequest r) = r

instance simpleStringRequestEncodeJson :: EncodeJson SimpleStringRequest where
  encodeJson (SimpleStringRequest o) =
       "tag" := "SimpleStringRequest"
    ~> "simple_string_request" := o.simpleStringRequest
    ~> jsonEmptyObject


instance simpleStringRequestDecodeJson :: DecodeJson SimpleStringRequest where
  decodeJson o = do
    obj <- decodeJson o
    simpleStringRequest <- obj .? "simple_string_request"
    pure $ SimpleStringRequest {
      simpleStringRequest
    }


instance simpleStringRequestRequestable :: Requestable SimpleStringRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance simpleStringRequestRespondable :: Respondable SimpleStringRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkSimpleStringRequest
      <$> readProp "simple_string_request" json


instance simpleStringRequestDecode :: Decode SimpleStringRequest where
  read json =
      mkSimpleStringRequest
      <$> readProp "simple_string_request" json


newtype SimpleStringResponse = SimpleStringResponse {
  simpleStringResponse :: String
}


type SimpleStringResponseR = {
  simpleStringResponse :: String
}


_SimpleStringResponse :: Lens' SimpleStringResponse {
  simpleStringResponse :: String
}
_SimpleStringResponse f (SimpleStringResponse o) = SimpleStringResponse <$> f o


mkSimpleStringResponse :: String -> SimpleStringResponse
mkSimpleStringResponse simpleStringResponse =
  SimpleStringResponse{simpleStringResponse}


unwrapSimpleStringResponse :: SimpleStringResponse -> {
  simpleStringResponse :: String
}
unwrapSimpleStringResponse (SimpleStringResponse r) = r

instance simpleStringResponseEncodeJson :: EncodeJson SimpleStringResponse where
  encodeJson (SimpleStringResponse o) =
       "tag" := "SimpleStringResponse"
    ~> "simple_string_response" := o.simpleStringResponse
    ~> jsonEmptyObject


instance simpleStringResponseDecodeJson :: DecodeJson SimpleStringResponse where
  decodeJson o = do
    obj <- decodeJson o
    simpleStringResponse <- obj .? "simple_string_response"
    pure $ SimpleStringResponse {
      simpleStringResponse
    }


instance simpleStringResponseRequestable :: Requestable SimpleStringResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance simpleStringResponseRespondable :: Respondable SimpleStringResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkSimpleStringResponse
      <$> readProp "simple_string_response" json


instance simpleStringResponseDecode :: Decode SimpleStringResponse where
  read json =
      mkSimpleStringResponse
      <$> readProp "simple_string_response" json


newtype SimpleStringsRequest = SimpleStringsRequest {
  simpleStringsRequest :: (Array String)
}


type SimpleStringsRequestR = {
  simpleStringsRequest :: (Array String)
}


_SimpleStringsRequest :: Lens' SimpleStringsRequest {
  simpleStringsRequest :: (Array String)
}
_SimpleStringsRequest f (SimpleStringsRequest o) = SimpleStringsRequest <$> f o


mkSimpleStringsRequest :: (Array String) -> SimpleStringsRequest
mkSimpleStringsRequest simpleStringsRequest =
  SimpleStringsRequest{simpleStringsRequest}


unwrapSimpleStringsRequest :: SimpleStringsRequest -> {
  simpleStringsRequest :: (Array String)
}
unwrapSimpleStringsRequest (SimpleStringsRequest r) = r

instance simpleStringsRequestEncodeJson :: EncodeJson SimpleStringsRequest where
  encodeJson (SimpleStringsRequest o) =
       "tag" := "SimpleStringsRequest"
    ~> "simple_strings_request" := o.simpleStringsRequest
    ~> jsonEmptyObject


instance simpleStringsRequestDecodeJson :: DecodeJson SimpleStringsRequest where
  decodeJson o = do
    obj <- decodeJson o
    simpleStringsRequest <- obj .? "simple_strings_request"
    pure $ SimpleStringsRequest {
      simpleStringsRequest
    }


instance simpleStringsRequestRequestable :: Requestable SimpleStringsRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance simpleStringsRequestRespondable :: Respondable SimpleStringsRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkSimpleStringsRequest
      <$> readProp "simple_strings_request" json


instance simpleStringsRequestDecode :: Decode SimpleStringsRequest where
  read json =
      mkSimpleStringsRequest
      <$> readProp "simple_strings_request" json


newtype SimpleStringsResponse = SimpleStringsResponse {
  simpleStringsResponse :: (Array String)
}


type SimpleStringsResponseR = {
  simpleStringsResponse :: (Array String)
}


_SimpleStringsResponse :: Lens' SimpleStringsResponse {
  simpleStringsResponse :: (Array String)
}
_SimpleStringsResponse f (SimpleStringsResponse o) = SimpleStringsResponse <$> f o


mkSimpleStringsResponse :: (Array String) -> SimpleStringsResponse
mkSimpleStringsResponse simpleStringsResponse =
  SimpleStringsResponse{simpleStringsResponse}


unwrapSimpleStringsResponse :: SimpleStringsResponse -> {
  simpleStringsResponse :: (Array String)
}
unwrapSimpleStringsResponse (SimpleStringsResponse r) = r

instance simpleStringsResponseEncodeJson :: EncodeJson SimpleStringsResponse where
  encodeJson (SimpleStringsResponse o) =
       "tag" := "SimpleStringsResponse"
    ~> "simple_strings_response" := o.simpleStringsResponse
    ~> jsonEmptyObject


instance simpleStringsResponseDecodeJson :: DecodeJson SimpleStringsResponse where
  decodeJson o = do
    obj <- decodeJson o
    simpleStringsResponse <- obj .? "simple_strings_response"
    pure $ SimpleStringsResponse {
      simpleStringsResponse
    }


instance simpleStringsResponseRequestable :: Requestable SimpleStringsResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance simpleStringsResponseRespondable :: Respondable SimpleStringsResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkSimpleStringsResponse
      <$> readProp "simple_strings_response" json


instance simpleStringsResponseDecode :: Decode SimpleStringsResponse where
  read json =
      mkSimpleStringsResponse
      <$> readProp "simple_strings_response" json

-- footer