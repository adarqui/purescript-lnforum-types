module LN.T.Api where



import Data.Argonaut.Core               (jsonEmptyObject, stringify)
import Data.Argonaut.Decode             (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode             (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Date.Helpers                (Date)
import Data.Either                      (Either(..))
import Data.Foreign                     (ForeignError(..), fail, unsafeFromForeign)
import Data.Foreign.NullOrUndefined     (unNullOrUndefined)
import Data.Foreign.Class               (class Decode, decode)
import Data.Foreign.Helpers             (readPropUnsafe)
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

newtype ApiRequest = ApiRequest {
  comment :: (Maybe String),
  guard :: Int
}


type ApiRequestR = {
  comment :: (Maybe String),
  guard :: Int
}


_ApiRequest :: Lens' ApiRequest {
  comment :: (Maybe String),
  guard :: Int
}
_ApiRequest f (ApiRequest o) = ApiRequest <$> f o


mkApiRequest :: (Maybe String) -> Int -> ApiRequest
mkApiRequest comment guard =
  ApiRequest{comment, guard}


unwrapApiRequest :: ApiRequest -> {
  comment :: (Maybe String),
  guard :: Int
}
unwrapApiRequest (ApiRequest r) = r

instance apiRequestEncodeJson :: EncodeJson ApiRequest where
  encodeJson (ApiRequest o) =
       "tag" := "ApiRequest"
    ~> "comment" := o.comment
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance apiRequestDecodeJson :: DecodeJson ApiRequest where
  decodeJson o = do
    obj <- decodeJson o
    comment <- obj .? "comment"
    guard <- obj .? "guard"
    pure $ ApiRequest {
      comment,
      guard
    }


instance apiRequestRequestable :: Requestable ApiRequest where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance apiRequestRespondable :: Respondable ApiRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkApiRequest
      <$> (unNullOrUndefined <$> readPropUnsafe "comment" json)
      <*> readPropUnsafe "guard" json


instance apiRequestDecode :: Decode ApiRequest where
  decode json =
      mkApiRequest
      <$> (unNullOrUndefined <$> readPropUnsafe "comment" json)
      <*> readPropUnsafe "guard" json


newtype ApiResponse = ApiResponse {
  id :: Int,
  userId :: Int,
  key :: String,
  comment :: (Maybe String),
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}


type ApiResponseR = {
  id :: Int,
  userId :: Int,
  key :: String,
  comment :: (Maybe String),
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}


_ApiResponse :: Lens' ApiResponse {
  id :: Int,
  userId :: Int,
  key :: String,
  comment :: (Maybe String),
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}
_ApiResponse f (ApiResponse o) = ApiResponse <$> f o


mkApiResponse :: Int -> Int -> String -> (Maybe String) -> Int -> (Maybe Date) -> (Maybe Date) -> ApiResponse
mkApiResponse id userId key comment guard createdAt modifiedAt =
  ApiResponse{id, userId, key, comment, guard, createdAt, modifiedAt}


unwrapApiResponse :: ApiResponse -> {
  id :: Int,
  userId :: Int,
  key :: String,
  comment :: (Maybe String),
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}
unwrapApiResponse (ApiResponse r) = r

instance apiResponseEncodeJson :: EncodeJson ApiResponse where
  encodeJson (ApiResponse o) =
       "tag" := "ApiResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "key" := o.key
    ~> "comment" := o.comment
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
    ~> jsonEmptyObject


instance apiResponseDecodeJson :: DecodeJson ApiResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    key <- obj .? "key"
    comment <- obj .? "comment"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    pure $ ApiResponse {
      id,
      userId,
      key,
      comment,
      guard,
      createdAt,
      modifiedAt
    }


instance apiResponseRequestable :: Requestable ApiResponse where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance apiResponseRespondable :: Respondable ApiResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkApiResponse
      <$> readPropUnsafe "id" json
      <*> readPropUnsafe "user_id" json
      <*> readPropUnsafe "key" json
      <*> (unNullOrUndefined <$> readPropUnsafe "comment" json)
      <*> readPropUnsafe "guard" json
      <*> (unNullOrUndefined <$> readPropUnsafe "created_at" json)
      <*> (unNullOrUndefined <$> readPropUnsafe "modified_at" json)


instance apiResponseDecode :: Decode ApiResponse where
  decode json =
      mkApiResponse
      <$> readPropUnsafe "id" json
      <*> readPropUnsafe "user_id" json
      <*> readPropUnsafe "key" json
      <*> (unNullOrUndefined <$> readPropUnsafe "comment" json)
      <*> readPropUnsafe "guard" json
      <*> (unNullOrUndefined <$> readPropUnsafe "created_at" json)
      <*> (unNullOrUndefined <$> readPropUnsafe "modified_at" json)


newtype ApiResponses = ApiResponses {
  apiResponses :: (Array ApiResponse)
}


type ApiResponsesR = {
  apiResponses :: (Array ApiResponse)
}


_ApiResponses :: Lens' ApiResponses {
  apiResponses :: (Array ApiResponse)
}
_ApiResponses f (ApiResponses o) = ApiResponses <$> f o


mkApiResponses :: (Array ApiResponse) -> ApiResponses
mkApiResponses apiResponses =
  ApiResponses{apiResponses}


unwrapApiResponses :: ApiResponses -> {
  apiResponses :: (Array ApiResponse)
}
unwrapApiResponses (ApiResponses r) = r

instance apiResponsesEncodeJson :: EncodeJson ApiResponses where
  encodeJson (ApiResponses o) =
       "tag" := "ApiResponses"
    ~> "api_responses" := o.apiResponses
    ~> jsonEmptyObject


instance apiResponsesDecodeJson :: DecodeJson ApiResponses where
  decodeJson o = do
    obj <- decodeJson o
    apiResponses <- obj .? "api_responses"
    pure $ ApiResponses {
      apiResponses
    }


instance apiResponsesRequestable :: Requestable ApiResponses where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance apiResponsesRespondable :: Respondable ApiResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkApiResponses
      <$> readPropUnsafe "api_responses" json


instance apiResponsesDecode :: Decode ApiResponses where
  decode json =
      mkApiResponses
      <$> readPropUnsafe "api_responses" json

-- footer