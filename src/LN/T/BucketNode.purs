module LN.T.BucketNode where
import LN.T.Training


import Data.Argonaut.Core               (jsonEmptyObject, stringify)
import Data.Argonaut.Decode             (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode             (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
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

newtype BucketNodeRequest = BucketNodeRequest {
  requestGuard :: Int
}


type BucketNodeRequestR = {
  requestGuard :: Int
}


_BucketNodeRequest :: Lens' BucketNodeRequest {
  requestGuard :: Int
}
_BucketNodeRequest f (BucketNodeRequest o) = BucketNodeRequest <$> f o


mkBucketNodeRequest :: Int -> BucketNodeRequest
mkBucketNodeRequest requestGuard =
  BucketNodeRequest{requestGuard}


unwrapBucketNodeRequest :: BucketNodeRequest -> {
  requestGuard :: Int
}
unwrapBucketNodeRequest (BucketNodeRequest r) = r

instance bucketNodeRequestEncodeJson :: EncodeJson BucketNodeRequest where
  encodeJson (BucketNodeRequest o) =
       "tag" := "BucketNodeRequest"
    ~> "request_guard" := o.requestGuard
    ~> jsonEmptyObject


instance bucketNodeRequestDecodeJson :: DecodeJson BucketNodeRequest where
  decodeJson o = do
    obj <- decodeJson o
    requestGuard <- obj .? "request_guard"
    pure $ BucketNodeRequest {
      requestGuard
    }


instance bucketNodeRequestRequestable :: Requestable BucketNodeRequest where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance bucketNodeRequestRespondable :: Respondable BucketNodeRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkBucketNodeRequest
      <$> readProp "request_guard" json


instance bucketNodeRequestDecode :: Decode BucketNodeRequest where
  read json =
      mkBucketNodeRequest
      <$> readProp "request_guard" json


newtype BucketNodeResponse = BucketNodeResponse {
  id :: Int,
  userId :: Int,
  bucketId :: Int,
  leuronId :: Int,
  timeLimit :: Int,
  timeLimitExceeded :: Int,
  style :: String,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}


type BucketNodeResponseR = {
  id :: Int,
  userId :: Int,
  bucketId :: Int,
  leuronId :: Int,
  timeLimit :: Int,
  timeLimitExceeded :: Int,
  style :: String,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}


_BucketNodeResponse :: Lens' BucketNodeResponse {
  id :: Int,
  userId :: Int,
  bucketId :: Int,
  leuronId :: Int,
  timeLimit :: Int,
  timeLimitExceeded :: Int,
  style :: String,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}
_BucketNodeResponse f (BucketNodeResponse o) = BucketNodeResponse <$> f o


mkBucketNodeResponse :: Int -> Int -> Int -> Int -> Int -> Int -> String -> Boolean -> Int -> (Maybe Date) -> (Maybe Date) -> BucketNodeResponse
mkBucketNodeResponse id userId bucketId leuronId timeLimit timeLimitExceeded style active guard createdAt modifiedAt =
  BucketNodeResponse{id, userId, bucketId, leuronId, timeLimit, timeLimitExceeded, style, active, guard, createdAt, modifiedAt}


unwrapBucketNodeResponse :: BucketNodeResponse -> {
  id :: Int,
  userId :: Int,
  bucketId :: Int,
  leuronId :: Int,
  timeLimit :: Int,
  timeLimitExceeded :: Int,
  style :: String,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}
unwrapBucketNodeResponse (BucketNodeResponse r) = r

instance bucketNodeResponseEncodeJson :: EncodeJson BucketNodeResponse where
  encodeJson (BucketNodeResponse o) =
       "tag" := "BucketNodeResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "bucket_id" := o.bucketId
    ~> "leuron_id" := o.leuronId
    ~> "time_limit" := o.timeLimit
    ~> "time_limit_exceeded" := o.timeLimitExceeded
    ~> "style" := o.style
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
    ~> jsonEmptyObject


instance bucketNodeResponseDecodeJson :: DecodeJson BucketNodeResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    bucketId <- obj .? "bucket_id"
    leuronId <- obj .? "leuron_id"
    timeLimit <- obj .? "time_limit"
    timeLimitExceeded <- obj .? "time_limit_exceeded"
    style <- obj .? "style"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    pure $ BucketNodeResponse {
      id,
      userId,
      bucketId,
      leuronId,
      timeLimit,
      timeLimitExceeded,
      style,
      active,
      guard,
      createdAt,
      modifiedAt
    }


instance bucketNodeResponseRequestable :: Requestable BucketNodeResponse where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance bucketNodeResponseRespondable :: Respondable BucketNodeResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkBucketNodeResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "bucket_id" json
      <*> readProp "leuron_id" json
      <*> readProp "time_limit" json
      <*> readProp "time_limit_exceeded" json
      <*> readProp "style" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)


instance bucketNodeResponseDecode :: Decode BucketNodeResponse where
  read json =
      mkBucketNodeResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "bucket_id" json
      <*> readProp "leuron_id" json
      <*> readProp "time_limit" json
      <*> readProp "time_limit_exceeded" json
      <*> readProp "style" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)


newtype BucketNodeResponses = BucketNodeResponses {
  bucketNodeResponses :: (Array BucketNodeResponse)
}


type BucketNodeResponsesR = {
  bucketNodeResponses :: (Array BucketNodeResponse)
}


_BucketNodeResponses :: Lens' BucketNodeResponses {
  bucketNodeResponses :: (Array BucketNodeResponse)
}
_BucketNodeResponses f (BucketNodeResponses o) = BucketNodeResponses <$> f o


mkBucketNodeResponses :: (Array BucketNodeResponse) -> BucketNodeResponses
mkBucketNodeResponses bucketNodeResponses =
  BucketNodeResponses{bucketNodeResponses}


unwrapBucketNodeResponses :: BucketNodeResponses -> {
  bucketNodeResponses :: (Array BucketNodeResponse)
}
unwrapBucketNodeResponses (BucketNodeResponses r) = r

instance bucketNodeResponsesEncodeJson :: EncodeJson BucketNodeResponses where
  encodeJson (BucketNodeResponses o) =
       "tag" := "BucketNodeResponses"
    ~> "bucket_node_responses" := o.bucketNodeResponses
    ~> jsonEmptyObject


instance bucketNodeResponsesDecodeJson :: DecodeJson BucketNodeResponses where
  decodeJson o = do
    obj <- decodeJson o
    bucketNodeResponses <- obj .? "bucket_node_responses"
    pure $ BucketNodeResponses {
      bucketNodeResponses
    }


instance bucketNodeResponsesRequestable :: Requestable BucketNodeResponses where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance bucketNodeResponsesRespondable :: Respondable BucketNodeResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkBucketNodeResponses
      <$> readProp "bucket_node_responses" json


instance bucketNodeResponsesDecode :: Decode BucketNodeResponses where
  read json =
      mkBucketNodeResponses
      <$> readProp "bucket_node_responses" json

-- footer