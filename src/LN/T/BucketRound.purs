module LN.T.BucketRound where
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

newtype BucketRoundRequest = BucketRoundRequest {
  trainingStyles :: (Array TrainingStyle),
  threshold :: Int,
  timeLimit :: Int,
  guard :: Int
}


type BucketRoundRequestR = {
  trainingStyles :: (Array TrainingStyle),
  threshold :: Int,
  timeLimit :: Int,
  guard :: Int
}


_BucketRoundRequest :: Lens' BucketRoundRequest {
  trainingStyles :: (Array TrainingStyle),
  threshold :: Int,
  timeLimit :: Int,
  guard :: Int
}
_BucketRoundRequest f (BucketRoundRequest o) = BucketRoundRequest <$> f o


mkBucketRoundRequest :: (Array TrainingStyle) -> Int -> Int -> Int -> BucketRoundRequest
mkBucketRoundRequest trainingStyles threshold timeLimit guard =
  BucketRoundRequest{trainingStyles, threshold, timeLimit, guard}


unwrapBucketRoundRequest :: BucketRoundRequest -> {
  trainingStyles :: (Array TrainingStyle),
  threshold :: Int,
  timeLimit :: Int,
  guard :: Int
}
unwrapBucketRoundRequest (BucketRoundRequest r) = r

instance bucketRoundRequestEncodeJson :: EncodeJson BucketRoundRequest where
  encodeJson (BucketRoundRequest o) =
       "tag" := "BucketRoundRequest"
    ~> "training_styles" := o.trainingStyles
    ~> "threshold" := o.threshold
    ~> "time_limit" := o.timeLimit
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance bucketRoundRequestDecodeJson :: DecodeJson BucketRoundRequest where
  decodeJson o = do
    obj <- decodeJson o
    trainingStyles <- obj .? "training_styles"
    threshold <- obj .? "threshold"
    timeLimit <- obj .? "time_limit"
    guard <- obj .? "guard"
    pure $ BucketRoundRequest {
      trainingStyles,
      threshold,
      timeLimit,
      guard
    }


instance bucketRoundRequestRequestable :: Requestable BucketRoundRequest where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance bucketRoundRequestRespondable :: Respondable BucketRoundRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkBucketRoundRequest
      <$> readProp "training_styles" json
      <*> readProp "threshold" json
      <*> readProp "time_limit" json
      <*> readProp "guard" json


instance bucketRoundRequestDecode :: Decode BucketRoundRequest where
  read json =
      mkBucketRoundRequest
      <$> readProp "training_styles" json
      <*> readProp "threshold" json
      <*> readProp "time_limit" json
      <*> readProp "guard" json


newtype BucketRoundResponse = BucketRoundResponse {
  id :: Int,
  userId :: Int,
  bucketId :: Int,
  trainingStyles :: (Array TrainingStyle),
  threshold :: Int,
  timeLimit :: Int,
  trainingNode :: TrainingNode,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


type BucketRoundResponseR = {
  id :: Int,
  userId :: Int,
  bucketId :: Int,
  trainingStyles :: (Array TrainingStyle),
  threshold :: Int,
  timeLimit :: Int,
  trainingNode :: TrainingNode,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


_BucketRoundResponse :: Lens' BucketRoundResponse {
  id :: Int,
  userId :: Int,
  bucketId :: Int,
  trainingStyles :: (Array TrainingStyle),
  threshold :: Int,
  timeLimit :: Int,
  trainingNode :: TrainingNode,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
_BucketRoundResponse f (BucketRoundResponse o) = BucketRoundResponse <$> f o


mkBucketRoundResponse :: Int -> Int -> Int -> (Array TrainingStyle) -> Int -> Int -> TrainingNode -> Boolean -> Int -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> BucketRoundResponse
mkBucketRoundResponse id userId bucketId trainingStyles threshold timeLimit trainingNode active guard createdAt modifiedAt activityAt =
  BucketRoundResponse{id, userId, bucketId, trainingStyles, threshold, timeLimit, trainingNode, active, guard, createdAt, modifiedAt, activityAt}


unwrapBucketRoundResponse :: BucketRoundResponse -> {
  id :: Int,
  userId :: Int,
  bucketId :: Int,
  trainingStyles :: (Array TrainingStyle),
  threshold :: Int,
  timeLimit :: Int,
  trainingNode :: TrainingNode,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
unwrapBucketRoundResponse (BucketRoundResponse r) = r

instance bucketRoundResponseEncodeJson :: EncodeJson BucketRoundResponse where
  encodeJson (BucketRoundResponse o) =
       "tag" := "BucketRoundResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "bucket_id" := o.bucketId
    ~> "training_styles" := o.trainingStyles
    ~> "threshold" := o.threshold
    ~> "time_limit" := o.timeLimit
    ~> "training_node" := o.trainingNode
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
    ~> "activity_at" := o.activityAt
    ~> jsonEmptyObject


instance bucketRoundResponseDecodeJson :: DecodeJson BucketRoundResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    bucketId <- obj .? "bucket_id"
    trainingStyles <- obj .? "training_styles"
    threshold <- obj .? "threshold"
    timeLimit <- obj .? "time_limit"
    trainingNode <- obj .? "training_node"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    activityAt <- obj .? "activity_at"
    pure $ BucketRoundResponse {
      id,
      userId,
      bucketId,
      trainingStyles,
      threshold,
      timeLimit,
      trainingNode,
      active,
      guard,
      createdAt,
      modifiedAt,
      activityAt
    }


instance bucketRoundResponseRequestable :: Requestable BucketRoundResponse where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance bucketRoundResponseRespondable :: Respondable BucketRoundResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkBucketRoundResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "bucket_id" json
      <*> readProp "training_styles" json
      <*> readProp "threshold" json
      <*> readProp "time_limit" json
      <*> readProp "training_node" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


instance bucketRoundResponseDecode :: Decode BucketRoundResponse where
  read json =
      mkBucketRoundResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "bucket_id" json
      <*> readProp "training_styles" json
      <*> readProp "threshold" json
      <*> readProp "time_limit" json
      <*> readProp "training_node" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


newtype BucketRoundResponses = BucketRoundResponses {
  bucketRoundResponses :: (Array BucketRoundResponse)
}


type BucketRoundResponsesR = {
  bucketRoundResponses :: (Array BucketRoundResponse)
}


_BucketRoundResponses :: Lens' BucketRoundResponses {
  bucketRoundResponses :: (Array BucketRoundResponse)
}
_BucketRoundResponses f (BucketRoundResponses o) = BucketRoundResponses <$> f o


mkBucketRoundResponses :: (Array BucketRoundResponse) -> BucketRoundResponses
mkBucketRoundResponses bucketRoundResponses =
  BucketRoundResponses{bucketRoundResponses}


unwrapBucketRoundResponses :: BucketRoundResponses -> {
  bucketRoundResponses :: (Array BucketRoundResponse)
}
unwrapBucketRoundResponses (BucketRoundResponses r) = r

instance bucketRoundResponsesEncodeJson :: EncodeJson BucketRoundResponses where
  encodeJson (BucketRoundResponses o) =
       "tag" := "BucketRoundResponses"
    ~> "bucket_round_responses" := o.bucketRoundResponses
    ~> jsonEmptyObject


instance bucketRoundResponsesDecodeJson :: DecodeJson BucketRoundResponses where
  decodeJson o = do
    obj <- decodeJson o
    bucketRoundResponses <- obj .? "bucket_round_responses"
    pure $ BucketRoundResponses {
      bucketRoundResponses
    }


instance bucketRoundResponsesRequestable :: Requestable BucketRoundResponses where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance bucketRoundResponsesRespondable :: Respondable BucketRoundResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkBucketRoundResponses
      <$> readProp "bucket_round_responses" json


instance bucketRoundResponsesDecode :: Decode BucketRoundResponses where
  read json =
      mkBucketRoundResponses
      <$> readProp "bucket_round_responses" json

-- footer