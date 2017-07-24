module LN.T.LeuronNode where
import LN.T.Training


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

newtype LeuronNodeRequest = LeuronNodeRequest {
  requestGuard :: Int
}


type LeuronNodeRequestR = {
  requestGuard :: Int
}


_LeuronNodeRequest :: Lens' LeuronNodeRequest {
  requestGuard :: Int
}
_LeuronNodeRequest f (LeuronNodeRequest o) = LeuronNodeRequest <$> f o


mkLeuronNodeRequest :: Int -> LeuronNodeRequest
mkLeuronNodeRequest requestGuard =
  LeuronNodeRequest{requestGuard}


unwrapLeuronNodeRequest :: LeuronNodeRequest -> {
  requestGuard :: Int
}
unwrapLeuronNodeRequest (LeuronNodeRequest r) = r

instance leuronNodeRequestEncodeJson :: EncodeJson LeuronNodeRequest where
  encodeJson (LeuronNodeRequest o) =
       "tag" := "LeuronNodeRequest"
    ~> "request_guard" := o.requestGuard
    ~> jsonEmptyObject


instance leuronNodeRequestDecodeJson :: DecodeJson LeuronNodeRequest where
  decodeJson o = do
    obj <- decodeJson o
    requestGuard <- obj .? "request_guard"
    pure $ LeuronNodeRequest {
      requestGuard
    }


instance leuronNodeRequestRequestable :: Requestable LeuronNodeRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance leuronNodeRequestRespondable :: Respondable LeuronNodeRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkLeuronNodeRequest
      <$> readProp "request_guard" json


instance leuronNodeRequestDecode :: Decode LeuronNodeRequest where
  read json =
      mkLeuronNodeRequest
      <$> readProp "request_guard" json


newtype LeuronNodeResponse = LeuronNodeResponse {
  id :: Int,
  userId :: Int,
  leuronId :: Int,
  trainingNode :: TrainingNode,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}


type LeuronNodeResponseR = {
  id :: Int,
  userId :: Int,
  leuronId :: Int,
  trainingNode :: TrainingNode,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}


_LeuronNodeResponse :: Lens' LeuronNodeResponse {
  id :: Int,
  userId :: Int,
  leuronId :: Int,
  trainingNode :: TrainingNode,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}
_LeuronNodeResponse f (LeuronNodeResponse o) = LeuronNodeResponse <$> f o


mkLeuronNodeResponse :: Int -> Int -> Int -> TrainingNode -> Boolean -> Int -> (Maybe Date) -> (Maybe Date) -> LeuronNodeResponse
mkLeuronNodeResponse id userId leuronId trainingNode active guard createdAt modifiedAt =
  LeuronNodeResponse{id, userId, leuronId, trainingNode, active, guard, createdAt, modifiedAt}


unwrapLeuronNodeResponse :: LeuronNodeResponse -> {
  id :: Int,
  userId :: Int,
  leuronId :: Int,
  trainingNode :: TrainingNode,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}
unwrapLeuronNodeResponse (LeuronNodeResponse r) = r

instance leuronNodeResponseEncodeJson :: EncodeJson LeuronNodeResponse where
  encodeJson (LeuronNodeResponse o) =
       "tag" := "LeuronNodeResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "leuron_id" := o.leuronId
    ~> "training_node" := o.trainingNode
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
    ~> jsonEmptyObject


instance leuronNodeResponseDecodeJson :: DecodeJson LeuronNodeResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    leuronId <- obj .? "leuron_id"
    trainingNode <- obj .? "training_node"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    pure $ LeuronNodeResponse {
      id,
      userId,
      leuronId,
      trainingNode,
      active,
      guard,
      createdAt,
      modifiedAt
    }


instance leuronNodeResponseRequestable :: Requestable LeuronNodeResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance leuronNodeResponseRespondable :: Respondable LeuronNodeResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkLeuronNodeResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "leuron_id" json
      <*> readProp "training_node" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)


instance leuronNodeResponseDecode :: Decode LeuronNodeResponse where
  read json =
      mkLeuronNodeResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "leuron_id" json
      <*> readProp "training_node" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)


newtype LeuronNodeResponses = LeuronNodeResponses {
  leuronNodeResponses :: (Array LeuronNodeResponse)
}


type LeuronNodeResponsesR = {
  leuronNodeResponses :: (Array LeuronNodeResponse)
}


_LeuronNodeResponses :: Lens' LeuronNodeResponses {
  leuronNodeResponses :: (Array LeuronNodeResponse)
}
_LeuronNodeResponses f (LeuronNodeResponses o) = LeuronNodeResponses <$> f o


mkLeuronNodeResponses :: (Array LeuronNodeResponse) -> LeuronNodeResponses
mkLeuronNodeResponses leuronNodeResponses =
  LeuronNodeResponses{leuronNodeResponses}


unwrapLeuronNodeResponses :: LeuronNodeResponses -> {
  leuronNodeResponses :: (Array LeuronNodeResponse)
}
unwrapLeuronNodeResponses (LeuronNodeResponses r) = r

instance leuronNodeResponsesEncodeJson :: EncodeJson LeuronNodeResponses where
  encodeJson (LeuronNodeResponses o) =
       "tag" := "LeuronNodeResponses"
    ~> "leuron_node_responses" := o.leuronNodeResponses
    ~> jsonEmptyObject


instance leuronNodeResponsesDecodeJson :: DecodeJson LeuronNodeResponses where
  decodeJson o = do
    obj <- decodeJson o
    leuronNodeResponses <- obj .? "leuron_node_responses"
    pure $ LeuronNodeResponses {
      leuronNodeResponses
    }


instance leuronNodeResponsesRequestable :: Requestable LeuronNodeResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance leuronNodeResponsesRespondable :: Respondable LeuronNodeResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkLeuronNodeResponses
      <$> readProp "leuron_node_responses" json


instance leuronNodeResponsesDecode :: Decode LeuronNodeResponses where
  read json =
      mkLeuronNodeResponses
      <$> readProp "leuron_node_responses" json

-- footer