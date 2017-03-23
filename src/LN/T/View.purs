module LN.T.View where
import LN.T.Ent


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
import Data.Foreign.Class               (class IsForeign, read, readProp)
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

newtype ViewRequest = ViewRequest {
  count :: Int
}


type ViewRequestR = {
  count :: Int
}


_ViewRequest :: Lens' ViewRequest {
  count :: Int
}
_ViewRequest f (ViewRequest o) = ViewRequest <$> f o


mkViewRequest :: Int -> ViewRequest
mkViewRequest count =
  ViewRequest{count}


unwrapViewRequest :: ViewRequest -> {
  count :: Int
}
unwrapViewRequest (ViewRequest r) = r

instance viewRequestEncodeJson :: EncodeJson ViewRequest where
  encodeJson (ViewRequest o) =
       "tag" := "ViewRequest"
    ~> "count" := o.count
    ~> jsonEmptyObject


instance viewRequestDecodeJson :: DecodeJson ViewRequest where
  decodeJson o = do
    obj <- decodeJson o
    count <- obj .? "count"
    pure $ ViewRequest {
      count
    }


instance viewRequestRequestable :: Requestable ViewRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance viewRequestRespondable :: Respondable ViewRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkViewRequest
      <$> readProp "count" json


instance viewRequestIsForeign :: IsForeign ViewRequest where
  read json =
      mkViewRequest
      <$> readProp "count" json


newtype ViewResponse = ViewResponse {
  id :: Int,
  ent :: Ent,
  entId :: Int,
  count :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}


type ViewResponseR = {
  id :: Int,
  ent :: Ent,
  entId :: Int,
  count :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}


_ViewResponse :: Lens' ViewResponse {
  id :: Int,
  ent :: Ent,
  entId :: Int,
  count :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}
_ViewResponse f (ViewResponse o) = ViewResponse <$> f o


mkViewResponse :: Int -> Ent -> Int -> Int -> (Maybe Date) -> (Maybe Date) -> ViewResponse
mkViewResponse id ent entId count createdAt modifiedAt =
  ViewResponse{id, ent, entId, count, createdAt, modifiedAt}


unwrapViewResponse :: ViewResponse -> {
  id :: Int,
  ent :: Ent,
  entId :: Int,
  count :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}
unwrapViewResponse (ViewResponse r) = r

instance viewResponseEncodeJson :: EncodeJson ViewResponse where
  encodeJson (ViewResponse o) =
       "tag" := "ViewResponse"
    ~> "id" := o.id
    ~> "ent" := o.ent
    ~> "ent_id" := o.entId
    ~> "count" := o.count
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
    ~> jsonEmptyObject


instance viewResponseDecodeJson :: DecodeJson ViewResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    ent <- obj .? "ent"
    entId <- obj .? "ent_id"
    count <- obj .? "count"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    pure $ ViewResponse {
      id,
      ent,
      entId,
      count,
      createdAt,
      modifiedAt
    }


instance viewResponseRequestable :: Requestable ViewResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance viewResponseRespondable :: Respondable ViewResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkViewResponse
      <$> readProp "id" json
      <*> readProp "ent" json
      <*> readProp "ent_id" json
      <*> readProp "count" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)


instance viewResponseIsForeign :: IsForeign ViewResponse where
  read json =
      mkViewResponse
      <$> readProp "id" json
      <*> readProp "ent" json
      <*> readProp "ent_id" json
      <*> readProp "count" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)


newtype ViewResponses = ViewResponses {
  viewResponses :: (Array ViewResponse)
}


type ViewResponsesR = {
  viewResponses :: (Array ViewResponse)
}


_ViewResponses :: Lens' ViewResponses {
  viewResponses :: (Array ViewResponse)
}
_ViewResponses f (ViewResponses o) = ViewResponses <$> f o


mkViewResponses :: (Array ViewResponse) -> ViewResponses
mkViewResponses viewResponses =
  ViewResponses{viewResponses}


unwrapViewResponses :: ViewResponses -> {
  viewResponses :: (Array ViewResponse)
}
unwrapViewResponses (ViewResponses r) = r

instance viewResponsesEncodeJson :: EncodeJson ViewResponses where
  encodeJson (ViewResponses o) =
       "tag" := "ViewResponses"
    ~> "view_responses" := o.viewResponses
    ~> jsonEmptyObject


instance viewResponsesDecodeJson :: DecodeJson ViewResponses where
  decodeJson o = do
    obj <- decodeJson o
    viewResponses <- obj .? "view_responses"
    pure $ ViewResponses {
      viewResponses
    }


instance viewResponsesRequestable :: Requestable ViewResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance viewResponsesRespondable :: Respondable ViewResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkViewResponses
      <$> readProp "view_responses" json


instance viewResponsesIsForeign :: IsForeign ViewResponses where
  read json =
      mkViewResponses
      <$> readProp "view_responses" json

-- footer