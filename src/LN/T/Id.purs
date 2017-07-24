module LN.T.Id where



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

newtype IdRequest = IdRequest {
  targetId :: Int,
  guard :: Int
}


type IdRequestR = {
  targetId :: Int,
  guard :: Int
}


_IdRequest :: Lens' IdRequest {
  targetId :: Int,
  guard :: Int
}
_IdRequest f (IdRequest o) = IdRequest <$> f o


mkIdRequest :: Int -> Int -> IdRequest
mkIdRequest targetId guard =
  IdRequest{targetId, guard}


unwrapIdRequest :: IdRequest -> {
  targetId :: Int,
  guard :: Int
}
unwrapIdRequest (IdRequest r) = r

instance idRequestEncodeJson :: EncodeJson IdRequest where
  encodeJson (IdRequest o) =
       "tag" := "IdRequest"
    ~> "target_id" := o.targetId
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance idRequestDecodeJson :: DecodeJson IdRequest where
  decodeJson o = do
    obj <- decodeJson o
    targetId <- obj .? "target_id"
    guard <- obj .? "guard"
    pure $ IdRequest {
      targetId,
      guard
    }


instance idRequestRequestable :: Requestable IdRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance idRequestRespondable :: Respondable IdRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkIdRequest
      <$> readProp "target_id" json
      <*> readProp "guard" json


instance idRequestDecode :: Decode IdRequest where
  read json =
      mkIdRequest
      <$> readProp "target_id" json
      <*> readProp "guard" json


newtype IdResponse = IdResponse {
  id :: Int,
  userId :: Int,
  targetId :: Int,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


type IdResponseR = {
  id :: Int,
  userId :: Int,
  targetId :: Int,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


_IdResponse :: Lens' IdResponse {
  id :: Int,
  userId :: Int,
  targetId :: Int,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
_IdResponse f (IdResponse o) = IdResponse <$> f o


mkIdResponse :: Int -> Int -> Int -> Int -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> IdResponse
mkIdResponse id userId targetId guard createdAt modifiedAt activityAt =
  IdResponse{id, userId, targetId, guard, createdAt, modifiedAt, activityAt}


unwrapIdResponse :: IdResponse -> {
  id :: Int,
  userId :: Int,
  targetId :: Int,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
unwrapIdResponse (IdResponse r) = r

instance idResponseEncodeJson :: EncodeJson IdResponse where
  encodeJson (IdResponse o) =
       "tag" := "IdResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "target_id" := o.targetId
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
    ~> "activity_at" := o.activityAt
    ~> jsonEmptyObject


instance idResponseDecodeJson :: DecodeJson IdResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    targetId <- obj .? "target_id"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    activityAt <- obj .? "activity_at"
    pure $ IdResponse {
      id,
      userId,
      targetId,
      guard,
      createdAt,
      modifiedAt,
      activityAt
    }


instance idResponseRequestable :: Requestable IdResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance idResponseRespondable :: Respondable IdResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkIdResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "target_id" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


instance idResponseDecode :: Decode IdResponse where
  read json =
      mkIdResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "target_id" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


newtype IdResponses = IdResponses {
  idResponses :: (Array IdResponse)
}


type IdResponsesR = {
  idResponses :: (Array IdResponse)
}


_IdResponses :: Lens' IdResponses {
  idResponses :: (Array IdResponse)
}
_IdResponses f (IdResponses o) = IdResponses <$> f o


mkIdResponses :: (Array IdResponse) -> IdResponses
mkIdResponses idResponses =
  IdResponses{idResponses}


unwrapIdResponses :: IdResponses -> {
  idResponses :: (Array IdResponse)
}
unwrapIdResponses (IdResponses r) = r

instance idResponsesEncodeJson :: EncodeJson IdResponses where
  encodeJson (IdResponses o) =
       "tag" := "IdResponses"
    ~> "id_responses" := o.idResponses
    ~> jsonEmptyObject


instance idResponsesDecodeJson :: DecodeJson IdResponses where
  decodeJson o = do
    obj <- decodeJson o
    idResponses <- obj .? "id_responses"
    pure $ IdResponses {
      idResponses
    }


instance idResponsesRequestable :: Requestable IdResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance idResponsesRespondable :: Respondable IdResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkIdResponses
      <$> readProp "id_responses" json


instance idResponsesDecode :: Decode IdResponses where
  read json =
      mkIdResponses
      <$> readProp "id_responses" json

-- footer