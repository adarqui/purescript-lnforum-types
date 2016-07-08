module LN.T.Star where
import LN.T.Ent


import Data.Argonaut.Core               (jsonEmptyObject)
import Data.Argonaut.Decode             (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode             (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Argonaut.Printer            (printJson)
import Data.Date.Helpers                (Date)
import Data.Either                      (Either(..))
import Data.Foreign                     (ForeignError(..))
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

import Purescript.Api.Helpers

newtype StarRequest = StarRequest {
  reason :: (Maybe String),
  guard :: Int
}


type StarRequestR = {
  reason :: (Maybe String),
  guard :: Int
}


_StarRequest :: Lens' StarRequest {
  reason :: (Maybe String),
  guard :: Int
}
_StarRequest f (StarRequest o) = StarRequest <$> f o


mkStarRequest :: (Maybe String) -> Int -> StarRequest
mkStarRequest reason guard =
  StarRequest{reason, guard}


unwrapStarRequest :: StarRequest -> {
  reason :: (Maybe String),
  guard :: Int
}
unwrapStarRequest (StarRequest r) = r

instance starRequestEncodeJson :: EncodeJson StarRequest where
  encodeJson (StarRequest o) =
       "tag" := "StarRequest"
    ~> "reason" := o.reason
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance starRequestDecodeJson :: DecodeJson StarRequest where
  decodeJson o = do
    obj <- decodeJson o
    reason <- obj .? "reason"
    guard <- obj .? "guard"
    pure $ StarRequest {
      reason,
      guard
    }


instance starRequestRequestable :: Requestable StarRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance starRequestRespondable :: Respondable StarRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkStarRequest
      <$> (unNullOrUndefined <$> readProp "reason" json)
      <*> readProp "guard" json


instance starRequestIsForeign :: IsForeign StarRequest where
  read json =
      mkStarRequest
      <$> (unNullOrUndefined <$> readProp "reason" json)
      <*> readProp "guard" json


newtype StarResponse = StarResponse {
  id :: Int,
  ent :: Ent,
  entId :: Int,
  userId :: Int,
  reason :: (Maybe String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}


type StarResponseR = {
  id :: Int,
  ent :: Ent,
  entId :: Int,
  userId :: Int,
  reason :: (Maybe String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}


_StarResponse :: Lens' StarResponse {
  id :: Int,
  ent :: Ent,
  entId :: Int,
  userId :: Int,
  reason :: (Maybe String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}
_StarResponse f (StarResponse o) = StarResponse <$> f o


mkStarResponse :: Int -> Ent -> Int -> Int -> (Maybe String) -> Boolean -> Int -> (Maybe Date) -> (Maybe Date) -> StarResponse
mkStarResponse id ent entId userId reason active guard createdAt modifiedAt =
  StarResponse{id, ent, entId, userId, reason, active, guard, createdAt, modifiedAt}


unwrapStarResponse :: StarResponse -> {
  id :: Int,
  ent :: Ent,
  entId :: Int,
  userId :: Int,
  reason :: (Maybe String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}
unwrapStarResponse (StarResponse r) = r

instance starResponseEncodeJson :: EncodeJson StarResponse where
  encodeJson (StarResponse o) =
       "tag" := "StarResponse"
    ~> "id" := o.id
    ~> "ent" := o.ent
    ~> "ent_id" := o.entId
    ~> "user_id" := o.userId
    ~> "reason" := o.reason
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
    ~> jsonEmptyObject


instance starResponseDecodeJson :: DecodeJson StarResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    ent <- obj .? "ent"
    entId <- obj .? "ent_id"
    userId <- obj .? "user_id"
    reason <- obj .? "reason"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    pure $ StarResponse {
      id,
      ent,
      entId,
      userId,
      reason,
      active,
      guard,
      createdAt,
      modifiedAt
    }


instance starResponseRequestable :: Requestable StarResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance starResponseRespondable :: Respondable StarResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkStarResponse
      <$> readProp "id" json
      <*> readProp "ent" json
      <*> readProp "ent_id" json
      <*> readProp "user_id" json
      <*> (unNullOrUndefined <$> readProp "reason" json)
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)


instance starResponseIsForeign :: IsForeign StarResponse where
  read json =
      mkStarResponse
      <$> readProp "id" json
      <*> readProp "ent" json
      <*> readProp "ent_id" json
      <*> readProp "user_id" json
      <*> (unNullOrUndefined <$> readProp "reason" json)
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)


newtype StarResponses = StarResponses {
  starResponses :: (Array StarResponse)
}


type StarResponsesR = {
  starResponses :: (Array StarResponse)
}


_StarResponses :: Lens' StarResponses {
  starResponses :: (Array StarResponse)
}
_StarResponses f (StarResponses o) = StarResponses <$> f o


mkStarResponses :: (Array StarResponse) -> StarResponses
mkStarResponses starResponses =
  StarResponses{starResponses}


unwrapStarResponses :: StarResponses -> {
  starResponses :: (Array StarResponse)
}
unwrapStarResponses (StarResponses r) = r

instance starResponsesEncodeJson :: EncodeJson StarResponses where
  encodeJson (StarResponses o) =
       "tag" := "StarResponses"
    ~> "star_responses" := o.starResponses
    ~> jsonEmptyObject


instance starResponsesDecodeJson :: DecodeJson StarResponses where
  decodeJson o = do
    obj <- decodeJson o
    starResponses <- obj .? "star_responses"
    pure $ StarResponses {
      starResponses
    }


instance starResponsesRequestable :: Requestable StarResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance starResponsesRespondable :: Respondable StarResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkStarResponses
      <$> readProp "star_responses" json


instance starResponsesIsForeign :: IsForeign StarResponses where
  read json =
      mkStarResponses
      <$> readProp "star_responses" json


newtype StarStatResponse = StarStatResponse {
  ent :: Ent,
  entId :: Int,
  stars :: Int
}


type StarStatResponseR = {
  ent :: Ent,
  entId :: Int,
  stars :: Int
}


_StarStatResponse :: Lens' StarStatResponse {
  ent :: Ent,
  entId :: Int,
  stars :: Int
}
_StarStatResponse f (StarStatResponse o) = StarStatResponse <$> f o


mkStarStatResponse :: Ent -> Int -> Int -> StarStatResponse
mkStarStatResponse ent entId stars =
  StarStatResponse{ent, entId, stars}


unwrapStarStatResponse :: StarStatResponse -> {
  ent :: Ent,
  entId :: Int,
  stars :: Int
}
unwrapStarStatResponse (StarStatResponse r) = r

instance starStatResponseEncodeJson :: EncodeJson StarStatResponse where
  encodeJson (StarStatResponse o) =
       "tag" := "StarStatResponse"
    ~> "ent" := o.ent
    ~> "ent_id" := o.entId
    ~> "stars" := o.stars
    ~> jsonEmptyObject


instance starStatResponseDecodeJson :: DecodeJson StarStatResponse where
  decodeJson o = do
    obj <- decodeJson o
    ent <- obj .? "ent"
    entId <- obj .? "ent_id"
    stars <- obj .? "stars"
    pure $ StarStatResponse {
      ent,
      entId,
      stars
    }


instance starStatResponseRequestable :: Requestable StarStatResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance starStatResponseRespondable :: Respondable StarStatResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkStarStatResponse
      <$> readProp "ent" json
      <*> readProp "ent_id" json
      <*> readProp "stars" json


instance starStatResponseIsForeign :: IsForeign StarStatResponse where
  read json =
      mkStarStatResponse
      <$> readProp "ent" json
      <*> readProp "ent_id" json
      <*> readProp "stars" json


newtype StarStatResponses = StarStatResponses {
  starStatResponses :: (Array StarStatResponse)
}


type StarStatResponsesR = {
  starStatResponses :: (Array StarStatResponse)
}


_StarStatResponses :: Lens' StarStatResponses {
  starStatResponses :: (Array StarStatResponse)
}
_StarStatResponses f (StarStatResponses o) = StarStatResponses <$> f o


mkStarStatResponses :: (Array StarStatResponse) -> StarStatResponses
mkStarStatResponses starStatResponses =
  StarStatResponses{starStatResponses}


unwrapStarStatResponses :: StarStatResponses -> {
  starStatResponses :: (Array StarStatResponse)
}
unwrapStarStatResponses (StarStatResponses r) = r

instance starStatResponsesEncodeJson :: EncodeJson StarStatResponses where
  encodeJson (StarStatResponses o) =
       "tag" := "StarStatResponses"
    ~> "star_stat_responses" := o.starStatResponses
    ~> jsonEmptyObject


instance starStatResponsesDecodeJson :: DecodeJson StarStatResponses where
  decodeJson o = do
    obj <- decodeJson o
    starStatResponses <- obj .? "star_stat_responses"
    pure $ StarStatResponses {
      starStatResponses
    }


instance starStatResponsesRequestable :: Requestable StarStatResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance starStatResponsesRespondable :: Respondable StarStatResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkStarStatResponses
      <$> readProp "star_stat_responses" json


instance starStatResponsesIsForeign :: IsForeign StarStatResponses where
  read json =
      mkStarStatResponses
      <$> readProp "star_stat_responses" json

-- footer