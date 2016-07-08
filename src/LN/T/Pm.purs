module LN.T.Pm where



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
import Data.Default

import Purescript.Api.Helpers

newtype PmRequest = PmRequest {
  subject :: String,
  body :: String,
  guard :: Int
}


type PmRequestR = {
  subject :: String,
  body :: String,
  guard :: Int
}


_PmRequest :: Lens' PmRequest {
  subject :: String,
  body :: String,
  guard :: Int
}
_PmRequest f (PmRequest o) = PmRequest <$> f o


mkPmRequest :: String -> String -> Int -> PmRequest
mkPmRequest subject body guard =
  PmRequest{subject, body, guard}


unwrapPmRequest :: PmRequest -> {
  subject :: String,
  body :: String,
  guard :: Int
}
unwrapPmRequest (PmRequest r) = r

instance pmRequestEncodeJson :: EncodeJson PmRequest where
  encodeJson (PmRequest o) =
       "tag" := "PmRequest"
    ~> "subject" := o.subject
    ~> "body" := o.body
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance pmRequestDecodeJson :: DecodeJson PmRequest where
  decodeJson o = do
    obj <- decodeJson o
    subject <- obj .? "subject"
    body <- obj .? "body"
    guard <- obj .? "guard"
    pure $ PmRequest {
      subject,
      body,
      guard
    }


instance pmRequestRequestable :: Requestable PmRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance pmRequestRespondable :: Respondable PmRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkPmRequest
      <$> readProp "subject" json
      <*> readProp "body" json
      <*> readProp "guard" json


instance pmRequestIsForeign :: IsForeign PmRequest where
  read json =
      mkPmRequest
      <$> readProp "subject" json
      <*> readProp "body" json
      <*> readProp "guard" json


newtype PmResponse = PmResponse {
  id :: Int,
  userId :: Int,
  toUserId :: Int,
  subject :: String,
  body :: String,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


type PmResponseR = {
  id :: Int,
  userId :: Int,
  toUserId :: Int,
  subject :: String,
  body :: String,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


_PmResponse :: Lens' PmResponse {
  id :: Int,
  userId :: Int,
  toUserId :: Int,
  subject :: String,
  body :: String,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
_PmResponse f (PmResponse o) = PmResponse <$> f o


mkPmResponse :: Int -> Int -> Int -> String -> String -> Boolean -> Int -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> PmResponse
mkPmResponse id userId toUserId subject body active guard createdAt modifiedAt activityAt =
  PmResponse{id, userId, toUserId, subject, body, active, guard, createdAt, modifiedAt, activityAt}


unwrapPmResponse :: PmResponse -> {
  id :: Int,
  userId :: Int,
  toUserId :: Int,
  subject :: String,
  body :: String,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
unwrapPmResponse (PmResponse r) = r

instance pmResponseEncodeJson :: EncodeJson PmResponse where
  encodeJson (PmResponse o) =
       "tag" := "PmResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "to_user_id" := o.toUserId
    ~> "subject" := o.subject
    ~> "body" := o.body
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
    ~> "activity_at" := o.activityAt
    ~> jsonEmptyObject


instance pmResponseDecodeJson :: DecodeJson PmResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    toUserId <- obj .? "to_user_id"
    subject <- obj .? "subject"
    body <- obj .? "body"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    activityAt <- obj .? "activity_at"
    pure $ PmResponse {
      id,
      userId,
      toUserId,
      subject,
      body,
      active,
      guard,
      createdAt,
      modifiedAt,
      activityAt
    }


instance pmResponseRequestable :: Requestable PmResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance pmResponseRespondable :: Respondable PmResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkPmResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "to_user_id" json
      <*> readProp "subject" json
      <*> readProp "body" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


instance pmResponseIsForeign :: IsForeign PmResponse where
  read json =
      mkPmResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "to_user_id" json
      <*> readProp "subject" json
      <*> readProp "body" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


newtype PmResponses = PmResponses {
  pmResponses :: (Array PmResponse)
}


type PmResponsesR = {
  pmResponses :: (Array PmResponse)
}


_PmResponses :: Lens' PmResponses {
  pmResponses :: (Array PmResponse)
}
_PmResponses f (PmResponses o) = PmResponses <$> f o


mkPmResponses :: (Array PmResponse) -> PmResponses
mkPmResponses pmResponses =
  PmResponses{pmResponses}


unwrapPmResponses :: PmResponses -> {
  pmResponses :: (Array PmResponse)
}
unwrapPmResponses (PmResponses r) = r

instance pmResponsesEncodeJson :: EncodeJson PmResponses where
  encodeJson (PmResponses o) =
       "tag" := "PmResponses"
    ~> "pm_responses" := o.pmResponses
    ~> jsonEmptyObject


instance pmResponsesDecodeJson :: DecodeJson PmResponses where
  decodeJson o = do
    obj <- decodeJson o
    pmResponses <- obj .? "pm_responses"
    pure $ PmResponses {
      pmResponses
    }


instance pmResponsesRequestable :: Requestable PmResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance pmResponsesRespondable :: Respondable PmResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkPmResponses
      <$> readProp "pm_responses" json


instance pmResponsesIsForeign :: IsForeign PmResponses where
  read json =
      mkPmResponses
      <$> readProp "pm_responses" json

-- footer