module LN.T.PmIn where



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

newtype PmInRequest = PmInRequest {
  label :: (Maybe String),
  isRead :: Boolean,
  isStarred :: Boolean,
  guard :: Int
}


type PmInRequestR = {
  label :: (Maybe String),
  isRead :: Boolean,
  isStarred :: Boolean,
  guard :: Int
}


_PmInRequest :: Lens' PmInRequest {
  label :: (Maybe String),
  isRead :: Boolean,
  isStarred :: Boolean,
  guard :: Int
}
_PmInRequest f (PmInRequest o) = PmInRequest <$> f o


mkPmInRequest :: (Maybe String) -> Boolean -> Boolean -> Int -> PmInRequest
mkPmInRequest label isRead isStarred guard =
  PmInRequest{label, isRead, isStarred, guard}


unwrapPmInRequest :: PmInRequest -> {
  label :: (Maybe String),
  isRead :: Boolean,
  isStarred :: Boolean,
  guard :: Int
}
unwrapPmInRequest (PmInRequest r) = r

instance pmInRequestEncodeJson :: EncodeJson PmInRequest where
  encodeJson (PmInRequest o) =
       "tag" := "PmInRequest"
    ~> "label" := o.label
    ~> "is_read" := o.isRead
    ~> "is_starred" := o.isStarred
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance pmInRequestDecodeJson :: DecodeJson PmInRequest where
  decodeJson o = do
    obj <- decodeJson o
    label <- obj .? "label"
    isRead <- obj .? "is_read"
    isStarred <- obj .? "is_starred"
    guard <- obj .? "guard"
    pure $ PmInRequest {
      label,
      isRead,
      isStarred,
      guard
    }


instance pmInRequestRequestable :: Requestable PmInRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance pmInRequestRespondable :: Respondable PmInRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkPmInRequest
      <$> (unNullOrUndefined <$> readProp "label" json)
      <*> readProp "is_read" json
      <*> readProp "is_starred" json
      <*> readProp "guard" json


instance pmInRequestIsForeign :: IsForeign PmInRequest where
  read json =
      mkPmInRequest
      <$> (unNullOrUndefined <$> readProp "label" json)
      <*> readProp "is_read" json
      <*> readProp "is_starred" json
      <*> readProp "guard" json


newtype PmInResponse = PmInResponse {
  id :: Int,
  pmId :: Int,
  userId :: Int,
  label :: (Maybe String),
  isRead :: Boolean,
  isStarred :: Boolean,
  isNew :: Boolean,
  isSaved :: Boolean,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}


type PmInResponseR = {
  id :: Int,
  pmId :: Int,
  userId :: Int,
  label :: (Maybe String),
  isRead :: Boolean,
  isStarred :: Boolean,
  isNew :: Boolean,
  isSaved :: Boolean,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}


_PmInResponse :: Lens' PmInResponse {
  id :: Int,
  pmId :: Int,
  userId :: Int,
  label :: (Maybe String),
  isRead :: Boolean,
  isStarred :: Boolean,
  isNew :: Boolean,
  isSaved :: Boolean,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}
_PmInResponse f (PmInResponse o) = PmInResponse <$> f o


mkPmInResponse :: Int -> Int -> Int -> (Maybe String) -> Boolean -> Boolean -> Boolean -> Boolean -> Boolean -> Int -> (Maybe Date) -> (Maybe Date) -> PmInResponse
mkPmInResponse id pmId userId label isRead isStarred isNew isSaved active guard createdAt modifiedAt =
  PmInResponse{id, pmId, userId, label, isRead, isStarred, isNew, isSaved, active, guard, createdAt, modifiedAt}


unwrapPmInResponse :: PmInResponse -> {
  id :: Int,
  pmId :: Int,
  userId :: Int,
  label :: (Maybe String),
  isRead :: Boolean,
  isStarred :: Boolean,
  isNew :: Boolean,
  isSaved :: Boolean,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}
unwrapPmInResponse (PmInResponse r) = r

instance pmInResponseEncodeJson :: EncodeJson PmInResponse where
  encodeJson (PmInResponse o) =
       "tag" := "PmInResponse"
    ~> "id" := o.id
    ~> "pm_id" := o.pmId
    ~> "user_id" := o.userId
    ~> "label" := o.label
    ~> "is_read" := o.isRead
    ~> "is_starred" := o.isStarred
    ~> "is_new" := o.isNew
    ~> "is_saved" := o.isSaved
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
    ~> jsonEmptyObject


instance pmInResponseDecodeJson :: DecodeJson PmInResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    pmId <- obj .? "pm_id"
    userId <- obj .? "user_id"
    label <- obj .? "label"
    isRead <- obj .? "is_read"
    isStarred <- obj .? "is_starred"
    isNew <- obj .? "is_new"
    isSaved <- obj .? "is_saved"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    pure $ PmInResponse {
      id,
      pmId,
      userId,
      label,
      isRead,
      isStarred,
      isNew,
      isSaved,
      active,
      guard,
      createdAt,
      modifiedAt
    }


instance pmInResponseRequestable :: Requestable PmInResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance pmInResponseRespondable :: Respondable PmInResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkPmInResponse
      <$> readProp "id" json
      <*> readProp "pm_id" json
      <*> readProp "user_id" json
      <*> (unNullOrUndefined <$> readProp "label" json)
      <*> readProp "is_read" json
      <*> readProp "is_starred" json
      <*> readProp "is_new" json
      <*> readProp "is_saved" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)


instance pmInResponseIsForeign :: IsForeign PmInResponse where
  read json =
      mkPmInResponse
      <$> readProp "id" json
      <*> readProp "pm_id" json
      <*> readProp "user_id" json
      <*> (unNullOrUndefined <$> readProp "label" json)
      <*> readProp "is_read" json
      <*> readProp "is_starred" json
      <*> readProp "is_new" json
      <*> readProp "is_saved" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)


newtype PmInResponses = PmInResponses {
  pmInResponses :: (Array PmInResponse)
}


type PmInResponsesR = {
  pmInResponses :: (Array PmInResponse)
}


_PmInResponses :: Lens' PmInResponses {
  pmInResponses :: (Array PmInResponse)
}
_PmInResponses f (PmInResponses o) = PmInResponses <$> f o


mkPmInResponses :: (Array PmInResponse) -> PmInResponses
mkPmInResponses pmInResponses =
  PmInResponses{pmInResponses}


unwrapPmInResponses :: PmInResponses -> {
  pmInResponses :: (Array PmInResponse)
}
unwrapPmInResponses (PmInResponses r) = r

instance pmInResponsesEncodeJson :: EncodeJson PmInResponses where
  encodeJson (PmInResponses o) =
       "tag" := "PmInResponses"
    ~> "pm_in_responses" := o.pmInResponses
    ~> jsonEmptyObject


instance pmInResponsesDecodeJson :: DecodeJson PmInResponses where
  decodeJson o = do
    obj <- decodeJson o
    pmInResponses <- obj .? "pm_in_responses"
    pure $ PmInResponses {
      pmInResponses
    }


instance pmInResponsesRequestable :: Requestable PmInResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance pmInResponsesRespondable :: Respondable PmInResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkPmInResponses
      <$> readProp "pm_in_responses" json


instance pmInResponsesIsForeign :: IsForeign PmInResponses where
  read json =
      mkPmInResponses
      <$> readProp "pm_in_responses" json

-- footer