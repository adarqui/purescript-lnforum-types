module LN.T.PmOut where



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

newtype PmOutRequest = PmOutRequest {
  label :: (Maybe String),
  guard :: Int
}


type PmOutRequestR = {
  label :: (Maybe String),
  guard :: Int
}


_PmOutRequest :: Lens' PmOutRequest {
  label :: (Maybe String),
  guard :: Int
}
_PmOutRequest f (PmOutRequest o) = PmOutRequest <$> f o


mkPmOutRequest :: (Maybe String) -> Int -> PmOutRequest
mkPmOutRequest label guard =
  PmOutRequest{label, guard}


unwrapPmOutRequest :: PmOutRequest -> {
  label :: (Maybe String),
  guard :: Int
}
unwrapPmOutRequest (PmOutRequest r) = r

instance pmOutRequestEncodeJson :: EncodeJson PmOutRequest where
  encodeJson (PmOutRequest o) =
       "tag" := "PmOutRequest"
    ~> "label" := o.label
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance pmOutRequestDecodeJson :: DecodeJson PmOutRequest where
  decodeJson o = do
    obj <- decodeJson o
    label <- obj .? "label"
    guard <- obj .? "guard"
    pure $ PmOutRequest {
      label,
      guard
    }


instance pmOutRequestRequestable :: Requestable PmOutRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance pmOutRequestRespondable :: Respondable PmOutRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkPmOutRequest
      <$> (unNullOrUndefined <$> readProp "label" json)
      <*> readProp "guard" json


instance pmOutRequestIsForeign :: IsForeign PmOutRequest where
  read json =
      mkPmOutRequest
      <$> (unNullOrUndefined <$> readProp "label" json)
      <*> readProp "guard" json


newtype PmOutResponse = PmOutResponse {
  id :: Int,
  pmId :: Int,
  userId :: Int,
  label :: (Maybe String),
  isSaved :: Boolean,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}


type PmOutResponseR = {
  id :: Int,
  pmId :: Int,
  userId :: Int,
  label :: (Maybe String),
  isSaved :: Boolean,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}


_PmOutResponse :: Lens' PmOutResponse {
  id :: Int,
  pmId :: Int,
  userId :: Int,
  label :: (Maybe String),
  isSaved :: Boolean,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}
_PmOutResponse f (PmOutResponse o) = PmOutResponse <$> f o


mkPmOutResponse :: Int -> Int -> Int -> (Maybe String) -> Boolean -> Boolean -> Int -> (Maybe Date) -> (Maybe Date) -> PmOutResponse
mkPmOutResponse id pmId userId label isSaved active guard createdAt modifiedAt =
  PmOutResponse{id, pmId, userId, label, isSaved, active, guard, createdAt, modifiedAt}


unwrapPmOutResponse :: PmOutResponse -> {
  id :: Int,
  pmId :: Int,
  userId :: Int,
  label :: (Maybe String),
  isSaved :: Boolean,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}
unwrapPmOutResponse (PmOutResponse r) = r

instance pmOutResponseEncodeJson :: EncodeJson PmOutResponse where
  encodeJson (PmOutResponse o) =
       "tag" := "PmOutResponse"
    ~> "id" := o.id
    ~> "pm_id" := o.pmId
    ~> "user_id" := o.userId
    ~> "label" := o.label
    ~> "is_saved" := o.isSaved
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
    ~> jsonEmptyObject


instance pmOutResponseDecodeJson :: DecodeJson PmOutResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    pmId <- obj .? "pm_id"
    userId <- obj .? "user_id"
    label <- obj .? "label"
    isSaved <- obj .? "is_saved"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    pure $ PmOutResponse {
      id,
      pmId,
      userId,
      label,
      isSaved,
      active,
      guard,
      createdAt,
      modifiedAt
    }


instance pmOutResponseRequestable :: Requestable PmOutResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance pmOutResponseRespondable :: Respondable PmOutResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkPmOutResponse
      <$> readProp "id" json
      <*> readProp "pm_id" json
      <*> readProp "user_id" json
      <*> (unNullOrUndefined <$> readProp "label" json)
      <*> readProp "is_saved" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)


instance pmOutResponseIsForeign :: IsForeign PmOutResponse where
  read json =
      mkPmOutResponse
      <$> readProp "id" json
      <*> readProp "pm_id" json
      <*> readProp "user_id" json
      <*> (unNullOrUndefined <$> readProp "label" json)
      <*> readProp "is_saved" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)


newtype PmOutResponses = PmOutResponses {
  pmOutResponses :: (Array PmOutResponse)
}


type PmOutResponsesR = {
  pmOutResponses :: (Array PmOutResponse)
}


_PmOutResponses :: Lens' PmOutResponses {
  pmOutResponses :: (Array PmOutResponse)
}
_PmOutResponses f (PmOutResponses o) = PmOutResponses <$> f o


mkPmOutResponses :: (Array PmOutResponse) -> PmOutResponses
mkPmOutResponses pmOutResponses =
  PmOutResponses{pmOutResponses}


unwrapPmOutResponses :: PmOutResponses -> {
  pmOutResponses :: (Array PmOutResponse)
}
unwrapPmOutResponses (PmOutResponses r) = r

instance pmOutResponsesEncodeJson :: EncodeJson PmOutResponses where
  encodeJson (PmOutResponses o) =
       "tag" := "PmOutResponses"
    ~> "pm_out_responses" := o.pmOutResponses
    ~> jsonEmptyObject


instance pmOutResponsesDecodeJson :: DecodeJson PmOutResponses where
  decodeJson o = do
    obj <- decodeJson o
    pmOutResponses <- obj .? "pm_out_responses"
    pure $ PmOutResponses {
      pmOutResponses
    }


instance pmOutResponsesRequestable :: Requestable PmOutResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance pmOutResponsesRespondable :: Respondable PmOutResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkPmOutResponses
      <$> readProp "pm_out_responses" json


instance pmOutResponsesIsForeign :: IsForeign PmOutResponses where
  read json =
      mkPmOutResponses
      <$> readProp "pm_out_responses" json

-- footer