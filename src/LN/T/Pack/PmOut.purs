module LN.T.Pack.PmOut where
import LN.T.PmOut
import LN.T.User


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

newtype PmOutPackResponse = PmOutPackResponse {
  pmOut :: PmOutResponse,
  pmOutId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int
}


type PmOutPackResponseR = {
  pmOut :: PmOutResponse,
  pmOutId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int
}


_PmOutPackResponse :: Lens' PmOutPackResponse {
  pmOut :: PmOutResponse,
  pmOutId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int
}
_PmOutPackResponse f (PmOutPackResponse o) = PmOutPackResponse <$> f o


mkPmOutPackResponse :: PmOutResponse -> Int -> UserSanitizedResponse -> Int -> PmOutPackResponse
mkPmOutPackResponse pmOut pmOutId user userId =
  PmOutPackResponse{pmOut, pmOutId, user, userId}


unwrapPmOutPackResponse :: PmOutPackResponse -> {
  pmOut :: PmOutResponse,
  pmOutId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int
}
unwrapPmOutPackResponse (PmOutPackResponse r) = r

instance pmOutPackResponseEncodeJson :: EncodeJson PmOutPackResponse where
  encodeJson (PmOutPackResponse o) =
       "tag" := "PmOutPackResponse"
    ~> "pm_out" := o.pmOut
    ~> "pm_out_id" := o.pmOutId
    ~> "user" := o.user
    ~> "user_id" := o.userId
    ~> jsonEmptyObject


instance pmOutPackResponseDecodeJson :: DecodeJson PmOutPackResponse where
  decodeJson o = do
    obj <- decodeJson o
    pmOut <- obj .? "pm_out"
    pmOutId <- obj .? "pm_out_id"
    user <- obj .? "user"
    userId <- obj .? "user_id"
    pure $ PmOutPackResponse {
      pmOut,
      pmOutId,
      user,
      userId
    }


instance pmOutPackResponseRequestable :: Requestable PmOutPackResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance pmOutPackResponseRespondable :: Respondable PmOutPackResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkPmOutPackResponse
      <$> readProp "pm_out" json
      <*> readProp "pm_out_id" json
      <*> readProp "user" json
      <*> readProp "user_id" json


instance pmOutPackResponseIsForeign :: IsForeign PmOutPackResponse where
  read json =
      mkPmOutPackResponse
      <$> readProp "pm_out" json
      <*> readProp "pm_out_id" json
      <*> readProp "user" json
      <*> readProp "user_id" json


newtype PmOutPackResponses = PmOutPackResponses {
  pmOutPackResponses :: (Array PmOutPackResponse)
}


type PmOutPackResponsesR = {
  pmOutPackResponses :: (Array PmOutPackResponse)
}


_PmOutPackResponses :: Lens' PmOutPackResponses {
  pmOutPackResponses :: (Array PmOutPackResponse)
}
_PmOutPackResponses f (PmOutPackResponses o) = PmOutPackResponses <$> f o


mkPmOutPackResponses :: (Array PmOutPackResponse) -> PmOutPackResponses
mkPmOutPackResponses pmOutPackResponses =
  PmOutPackResponses{pmOutPackResponses}


unwrapPmOutPackResponses :: PmOutPackResponses -> {
  pmOutPackResponses :: (Array PmOutPackResponse)
}
unwrapPmOutPackResponses (PmOutPackResponses r) = r

instance pmOutPackResponsesEncodeJson :: EncodeJson PmOutPackResponses where
  encodeJson (PmOutPackResponses o) =
       "tag" := "PmOutPackResponses"
    ~> "pm_out_pack_responses" := o.pmOutPackResponses
    ~> jsonEmptyObject


instance pmOutPackResponsesDecodeJson :: DecodeJson PmOutPackResponses where
  decodeJson o = do
    obj <- decodeJson o
    pmOutPackResponses <- obj .? "pm_out_pack_responses"
    pure $ PmOutPackResponses {
      pmOutPackResponses
    }


instance pmOutPackResponsesRequestable :: Requestable PmOutPackResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance pmOutPackResponsesRespondable :: Respondable PmOutPackResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkPmOutPackResponses
      <$> readProp "pm_out_pack_responses" json


instance pmOutPackResponsesIsForeign :: IsForeign PmOutPackResponses where
  read json =
      mkPmOutPackResponses
      <$> readProp "pm_out_pack_responses" json

-- footer