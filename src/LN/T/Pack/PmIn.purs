module LN.T.Pack.PmIn where


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

newtype PmInPackResponse = PmInPackResponse {
  pmIn :: PmInResponse,
  pmInId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int
}


type PmInPackResponseR = {
  pmIn :: PmInResponse,
  pmInId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int
}


_PmInPackResponse :: Lens' PmInPackResponse {
  pmIn :: PmInResponse,
  pmInId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int
}
_PmInPackResponse f (PmInPackResponse o) = PmInPackResponse <$> f o


mkPmInPackResponse :: PmInResponse -> Int -> UserSanitizedResponse -> Int -> PmInPackResponse
mkPmInPackResponse pmIn pmInId user userId =
  PmInPackResponse{pmIn, pmInId, user, userId}


unwrapPmInPackResponse :: PmInPackResponse -> {
  pmIn :: PmInResponse,
  pmInId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int
}
unwrapPmInPackResponse (PmInPackResponse r) = r

instance pmInPackResponseEncodeJson :: EncodeJson PmInPackResponse where
  encodeJson (PmInPackResponse o) =
       "tag" := "PmInPackResponse"
    ~> "pm_in" := o.pmIn
    ~> "pm_in_id" := o.pmInId
    ~> "user" := o.user
    ~> "user_id" := o.userId
    ~> jsonEmptyObject


instance pmInPackResponseDecodeJson :: DecodeJson PmInPackResponse where
  decodeJson o = do
    obj <- decodeJson o
    pmIn <- obj .? "pm_in"
    pmInId <- obj .? "pm_in_id"
    user <- obj .? "user"
    userId <- obj .? "user_id"
    pure $ PmInPackResponse {
      pmIn,
      pmInId,
      user,
      userId
    }


instance pmInPackResponseRequestable :: Requestable PmInPackResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance pmInPackResponseRespondable :: Respondable PmInPackResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkPmInPackResponse
      <$> readProp "pm_in" json
      <*> readProp "pm_in_id" json
      <*> readProp "user" json
      <*> readProp "user_id" json


instance pmInPackResponseIsForeign :: IsForeign PmInPackResponse where
  read json =
      mkPmInPackResponse
      <$> readProp "pm_in" json
      <*> readProp "pm_in_id" json
      <*> readProp "user" json
      <*> readProp "user_id" json


newtype PmInPackResponses = PmInPackResponses {
  pmInPackResponses :: (Array PmInPackResponse)
}


type PmInPackResponsesR = {
  pmInPackResponses :: (Array PmInPackResponse)
}


_PmInPackResponses :: Lens' PmInPackResponses {
  pmInPackResponses :: (Array PmInPackResponse)
}
_PmInPackResponses f (PmInPackResponses o) = PmInPackResponses <$> f o


mkPmInPackResponses :: (Array PmInPackResponse) -> PmInPackResponses
mkPmInPackResponses pmInPackResponses =
  PmInPackResponses{pmInPackResponses}


unwrapPmInPackResponses :: PmInPackResponses -> {
  pmInPackResponses :: (Array PmInPackResponse)
}
unwrapPmInPackResponses (PmInPackResponses r) = r

instance pmInPackResponsesEncodeJson :: EncodeJson PmInPackResponses where
  encodeJson (PmInPackResponses o) =
       "tag" := "PmInPackResponses"
    ~> "pm_in_pack_responses" := o.pmInPackResponses
    ~> jsonEmptyObject


instance pmInPackResponsesDecodeJson :: DecodeJson PmInPackResponses where
  decodeJson o = do
    obj <- decodeJson o
    pmInPackResponses <- obj .? "pm_in_pack_responses"
    pure $ PmInPackResponses {
      pmInPackResponses
    }


instance pmInPackResponsesRequestable :: Requestable PmInPackResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance pmInPackResponsesRespondable :: Respondable PmInPackResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkPmInPackResponses
      <$> readProp "pm_in_pack_responses" json


instance pmInPackResponsesIsForeign :: IsForeign PmInPackResponses where
  read json =
      mkPmInPackResponses
      <$> readProp "pm_in_pack_responses" json

-- footer