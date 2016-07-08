module LN.T.Pack.Leuron where
import LN.T.Leuron
import LN.T.LeuronTraining
import LN.T.User
import LN.T.Permission
import LN.T.Star
import LN.T.Like


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

newtype LeuronPackResponse = LeuronPackResponse {
  leuron :: LeuronResponse,
  leuronId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int,
  training :: LeuronTrainingResponse,
  stat :: LeuronStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  permissions :: Permissions
}


type LeuronPackResponseR = {
  leuron :: LeuronResponse,
  leuronId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int,
  training :: LeuronTrainingResponse,
  stat :: LeuronStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  permissions :: Permissions
}


_LeuronPackResponse :: Lens' LeuronPackResponse {
  leuron :: LeuronResponse,
  leuronId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int,
  training :: LeuronTrainingResponse,
  stat :: LeuronStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  permissions :: Permissions
}
_LeuronPackResponse f (LeuronPackResponse o) = LeuronPackResponse <$> f o


mkLeuronPackResponse :: LeuronResponse -> Int -> UserSanitizedResponse -> Int -> LeuronTrainingResponse -> LeuronStatResponse -> (Maybe LikeResponse) -> (Maybe StarResponse) -> Permissions -> LeuronPackResponse
mkLeuronPackResponse leuron leuronId user userId training stat like star permissions =
  LeuronPackResponse{leuron, leuronId, user, userId, training, stat, like, star, permissions}


unwrapLeuronPackResponse :: LeuronPackResponse -> {
  leuron :: LeuronResponse,
  leuronId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int,
  training :: LeuronTrainingResponse,
  stat :: LeuronStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  permissions :: Permissions
}
unwrapLeuronPackResponse (LeuronPackResponse r) = r

instance leuronPackResponseEncodeJson :: EncodeJson LeuronPackResponse where
  encodeJson (LeuronPackResponse o) =
       "tag" := "LeuronPackResponse"
    ~> "leuron" := o.leuron
    ~> "leuron_id" := o.leuronId
    ~> "user" := o.user
    ~> "user_id" := o.userId
    ~> "training" := o.training
    ~> "stat" := o.stat
    ~> "like" := o.like
    ~> "star" := o.star
    ~> "permissions" := o.permissions
    ~> jsonEmptyObject


instance leuronPackResponseDecodeJson :: DecodeJson LeuronPackResponse where
  decodeJson o = do
    obj <- decodeJson o
    leuron <- obj .? "leuron"
    leuronId <- obj .? "leuron_id"
    user <- obj .? "user"
    userId <- obj .? "user_id"
    training <- obj .? "training"
    stat <- obj .? "stat"
    like <- obj .? "like"
    star <- obj .? "star"
    permissions <- obj .? "permissions"
    pure $ LeuronPackResponse {
      leuron,
      leuronId,
      user,
      userId,
      training,
      stat,
      like,
      star,
      permissions
    }


instance leuronPackResponseRequestable :: Requestable LeuronPackResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance leuronPackResponseRespondable :: Respondable LeuronPackResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkLeuronPackResponse
      <$> readProp "leuron" json
      <*> readProp "leuron_id" json
      <*> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "training" json
      <*> readProp "stat" json
      <*> (unNullOrUndefined <$> readProp "like" json)
      <*> (unNullOrUndefined <$> readProp "star" json)
      <*> readProp "permissions" json


instance leuronPackResponseIsForeign :: IsForeign LeuronPackResponse where
  read json =
      mkLeuronPackResponse
      <$> readProp "leuron" json
      <*> readProp "leuron_id" json
      <*> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "training" json
      <*> readProp "stat" json
      <*> (unNullOrUndefined <$> readProp "like" json)
      <*> (unNullOrUndefined <$> readProp "star" json)
      <*> readProp "permissions" json


newtype LeuronPackResponses = LeuronPackResponses {
  leuronPackResponses :: (Array LeuronPackResponse)
}


type LeuronPackResponsesR = {
  leuronPackResponses :: (Array LeuronPackResponse)
}


_LeuronPackResponses :: Lens' LeuronPackResponses {
  leuronPackResponses :: (Array LeuronPackResponse)
}
_LeuronPackResponses f (LeuronPackResponses o) = LeuronPackResponses <$> f o


mkLeuronPackResponses :: (Array LeuronPackResponse) -> LeuronPackResponses
mkLeuronPackResponses leuronPackResponses =
  LeuronPackResponses{leuronPackResponses}


unwrapLeuronPackResponses :: LeuronPackResponses -> {
  leuronPackResponses :: (Array LeuronPackResponse)
}
unwrapLeuronPackResponses (LeuronPackResponses r) = r

instance leuronPackResponsesEncodeJson :: EncodeJson LeuronPackResponses where
  encodeJson (LeuronPackResponses o) =
       "tag" := "LeuronPackResponses"
    ~> "leuron_pack_responses" := o.leuronPackResponses
    ~> jsonEmptyObject


instance leuronPackResponsesDecodeJson :: DecodeJson LeuronPackResponses where
  decodeJson o = do
    obj <- decodeJson o
    leuronPackResponses <- obj .? "leuron_pack_responses"
    pure $ LeuronPackResponses {
      leuronPackResponses
    }


instance leuronPackResponsesRequestable :: Requestable LeuronPackResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance leuronPackResponsesRespondable :: Respondable LeuronPackResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkLeuronPackResponses
      <$> readProp "leuron_pack_responses" json


instance leuronPackResponsesIsForeign :: IsForeign LeuronPackResponses where
  read json =
      mkLeuronPackResponses
      <$> readProp "leuron_pack_responses" json

-- footer