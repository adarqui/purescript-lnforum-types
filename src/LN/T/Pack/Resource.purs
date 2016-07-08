module LN.T.Pack.Resource where
import LN.T.Resource
import LN.T.User
import LN.T.Permission
import LN.T.Like
import LN.T.Star


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

newtype ResourcePackResponse = ResourcePackResponse {
  resource :: ResourceResponse,
  resourceId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int,
  stat :: ResourceStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  permissions :: Permissions
}


type ResourcePackResponseR = {
  resource :: ResourceResponse,
  resourceId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int,
  stat :: ResourceStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  permissions :: Permissions
}


_ResourcePackResponse :: Lens' ResourcePackResponse {
  resource :: ResourceResponse,
  resourceId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int,
  stat :: ResourceStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  permissions :: Permissions
}
_ResourcePackResponse f (ResourcePackResponse o) = ResourcePackResponse <$> f o


mkResourcePackResponse :: ResourceResponse -> Int -> UserSanitizedResponse -> Int -> ResourceStatResponse -> (Maybe LikeResponse) -> (Maybe StarResponse) -> Permissions -> ResourcePackResponse
mkResourcePackResponse resource resourceId user userId stat like star permissions =
  ResourcePackResponse{resource, resourceId, user, userId, stat, like, star, permissions}


unwrapResourcePackResponse :: ResourcePackResponse -> {
  resource :: ResourceResponse,
  resourceId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int,
  stat :: ResourceStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  permissions :: Permissions
}
unwrapResourcePackResponse (ResourcePackResponse r) = r

instance resourcePackResponseEncodeJson :: EncodeJson ResourcePackResponse where
  encodeJson (ResourcePackResponse o) =
       "tag" := "ResourcePackResponse"
    ~> "resource" := o.resource
    ~> "resource_id" := o.resourceId
    ~> "user" := o.user
    ~> "user_id" := o.userId
    ~> "stat" := o.stat
    ~> "like" := o.like
    ~> "star" := o.star
    ~> "permissions" := o.permissions
    ~> jsonEmptyObject


instance resourcePackResponseDecodeJson :: DecodeJson ResourcePackResponse where
  decodeJson o = do
    obj <- decodeJson o
    resource <- obj .? "resource"
    resourceId <- obj .? "resource_id"
    user <- obj .? "user"
    userId <- obj .? "user_id"
    stat <- obj .? "stat"
    like <- obj .? "like"
    star <- obj .? "star"
    permissions <- obj .? "permissions"
    pure $ ResourcePackResponse {
      resource,
      resourceId,
      user,
      userId,
      stat,
      like,
      star,
      permissions
    }


instance resourcePackResponseRequestable :: Requestable ResourcePackResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance resourcePackResponseRespondable :: Respondable ResourcePackResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkResourcePackResponse
      <$> readProp "resource" json
      <*> readProp "resource_id" json
      <*> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "stat" json
      <*> (unNullOrUndefined <$> readProp "like" json)
      <*> (unNullOrUndefined <$> readProp "star" json)
      <*> readProp "permissions" json


instance resourcePackResponseIsForeign :: IsForeign ResourcePackResponse where
  read json =
      mkResourcePackResponse
      <$> readProp "resource" json
      <*> readProp "resource_id" json
      <*> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "stat" json
      <*> (unNullOrUndefined <$> readProp "like" json)
      <*> (unNullOrUndefined <$> readProp "star" json)
      <*> readProp "permissions" json


newtype ResourcePackResponses = ResourcePackResponses {
  resourcePackResponses :: (Array ResourcePackResponse)
}


type ResourcePackResponsesR = {
  resourcePackResponses :: (Array ResourcePackResponse)
}


_ResourcePackResponses :: Lens' ResourcePackResponses {
  resourcePackResponses :: (Array ResourcePackResponse)
}
_ResourcePackResponses f (ResourcePackResponses o) = ResourcePackResponses <$> f o


mkResourcePackResponses :: (Array ResourcePackResponse) -> ResourcePackResponses
mkResourcePackResponses resourcePackResponses =
  ResourcePackResponses{resourcePackResponses}


unwrapResourcePackResponses :: ResourcePackResponses -> {
  resourcePackResponses :: (Array ResourcePackResponse)
}
unwrapResourcePackResponses (ResourcePackResponses r) = r

instance resourcePackResponsesEncodeJson :: EncodeJson ResourcePackResponses where
  encodeJson (ResourcePackResponses o) =
       "tag" := "ResourcePackResponses"
    ~> "resource_pack_responses" := o.resourcePackResponses
    ~> jsonEmptyObject


instance resourcePackResponsesDecodeJson :: DecodeJson ResourcePackResponses where
  decodeJson o = do
    obj <- decodeJson o
    resourcePackResponses <- obj .? "resource_pack_responses"
    pure $ ResourcePackResponses {
      resourcePackResponses
    }


instance resourcePackResponsesRequestable :: Requestable ResourcePackResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance resourcePackResponsesRespondable :: Respondable ResourcePackResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkResourcePackResponses
      <$> readProp "resource_pack_responses" json


instance resourcePackResponsesIsForeign :: IsForeign ResourcePackResponses where
  read json =
      mkResourcePackResponses
      <$> readProp "resource_pack_responses" json

-- footer