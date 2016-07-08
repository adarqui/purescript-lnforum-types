module LN.T.Pack.Group where
import LN.T.Group
import LN.T.User
import LN.T.Permission
import LN.T.Organization


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

newtype GroupPackResponse = GroupPackResponse {
  user :: UserSanitizedResponse,
  userId :: Int,
  group :: GroupResponse,
  groupId :: Int,
  organization :: OrganizationResponse,
  organizationId :: Int,
  stat :: GroupStatResponse,
  permissions :: Permissions
}


type GroupPackResponseR = {
  user :: UserSanitizedResponse,
  userId :: Int,
  group :: GroupResponse,
  groupId :: Int,
  organization :: OrganizationResponse,
  organizationId :: Int,
  stat :: GroupStatResponse,
  permissions :: Permissions
}


_GroupPackResponse :: Lens' GroupPackResponse {
  user :: UserSanitizedResponse,
  userId :: Int,
  group :: GroupResponse,
  groupId :: Int,
  organization :: OrganizationResponse,
  organizationId :: Int,
  stat :: GroupStatResponse,
  permissions :: Permissions
}
_GroupPackResponse f (GroupPackResponse o) = GroupPackResponse <$> f o


mkGroupPackResponse :: UserSanitizedResponse -> Int -> GroupResponse -> Int -> OrganizationResponse -> Int -> GroupStatResponse -> Permissions -> GroupPackResponse
mkGroupPackResponse user userId group groupId organization organizationId stat permissions =
  GroupPackResponse{user, userId, group, groupId, organization, organizationId, stat, permissions}


unwrapGroupPackResponse :: GroupPackResponse -> {
  user :: UserSanitizedResponse,
  userId :: Int,
  group :: GroupResponse,
  groupId :: Int,
  organization :: OrganizationResponse,
  organizationId :: Int,
  stat :: GroupStatResponse,
  permissions :: Permissions
}
unwrapGroupPackResponse (GroupPackResponse r) = r

instance groupPackResponseEncodeJson :: EncodeJson GroupPackResponse where
  encodeJson (GroupPackResponse o) =
       "tag" := "GroupPackResponse"
    ~> "user" := o.user
    ~> "user_id" := o.userId
    ~> "group" := o.group
    ~> "group_id" := o.groupId
    ~> "organization" := o.organization
    ~> "organization_id" := o.organizationId
    ~> "stat" := o.stat
    ~> "permissions" := o.permissions
    ~> jsonEmptyObject


instance groupPackResponseDecodeJson :: DecodeJson GroupPackResponse where
  decodeJson o = do
    obj <- decodeJson o
    user <- obj .? "user"
    userId <- obj .? "user_id"
    group <- obj .? "group"
    groupId <- obj .? "group_id"
    organization <- obj .? "organization"
    organizationId <- obj .? "organization_id"
    stat <- obj .? "stat"
    permissions <- obj .? "permissions"
    pure $ GroupPackResponse {
      user,
      userId,
      group,
      groupId,
      organization,
      organizationId,
      stat,
      permissions
    }


instance groupPackResponseRequestable :: Requestable GroupPackResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance groupPackResponseRespondable :: Respondable GroupPackResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkGroupPackResponse
      <$> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "group" json
      <*> readProp "group_id" json
      <*> readProp "organization" json
      <*> readProp "organization_id" json
      <*> readProp "stat" json
      <*> readProp "permissions" json


instance groupPackResponseIsForeign :: IsForeign GroupPackResponse where
  read json =
      mkGroupPackResponse
      <$> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "group" json
      <*> readProp "group_id" json
      <*> readProp "organization" json
      <*> readProp "organization_id" json
      <*> readProp "stat" json
      <*> readProp "permissions" json


newtype GroupPackResponses = GroupPackResponses {
  groupPackResponses :: (Array GroupPackResponse)
}


type GroupPackResponsesR = {
  groupPackResponses :: (Array GroupPackResponse)
}


_GroupPackResponses :: Lens' GroupPackResponses {
  groupPackResponses :: (Array GroupPackResponse)
}
_GroupPackResponses f (GroupPackResponses o) = GroupPackResponses <$> f o


mkGroupPackResponses :: (Array GroupPackResponse) -> GroupPackResponses
mkGroupPackResponses groupPackResponses =
  GroupPackResponses{groupPackResponses}


unwrapGroupPackResponses :: GroupPackResponses -> {
  groupPackResponses :: (Array GroupPackResponse)
}
unwrapGroupPackResponses (GroupPackResponses r) = r

instance groupPackResponsesEncodeJson :: EncodeJson GroupPackResponses where
  encodeJson (GroupPackResponses o) =
       "tag" := "GroupPackResponses"
    ~> "group_pack_responses" := o.groupPackResponses
    ~> jsonEmptyObject


instance groupPackResponsesDecodeJson :: DecodeJson GroupPackResponses where
  decodeJson o = do
    obj <- decodeJson o
    groupPackResponses <- obj .? "group_pack_responses"
    pure $ GroupPackResponses {
      groupPackResponses
    }


instance groupPackResponsesRequestable :: Requestable GroupPackResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance groupPackResponsesRespondable :: Respondable GroupPackResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkGroupPackResponses
      <$> readProp "group_pack_responses" json


instance groupPackResponsesIsForeign :: IsForeign GroupPackResponses where
  read json =
      mkGroupPackResponses
      <$> readProp "group_pack_responses" json

-- footer