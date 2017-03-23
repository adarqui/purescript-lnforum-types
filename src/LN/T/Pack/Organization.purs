module LN.T.Pack.Organization where
import LN.T.Organization
import LN.T.User
import LN.T.Team
import LN.T.Like
import LN.T.Star
import LN.T.Permission


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

newtype OrganizationPackResponse = OrganizationPackResponse {
  user :: UserSanitizedResponse,
  userId :: Int,
  organization :: OrganizationResponse,
  organizationId :: Int,
  stat :: OrganizationStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  permissions :: Permissions,
  teams :: (Array SystemTeam)
}


type OrganizationPackResponseR = {
  user :: UserSanitizedResponse,
  userId :: Int,
  organization :: OrganizationResponse,
  organizationId :: Int,
  stat :: OrganizationStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  permissions :: Permissions,
  teams :: (Array SystemTeam)
}


_OrganizationPackResponse :: Lens' OrganizationPackResponse {
  user :: UserSanitizedResponse,
  userId :: Int,
  organization :: OrganizationResponse,
  organizationId :: Int,
  stat :: OrganizationStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  permissions :: Permissions,
  teams :: (Array SystemTeam)
}
_OrganizationPackResponse f (OrganizationPackResponse o) = OrganizationPackResponse <$> f o


mkOrganizationPackResponse :: UserSanitizedResponse -> Int -> OrganizationResponse -> Int -> OrganizationStatResponse -> (Maybe LikeResponse) -> (Maybe StarResponse) -> Permissions -> (Array SystemTeam) -> OrganizationPackResponse
mkOrganizationPackResponse user userId organization organizationId stat like star permissions teams =
  OrganizationPackResponse{user, userId, organization, organizationId, stat, like, star, permissions, teams}


unwrapOrganizationPackResponse :: OrganizationPackResponse -> {
  user :: UserSanitizedResponse,
  userId :: Int,
  organization :: OrganizationResponse,
  organizationId :: Int,
  stat :: OrganizationStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  permissions :: Permissions,
  teams :: (Array SystemTeam)
}
unwrapOrganizationPackResponse (OrganizationPackResponse r) = r

instance organizationPackResponseEncodeJson :: EncodeJson OrganizationPackResponse where
  encodeJson (OrganizationPackResponse o) =
       "tag" := "OrganizationPackResponse"
    ~> "user" := o.user
    ~> "user_id" := o.userId
    ~> "organization" := o.organization
    ~> "organization_id" := o.organizationId
    ~> "stat" := o.stat
    ~> "like" := o.like
    ~> "star" := o.star
    ~> "permissions" := o.permissions
    ~> "teams" := o.teams
    ~> jsonEmptyObject


instance organizationPackResponseDecodeJson :: DecodeJson OrganizationPackResponse where
  decodeJson o = do
    obj <- decodeJson o
    user <- obj .? "user"
    userId <- obj .? "user_id"
    organization <- obj .? "organization"
    organizationId <- obj .? "organization_id"
    stat <- obj .? "stat"
    like <- obj .? "like"
    star <- obj .? "star"
    permissions <- obj .? "permissions"
    teams <- obj .? "teams"
    pure $ OrganizationPackResponse {
      user,
      userId,
      organization,
      organizationId,
      stat,
      like,
      star,
      permissions,
      teams
    }


instance organizationPackResponseRequestable :: Requestable OrganizationPackResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance organizationPackResponseRespondable :: Respondable OrganizationPackResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkOrganizationPackResponse
      <$> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "organization" json
      <*> readProp "organization_id" json
      <*> readProp "stat" json
      <*> (unNullOrUndefined <$> readProp "like" json)
      <*> (unNullOrUndefined <$> readProp "star" json)
      <*> readProp "permissions" json
      <*> readProp "teams" json


instance organizationPackResponseIsForeign :: IsForeign OrganizationPackResponse where
  read json =
      mkOrganizationPackResponse
      <$> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "organization" json
      <*> readProp "organization_id" json
      <*> readProp "stat" json
      <*> (unNullOrUndefined <$> readProp "like" json)
      <*> (unNullOrUndefined <$> readProp "star" json)
      <*> readProp "permissions" json
      <*> readProp "teams" json


newtype OrganizationPackResponses = OrganizationPackResponses {
  organizationPackResponses :: (Array OrganizationPackResponse)
}


type OrganizationPackResponsesR = {
  organizationPackResponses :: (Array OrganizationPackResponse)
}


_OrganizationPackResponses :: Lens' OrganizationPackResponses {
  organizationPackResponses :: (Array OrganizationPackResponse)
}
_OrganizationPackResponses f (OrganizationPackResponses o) = OrganizationPackResponses <$> f o


mkOrganizationPackResponses :: (Array OrganizationPackResponse) -> OrganizationPackResponses
mkOrganizationPackResponses organizationPackResponses =
  OrganizationPackResponses{organizationPackResponses}


unwrapOrganizationPackResponses :: OrganizationPackResponses -> {
  organizationPackResponses :: (Array OrganizationPackResponse)
}
unwrapOrganizationPackResponses (OrganizationPackResponses r) = r

instance organizationPackResponsesEncodeJson :: EncodeJson OrganizationPackResponses where
  encodeJson (OrganizationPackResponses o) =
       "tag" := "OrganizationPackResponses"
    ~> "organization_pack_responses" := o.organizationPackResponses
    ~> jsonEmptyObject


instance organizationPackResponsesDecodeJson :: DecodeJson OrganizationPackResponses where
  decodeJson o = do
    obj <- decodeJson o
    organizationPackResponses <- obj .? "organization_pack_responses"
    pure $ OrganizationPackResponses {
      organizationPackResponses
    }


instance organizationPackResponsesRequestable :: Requestable OrganizationPackResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance organizationPackResponsesRespondable :: Respondable OrganizationPackResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkOrganizationPackResponses
      <$> readProp "organization_pack_responses" json


instance organizationPackResponsesIsForeign :: IsForeign OrganizationPackResponses where
  read json =
      mkOrganizationPackResponses
      <$> readProp "organization_pack_responses" json

-- footer