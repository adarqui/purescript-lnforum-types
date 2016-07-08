module LN.T.Pack.TeamMember where


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

newtype TeamMemberPackResponse = TeamMemberPackResponse {
  user :: UserSanitizedResponse,
  userId :: Int,
  teamMember :: TeamMemberResponse,
  teamMemberId :: Int,
  permissions :: Permissions
}


type TeamMemberPackResponseR = {
  user :: UserSanitizedResponse,
  userId :: Int,
  teamMember :: TeamMemberResponse,
  teamMemberId :: Int,
  permissions :: Permissions
}


_TeamMemberPackResponse :: Lens' TeamMemberPackResponse {
  user :: UserSanitizedResponse,
  userId :: Int,
  teamMember :: TeamMemberResponse,
  teamMemberId :: Int,
  permissions :: Permissions
}
_TeamMemberPackResponse f (TeamMemberPackResponse o) = TeamMemberPackResponse <$> f o


mkTeamMemberPackResponse :: UserSanitizedResponse -> Int -> TeamMemberResponse -> Int -> Permissions -> TeamMemberPackResponse
mkTeamMemberPackResponse user userId teamMember teamMemberId permissions =
  TeamMemberPackResponse{user, userId, teamMember, teamMemberId, permissions}


unwrapTeamMemberPackResponse :: TeamMemberPackResponse -> {
  user :: UserSanitizedResponse,
  userId :: Int,
  teamMember :: TeamMemberResponse,
  teamMemberId :: Int,
  permissions :: Permissions
}
unwrapTeamMemberPackResponse (TeamMemberPackResponse r) = r

instance teamMemberPackResponseEncodeJson :: EncodeJson TeamMemberPackResponse where
  encodeJson (TeamMemberPackResponse o) =
       "tag" := "TeamMemberPackResponse"
    ~> "user" := o.user
    ~> "user_id" := o.userId
    ~> "team_member" := o.teamMember
    ~> "team_member_id" := o.teamMemberId
    ~> "permissions" := o.permissions
    ~> jsonEmptyObject


instance teamMemberPackResponseDecodeJson :: DecodeJson TeamMemberPackResponse where
  decodeJson o = do
    obj <- decodeJson o
    user <- obj .? "user"
    userId <- obj .? "user_id"
    teamMember <- obj .? "team_member"
    teamMemberId <- obj .? "team_member_id"
    permissions <- obj .? "permissions"
    pure $ TeamMemberPackResponse {
      user,
      userId,
      teamMember,
      teamMemberId,
      permissions
    }


instance teamMemberPackResponseRequestable :: Requestable TeamMemberPackResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance teamMemberPackResponseRespondable :: Respondable TeamMemberPackResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkTeamMemberPackResponse
      <$> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "team_member" json
      <*> readProp "team_member_id" json
      <*> readProp "permissions" json


instance teamMemberPackResponseIsForeign :: IsForeign TeamMemberPackResponse where
  read json =
      mkTeamMemberPackResponse
      <$> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "team_member" json
      <*> readProp "team_member_id" json
      <*> readProp "permissions" json


newtype TeamMemberPackResponses = TeamMemberPackResponses {
  teamMemberPackResponses :: (Array TeamMemberPackResponse)
}


type TeamMemberPackResponsesR = {
  teamMemberPackResponses :: (Array TeamMemberPackResponse)
}


_TeamMemberPackResponses :: Lens' TeamMemberPackResponses {
  teamMemberPackResponses :: (Array TeamMemberPackResponse)
}
_TeamMemberPackResponses f (TeamMemberPackResponses o) = TeamMemberPackResponses <$> f o


mkTeamMemberPackResponses :: (Array TeamMemberPackResponse) -> TeamMemberPackResponses
mkTeamMemberPackResponses teamMemberPackResponses =
  TeamMemberPackResponses{teamMemberPackResponses}


unwrapTeamMemberPackResponses :: TeamMemberPackResponses -> {
  teamMemberPackResponses :: (Array TeamMemberPackResponse)
}
unwrapTeamMemberPackResponses (TeamMemberPackResponses r) = r

instance teamMemberPackResponsesEncodeJson :: EncodeJson TeamMemberPackResponses where
  encodeJson (TeamMemberPackResponses o) =
       "tag" := "TeamMemberPackResponses"
    ~> "team_member_pack_responses" := o.teamMemberPackResponses
    ~> jsonEmptyObject


instance teamMemberPackResponsesDecodeJson :: DecodeJson TeamMemberPackResponses where
  decodeJson o = do
    obj <- decodeJson o
    teamMemberPackResponses <- obj .? "team_member_pack_responses"
    pure $ TeamMemberPackResponses {
      teamMemberPackResponses
    }


instance teamMemberPackResponsesRequestable :: Requestable TeamMemberPackResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance teamMemberPackResponsesRespondable :: Respondable TeamMemberPackResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkTeamMemberPackResponses
      <$> readProp "team_member_pack_responses" json


instance teamMemberPackResponsesIsForeign :: IsForeign TeamMemberPackResponses where
  read json =
      mkTeamMemberPackResponses
      <$> readProp "team_member_pack_responses" json

-- footer