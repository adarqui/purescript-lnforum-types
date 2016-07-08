module LN.T.Pack.Team where
import LN.T.Team
import LN.T.User
import LN.T.Permission


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

newtype TeamPackResponse = TeamPackResponse {
  user :: UserSanitizedResponse,
  userId :: Int,
  team :: TeamResponse,
  teamId :: Int,
  stat :: TeamStatResponse,
  permissions :: Permissions
}


type TeamPackResponseR = {
  user :: UserSanitizedResponse,
  userId :: Int,
  team :: TeamResponse,
  teamId :: Int,
  stat :: TeamStatResponse,
  permissions :: Permissions
}


_TeamPackResponse :: Lens' TeamPackResponse {
  user :: UserSanitizedResponse,
  userId :: Int,
  team :: TeamResponse,
  teamId :: Int,
  stat :: TeamStatResponse,
  permissions :: Permissions
}
_TeamPackResponse f (TeamPackResponse o) = TeamPackResponse <$> f o


mkTeamPackResponse :: UserSanitizedResponse -> Int -> TeamResponse -> Int -> TeamStatResponse -> Permissions -> TeamPackResponse
mkTeamPackResponse user userId team teamId stat permissions =
  TeamPackResponse{user, userId, team, teamId, stat, permissions}


unwrapTeamPackResponse :: TeamPackResponse -> {
  user :: UserSanitizedResponse,
  userId :: Int,
  team :: TeamResponse,
  teamId :: Int,
  stat :: TeamStatResponse,
  permissions :: Permissions
}
unwrapTeamPackResponse (TeamPackResponse r) = r

instance teamPackResponseEncodeJson :: EncodeJson TeamPackResponse where
  encodeJson (TeamPackResponse o) =
       "tag" := "TeamPackResponse"
    ~> "user" := o.user
    ~> "user_id" := o.userId
    ~> "team" := o.team
    ~> "team_id" := o.teamId
    ~> "stat" := o.stat
    ~> "permissions" := o.permissions
    ~> jsonEmptyObject


instance teamPackResponseDecodeJson :: DecodeJson TeamPackResponse where
  decodeJson o = do
    obj <- decodeJson o
    user <- obj .? "user"
    userId <- obj .? "user_id"
    team <- obj .? "team"
    teamId <- obj .? "team_id"
    stat <- obj .? "stat"
    permissions <- obj .? "permissions"
    pure $ TeamPackResponse {
      user,
      userId,
      team,
      teamId,
      stat,
      permissions
    }


instance teamPackResponseRequestable :: Requestable TeamPackResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance teamPackResponseRespondable :: Respondable TeamPackResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkTeamPackResponse
      <$> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "team" json
      <*> readProp "team_id" json
      <*> readProp "stat" json
      <*> readProp "permissions" json


instance teamPackResponseIsForeign :: IsForeign TeamPackResponse where
  read json =
      mkTeamPackResponse
      <$> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "team" json
      <*> readProp "team_id" json
      <*> readProp "stat" json
      <*> readProp "permissions" json


newtype TeamPackResponses = TeamPackResponses {
  teamPackResponses :: (Array TeamPackResponse)
}


type TeamPackResponsesR = {
  teamPackResponses :: (Array TeamPackResponse)
}


_TeamPackResponses :: Lens' TeamPackResponses {
  teamPackResponses :: (Array TeamPackResponse)
}
_TeamPackResponses f (TeamPackResponses o) = TeamPackResponses <$> f o


mkTeamPackResponses :: (Array TeamPackResponse) -> TeamPackResponses
mkTeamPackResponses teamPackResponses =
  TeamPackResponses{teamPackResponses}


unwrapTeamPackResponses :: TeamPackResponses -> {
  teamPackResponses :: (Array TeamPackResponse)
}
unwrapTeamPackResponses (TeamPackResponses r) = r

instance teamPackResponsesEncodeJson :: EncodeJson TeamPackResponses where
  encodeJson (TeamPackResponses o) =
       "tag" := "TeamPackResponses"
    ~> "team_pack_responses" := o.teamPackResponses
    ~> jsonEmptyObject


instance teamPackResponsesDecodeJson :: DecodeJson TeamPackResponses where
  decodeJson o = do
    obj <- decodeJson o
    teamPackResponses <- obj .? "team_pack_responses"
    pure $ TeamPackResponses {
      teamPackResponses
    }


instance teamPackResponsesRequestable :: Requestable TeamPackResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance teamPackResponsesRespondable :: Respondable TeamPackResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkTeamPackResponses
      <$> readProp "team_pack_responses" json


instance teamPackResponsesIsForeign :: IsForeign TeamPackResponses where
  read json =
      mkTeamPackResponses
      <$> readProp "team_pack_responses" json

-- footer