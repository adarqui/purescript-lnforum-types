module LN.T.Team where
import LN.T.Visibility
import LN.T.Membership


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

data SystemTeam
  = Team_Owners 
  | Team_Members 



instance systemTeamEncodeJson :: EncodeJson SystemTeam where
  encodeJson (Team_Owners ) =
       "tag" := "Team_Owners"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Team_Members ) =
       "tag" := "Team_Members"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance systemTeamDecodeJson :: DecodeJson SystemTeam where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Team_Owners" -> do
        pure Team_Owners

      "Team_Members" -> do
        pure Team_Members

      _ -> Left $ "DecodeJson TypeMismatch for SystemTeam"



instance systemTeamRequestable :: Requestable SystemTeam where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance systemTeamRespondable :: Respondable SystemTeam where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "Team_Owners" -> do
        pure Team_Owners

      "Team_Members" -> do
        pure Team_Members

      _ -> Left $ TypeMismatch "SystemTeam" "Respondable"



instance systemTeamIsForeign :: IsForeign SystemTeam where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "Team_Owners" -> do
        pure Team_Owners

      "Team_Members" -> do
        pure Team_Members

      _ -> Left $ TypeMismatch "SystemTeam" "IsForeign"



instance systemTeamEq :: Eq SystemTeam where
  eq Team_Owners Team_Owners = true
  eq Team_Members Team_Members = true
  eq _ _ = false

readSystemTeam :: String -> Maybe SystemTeam
readSystemTeam "team_owners" = Just Team_Owners
readSystemTeam "team_members" = Just Team_Members
readSystemTeam _ = Nothing

newtype TeamRequest = TeamRequest {
  membership :: Membership,
  icon :: (Maybe String),
  tags :: (Array String),
  visibility :: Visibility,
  guard :: Int
}


type TeamRequestR = {
  membership :: Membership,
  icon :: (Maybe String),
  tags :: (Array String),
  visibility :: Visibility,
  guard :: Int
}


_TeamRequest :: Lens' TeamRequest {
  membership :: Membership,
  icon :: (Maybe String),
  tags :: (Array String),
  visibility :: Visibility,
  guard :: Int
}
_TeamRequest f (TeamRequest o) = TeamRequest <$> f o


mkTeamRequest :: Membership -> (Maybe String) -> (Array String) -> Visibility -> Int -> TeamRequest
mkTeamRequest membership icon tags visibility guard =
  TeamRequest{membership, icon, tags, visibility, guard}


unwrapTeamRequest :: TeamRequest -> {
  membership :: Membership,
  icon :: (Maybe String),
  tags :: (Array String),
  visibility :: Visibility,
  guard :: Int
}
unwrapTeamRequest (TeamRequest r) = r

instance teamRequestEncodeJson :: EncodeJson TeamRequest where
  encodeJson (TeamRequest o) =
       "tag" := "TeamRequest"
    ~> "membership" := o.membership
    ~> "icon" := o.icon
    ~> "tags" := o.tags
    ~> "visibility" := o.visibility
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance teamRequestDecodeJson :: DecodeJson TeamRequest where
  decodeJson o = do
    obj <- decodeJson o
    membership <- obj .? "membership"
    icon <- obj .? "icon"
    tags <- obj .? "tags"
    visibility <- obj .? "visibility"
    guard <- obj .? "guard"
    pure $ TeamRequest {
      membership,
      icon,
      tags,
      visibility,
      guard
    }


instance teamRequestRequestable :: Requestable TeamRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance teamRequestRespondable :: Respondable TeamRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkTeamRequest
      <$> readProp "membership" json
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "tags" json
      <*> readProp "visibility" json
      <*> readProp "guard" json


instance teamRequestIsForeign :: IsForeign TeamRequest where
  read json =
      mkTeamRequest
      <$> readProp "membership" json
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "tags" json
      <*> readProp "visibility" json
      <*> readProp "guard" json


newtype TeamResponse = TeamResponse {
  id :: Int,
  userId :: Int,
  orgId :: Int,
  system :: SystemTeam,
  membership :: Membership,
  icon :: (Maybe String),
  tags :: (Array String),
  visibility :: Visibility,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


type TeamResponseR = {
  id :: Int,
  userId :: Int,
  orgId :: Int,
  system :: SystemTeam,
  membership :: Membership,
  icon :: (Maybe String),
  tags :: (Array String),
  visibility :: Visibility,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


_TeamResponse :: Lens' TeamResponse {
  id :: Int,
  userId :: Int,
  orgId :: Int,
  system :: SystemTeam,
  membership :: Membership,
  icon :: (Maybe String),
  tags :: (Array String),
  visibility :: Visibility,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
_TeamResponse f (TeamResponse o) = TeamResponse <$> f o


mkTeamResponse :: Int -> Int -> Int -> SystemTeam -> Membership -> (Maybe String) -> (Array String) -> Visibility -> Boolean -> Int -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> TeamResponse
mkTeamResponse id userId orgId system membership icon tags visibility active guard createdAt modifiedBy modifiedAt activityAt =
  TeamResponse{id, userId, orgId, system, membership, icon, tags, visibility, active, guard, createdAt, modifiedBy, modifiedAt, activityAt}


unwrapTeamResponse :: TeamResponse -> {
  id :: Int,
  userId :: Int,
  orgId :: Int,
  system :: SystemTeam,
  membership :: Membership,
  icon :: (Maybe String),
  tags :: (Array String),
  visibility :: Visibility,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
unwrapTeamResponse (TeamResponse r) = r

instance teamResponseEncodeJson :: EncodeJson TeamResponse where
  encodeJson (TeamResponse o) =
       "tag" := "TeamResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "org_id" := o.orgId
    ~> "system" := o.system
    ~> "membership" := o.membership
    ~> "icon" := o.icon
    ~> "tags" := o.tags
    ~> "visibility" := o.visibility
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_by" := o.modifiedBy
    ~> "modified_at" := o.modifiedAt
    ~> "activity_at" := o.activityAt
    ~> jsonEmptyObject


instance teamResponseDecodeJson :: DecodeJson TeamResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    orgId <- obj .? "org_id"
    system <- obj .? "system"
    membership <- obj .? "membership"
    icon <- obj .? "icon"
    tags <- obj .? "tags"
    visibility <- obj .? "visibility"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedBy <- obj .? "modified_by"
    modifiedAt <- obj .? "modified_at"
    activityAt <- obj .? "activity_at"
    pure $ TeamResponse {
      id,
      userId,
      orgId,
      system,
      membership,
      icon,
      tags,
      visibility,
      active,
      guard,
      createdAt,
      modifiedBy,
      modifiedAt,
      activityAt
    }


instance teamResponseRequestable :: Requestable TeamResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance teamResponseRespondable :: Respondable TeamResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkTeamResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "org_id" json
      <*> readProp "system" json
      <*> readProp "membership" json
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "tags" json
      <*> readProp "visibility" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_by" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


instance teamResponseIsForeign :: IsForeign TeamResponse where
  read json =
      mkTeamResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "org_id" json
      <*> readProp "system" json
      <*> readProp "membership" json
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "tags" json
      <*> readProp "visibility" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_by" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


newtype TeamResponses = TeamResponses {
  teamResponses :: (Array TeamResponse)
}


type TeamResponsesR = {
  teamResponses :: (Array TeamResponse)
}


_TeamResponses :: Lens' TeamResponses {
  teamResponses :: (Array TeamResponse)
}
_TeamResponses f (TeamResponses o) = TeamResponses <$> f o


mkTeamResponses :: (Array TeamResponse) -> TeamResponses
mkTeamResponses teamResponses =
  TeamResponses{teamResponses}


unwrapTeamResponses :: TeamResponses -> {
  teamResponses :: (Array TeamResponse)
}
unwrapTeamResponses (TeamResponses r) = r

instance teamResponsesEncodeJson :: EncodeJson TeamResponses where
  encodeJson (TeamResponses o) =
       "tag" := "TeamResponses"
    ~> "team_responses" := o.teamResponses
    ~> jsonEmptyObject


instance teamResponsesDecodeJson :: DecodeJson TeamResponses where
  decodeJson o = do
    obj <- decodeJson o
    teamResponses <- obj .? "team_responses"
    pure $ TeamResponses {
      teamResponses
    }


instance teamResponsesRequestable :: Requestable TeamResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance teamResponsesRespondable :: Respondable TeamResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkTeamResponses
      <$> readProp "team_responses" json


instance teamResponsesIsForeign :: IsForeign TeamResponses where
  read json =
      mkTeamResponses
      <$> readProp "team_responses" json


newtype TeamStatResponse = TeamStatResponse {
  members :: Int
}


type TeamStatResponseR = {
  members :: Int
}


_TeamStatResponse :: Lens' TeamStatResponse {
  members :: Int
}
_TeamStatResponse f (TeamStatResponse o) = TeamStatResponse <$> f o


mkTeamStatResponse :: Int -> TeamStatResponse
mkTeamStatResponse members =
  TeamStatResponse{members}


unwrapTeamStatResponse :: TeamStatResponse -> {
  members :: Int
}
unwrapTeamStatResponse (TeamStatResponse r) = r

instance teamStatResponseEncodeJson :: EncodeJson TeamStatResponse where
  encodeJson (TeamStatResponse o) =
       "tag" := "TeamStatResponse"
    ~> "members" := o.members
    ~> jsonEmptyObject


instance teamStatResponseDecodeJson :: DecodeJson TeamStatResponse where
  decodeJson o = do
    obj <- decodeJson o
    members <- obj .? "members"
    pure $ TeamStatResponse {
      members
    }


instance teamStatResponseRequestable :: Requestable TeamStatResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance teamStatResponseRespondable :: Respondable TeamStatResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkTeamStatResponse
      <$> readProp "members" json


instance teamStatResponseIsForeign :: IsForeign TeamStatResponse where
  read json =
      mkTeamStatResponse
      <$> readProp "members" json


newtype TeamStatResponses = TeamStatResponses {
  teamStatResponses :: (Array TeamStatResponse)
}


type TeamStatResponsesR = {
  teamStatResponses :: (Array TeamStatResponse)
}


_TeamStatResponses :: Lens' TeamStatResponses {
  teamStatResponses :: (Array TeamStatResponse)
}
_TeamStatResponses f (TeamStatResponses o) = TeamStatResponses <$> f o


mkTeamStatResponses :: (Array TeamStatResponse) -> TeamStatResponses
mkTeamStatResponses teamStatResponses =
  TeamStatResponses{teamStatResponses}


unwrapTeamStatResponses :: TeamStatResponses -> {
  teamStatResponses :: (Array TeamStatResponse)
}
unwrapTeamStatResponses (TeamStatResponses r) = r

instance teamStatResponsesEncodeJson :: EncodeJson TeamStatResponses where
  encodeJson (TeamStatResponses o) =
       "tag" := "TeamStatResponses"
    ~> "team_stat_responses" := o.teamStatResponses
    ~> jsonEmptyObject


instance teamStatResponsesDecodeJson :: DecodeJson TeamStatResponses where
  decodeJson o = do
    obj <- decodeJson o
    teamStatResponses <- obj .? "team_stat_responses"
    pure $ TeamStatResponses {
      teamStatResponses
    }


instance teamStatResponsesRequestable :: Requestable TeamStatResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance teamStatResponsesRespondable :: Respondable TeamStatResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkTeamStatResponses
      <$> readProp "team_stat_responses" json


instance teamStatResponsesIsForeign :: IsForeign TeamStatResponses where
  read json =
      mkTeamStatResponses
      <$> readProp "team_stat_responses" json

-- footer