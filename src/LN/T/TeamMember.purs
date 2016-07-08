module LN.T.TeamMember where


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

newtype TeamMemberRequest = TeamMemberRequest {
  guard :: Int
}


type TeamMemberRequestR = {
  guard :: Int
}


_TeamMemberRequest :: Lens' TeamMemberRequest {
  guard :: Int
}
_TeamMemberRequest f (TeamMemberRequest o) = TeamMemberRequest <$> f o


mkTeamMemberRequest :: Int -> TeamMemberRequest
mkTeamMemberRequest guard =
  TeamMemberRequest{guard}


unwrapTeamMemberRequest :: TeamMemberRequest -> {
  guard :: Int
}
unwrapTeamMemberRequest (TeamMemberRequest r) = r

instance teamMemberRequestEncodeJson :: EncodeJson TeamMemberRequest where
  encodeJson (TeamMemberRequest o) =
       "tag" := "TeamMemberRequest"
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance teamMemberRequestDecodeJson :: DecodeJson TeamMemberRequest where
  decodeJson o = do
    obj <- decodeJson o
    guard <- obj .? "guard"
    pure $ TeamMemberRequest {
      guard
    }


instance teamMemberRequestRequestable :: Requestable TeamMemberRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance teamMemberRequestRespondable :: Respondable TeamMemberRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkTeamMemberRequest
      <$> readProp "guard" json


instance teamMemberRequestIsForeign :: IsForeign TeamMemberRequest where
  read json =
      mkTeamMemberRequest
      <$> readProp "guard" json


newtype TeamMemberResponse = TeamMemberResponse {
  id :: Int,
  userId :: Int,
  orgId :: Int,
  teamId :: Int,
  isAccepted :: Boolean,
  acceptedAt :: (Maybe Date),
  isBlocked :: Boolean,
  blockedAt :: (Maybe Date),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


type TeamMemberResponseR = {
  id :: Int,
  userId :: Int,
  orgId :: Int,
  teamId :: Int,
  isAccepted :: Boolean,
  acceptedAt :: (Maybe Date),
  isBlocked :: Boolean,
  blockedAt :: (Maybe Date),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


_TeamMemberResponse :: Lens' TeamMemberResponse {
  id :: Int,
  userId :: Int,
  orgId :: Int,
  teamId :: Int,
  isAccepted :: Boolean,
  acceptedAt :: (Maybe Date),
  isBlocked :: Boolean,
  blockedAt :: (Maybe Date),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
_TeamMemberResponse f (TeamMemberResponse o) = TeamMemberResponse <$> f o


mkTeamMemberResponse :: Int -> Int -> Int -> Int -> Boolean -> (Maybe Date) -> Boolean -> (Maybe Date) -> Boolean -> Int -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> TeamMemberResponse
mkTeamMemberResponse id userId orgId teamId isAccepted acceptedAt isBlocked blockedAt active guard createdAt modifiedBy modifiedAt activityAt =
  TeamMemberResponse{id, userId, orgId, teamId, isAccepted, acceptedAt, isBlocked, blockedAt, active, guard, createdAt, modifiedBy, modifiedAt, activityAt}


unwrapTeamMemberResponse :: TeamMemberResponse -> {
  id :: Int,
  userId :: Int,
  orgId :: Int,
  teamId :: Int,
  isAccepted :: Boolean,
  acceptedAt :: (Maybe Date),
  isBlocked :: Boolean,
  blockedAt :: (Maybe Date),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
unwrapTeamMemberResponse (TeamMemberResponse r) = r

instance teamMemberResponseEncodeJson :: EncodeJson TeamMemberResponse where
  encodeJson (TeamMemberResponse o) =
       "tag" := "TeamMemberResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "org_id" := o.orgId
    ~> "team_id" := o.teamId
    ~> "is_accepted" := o.isAccepted
    ~> "accepted_at" := o.acceptedAt
    ~> "is_blocked" := o.isBlocked
    ~> "blocked_at" := o.blockedAt
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_by" := o.modifiedBy
    ~> "modified_at" := o.modifiedAt
    ~> "activity_at" := o.activityAt
    ~> jsonEmptyObject


instance teamMemberResponseDecodeJson :: DecodeJson TeamMemberResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    orgId <- obj .? "org_id"
    teamId <- obj .? "team_id"
    isAccepted <- obj .? "is_accepted"
    acceptedAt <- obj .? "accepted_at"
    isBlocked <- obj .? "is_blocked"
    blockedAt <- obj .? "blocked_at"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedBy <- obj .? "modified_by"
    modifiedAt <- obj .? "modified_at"
    activityAt <- obj .? "activity_at"
    pure $ TeamMemberResponse {
      id,
      userId,
      orgId,
      teamId,
      isAccepted,
      acceptedAt,
      isBlocked,
      blockedAt,
      active,
      guard,
      createdAt,
      modifiedBy,
      modifiedAt,
      activityAt
    }


instance teamMemberResponseRequestable :: Requestable TeamMemberResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance teamMemberResponseRespondable :: Respondable TeamMemberResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkTeamMemberResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "org_id" json
      <*> readProp "team_id" json
      <*> readProp "is_accepted" json
      <*> (unNullOrUndefined <$> readProp "accepted_at" json)
      <*> readProp "is_blocked" json
      <*> (unNullOrUndefined <$> readProp "blocked_at" json)
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_by" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


instance teamMemberResponseIsForeign :: IsForeign TeamMemberResponse where
  read json =
      mkTeamMemberResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "org_id" json
      <*> readProp "team_id" json
      <*> readProp "is_accepted" json
      <*> (unNullOrUndefined <$> readProp "accepted_at" json)
      <*> readProp "is_blocked" json
      <*> (unNullOrUndefined <$> readProp "blocked_at" json)
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_by" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


newtype TeamMemberResponses = TeamMemberResponses {
  teamMemberResponses :: (Array TeamMemberResponse)
}


type TeamMemberResponsesR = {
  teamMemberResponses :: (Array TeamMemberResponse)
}


_TeamMemberResponses :: Lens' TeamMemberResponses {
  teamMemberResponses :: (Array TeamMemberResponse)
}
_TeamMemberResponses f (TeamMemberResponses o) = TeamMemberResponses <$> f o


mkTeamMemberResponses :: (Array TeamMemberResponse) -> TeamMemberResponses
mkTeamMemberResponses teamMemberResponses =
  TeamMemberResponses{teamMemberResponses}


unwrapTeamMemberResponses :: TeamMemberResponses -> {
  teamMemberResponses :: (Array TeamMemberResponse)
}
unwrapTeamMemberResponses (TeamMemberResponses r) = r

instance teamMemberResponsesEncodeJson :: EncodeJson TeamMemberResponses where
  encodeJson (TeamMemberResponses o) =
       "tag" := "TeamMemberResponses"
    ~> "team_member_responses" := o.teamMemberResponses
    ~> jsonEmptyObject


instance teamMemberResponsesDecodeJson :: DecodeJson TeamMemberResponses where
  decodeJson o = do
    obj <- decodeJson o
    teamMemberResponses <- obj .? "team_member_responses"
    pure $ TeamMemberResponses {
      teamMemberResponses
    }


instance teamMemberResponsesRequestable :: Requestable TeamMemberResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance teamMemberResponsesRespondable :: Respondable TeamMemberResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkTeamMemberResponses
      <$> readProp "team_member_responses" json


instance teamMemberResponsesIsForeign :: IsForeign TeamMemberResponses where
  read json =
      mkTeamMemberResponses
      <$> readProp "team_member_responses" json


data TeamMemberStatResponse
  = TeamMemberStatResponse 



instance teamMemberStatResponseEncodeJson :: EncodeJson TeamMemberStatResponse where
  encodeJson (TeamMemberStatResponse ) =
       "tag" := "TeamMemberStatResponse"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance teamMemberStatResponseDecodeJson :: DecodeJson TeamMemberStatResponse where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "TeamMemberStatResponse" -> do
        pure TeamMemberStatResponse

      _ -> Left $ "DecodeJson TypeMismatch for TeamMemberStatResponse"



instance teamMemberStatResponseRequestable :: Requestable TeamMemberStatResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance teamMemberStatResponseRespondable :: Respondable TeamMemberStatResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "TeamMemberStatResponse" -> do
        pure TeamMemberStatResponse

      _ -> Left $ TypeMismatch "TeamMemberStatResponse" "Respondable"



instance teamMemberStatResponseIsForeign :: IsForeign TeamMemberStatResponse where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "TeamMemberStatResponse" -> do
        pure TeamMemberStatResponse

      _ -> Left $ TypeMismatch "TeamMemberStatResponse" "IsForeign"



newtype TeamMemberStatResponses = TeamMemberStatResponses {
  teamMemberStatResponses :: (Array TeamMemberStatResponse)
}


type TeamMemberStatResponsesR = {
  teamMemberStatResponses :: (Array TeamMemberStatResponse)
}


_TeamMemberStatResponses :: Lens' TeamMemberStatResponses {
  teamMemberStatResponses :: (Array TeamMemberStatResponse)
}
_TeamMemberStatResponses f (TeamMemberStatResponses o) = TeamMemberStatResponses <$> f o


mkTeamMemberStatResponses :: (Array TeamMemberStatResponse) -> TeamMemberStatResponses
mkTeamMemberStatResponses teamMemberStatResponses =
  TeamMemberStatResponses{teamMemberStatResponses}


unwrapTeamMemberStatResponses :: TeamMemberStatResponses -> {
  teamMemberStatResponses :: (Array TeamMemberStatResponse)
}
unwrapTeamMemberStatResponses (TeamMemberStatResponses r) = r

instance teamMemberStatResponsesEncodeJson :: EncodeJson TeamMemberStatResponses where
  encodeJson (TeamMemberStatResponses o) =
       "tag" := "TeamMemberStatResponses"
    ~> "team_member_stat_responses" := o.teamMemberStatResponses
    ~> jsonEmptyObject


instance teamMemberStatResponsesDecodeJson :: DecodeJson TeamMemberStatResponses where
  decodeJson o = do
    obj <- decodeJson o
    teamMemberStatResponses <- obj .? "team_member_stat_responses"
    pure $ TeamMemberStatResponses {
      teamMemberStatResponses
    }


instance teamMemberStatResponsesRequestable :: Requestable TeamMemberStatResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance teamMemberStatResponsesRespondable :: Respondable TeamMemberStatResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkTeamMemberStatResponses
      <$> readProp "team_member_stat_responses" json


instance teamMemberStatResponsesIsForeign :: IsForeign TeamMemberStatResponses where
  read json =
      mkTeamMemberStatResponses
      <$> readProp "team_member_stat_responses" json

-- footer