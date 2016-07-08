module LN.T.Group where



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

newtype GroupRequest = GroupRequest {
  guard :: Int
}


type GroupRequestR = {
  guard :: Int
}


_GroupRequest :: Lens' GroupRequest {
  guard :: Int
}
_GroupRequest f (GroupRequest o) = GroupRequest <$> f o


mkGroupRequest :: Int -> GroupRequest
mkGroupRequest guard =
  GroupRequest{guard}


unwrapGroupRequest :: GroupRequest -> {
  guard :: Int
}
unwrapGroupRequest (GroupRequest r) = r

instance groupRequestEncodeJson :: EncodeJson GroupRequest where
  encodeJson (GroupRequest o) =
       "tag" := "GroupRequest"
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance groupRequestDecodeJson :: DecodeJson GroupRequest where
  decodeJson o = do
    obj <- decodeJson o
    guard <- obj .? "guard"
    pure $ GroupRequest {
      guard
    }


instance groupRequestRequestable :: Requestable GroupRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance groupRequestRespondable :: Respondable GroupRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkGroupRequest
      <$> readProp "guard" json


instance groupRequestIsForeign :: IsForeign GroupRequest where
  read json =
      mkGroupRequest
      <$> readProp "guard" json


newtype GroupResponse = GroupResponse {
  id :: Int,
  userId :: Int,
  globalGroupId :: Int,
  organizationId :: Int,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


type GroupResponseR = {
  id :: Int,
  userId :: Int,
  globalGroupId :: Int,
  organizationId :: Int,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


_GroupResponse :: Lens' GroupResponse {
  id :: Int,
  userId :: Int,
  globalGroupId :: Int,
  organizationId :: Int,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
_GroupResponse f (GroupResponse o) = GroupResponse <$> f o


mkGroupResponse :: Int -> Int -> Int -> Int -> Boolean -> Int -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> GroupResponse
mkGroupResponse id userId globalGroupId organizationId active guard createdAt modifiedBy modifiedAt activityAt =
  GroupResponse{id, userId, globalGroupId, organizationId, active, guard, createdAt, modifiedBy, modifiedAt, activityAt}


unwrapGroupResponse :: GroupResponse -> {
  id :: Int,
  userId :: Int,
  globalGroupId :: Int,
  organizationId :: Int,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
unwrapGroupResponse (GroupResponse r) = r

instance groupResponseEncodeJson :: EncodeJson GroupResponse where
  encodeJson (GroupResponse o) =
       "tag" := "GroupResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "global_group_id" := o.globalGroupId
    ~> "organization_id" := o.organizationId
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_by" := o.modifiedBy
    ~> "modified_at" := o.modifiedAt
    ~> "activity_at" := o.activityAt
    ~> jsonEmptyObject


instance groupResponseDecodeJson :: DecodeJson GroupResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    globalGroupId <- obj .? "global_group_id"
    organizationId <- obj .? "organization_id"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedBy <- obj .? "modified_by"
    modifiedAt <- obj .? "modified_at"
    activityAt <- obj .? "activity_at"
    pure $ GroupResponse {
      id,
      userId,
      globalGroupId,
      organizationId,
      active,
      guard,
      createdAt,
      modifiedBy,
      modifiedAt,
      activityAt
    }


instance groupResponseRequestable :: Requestable GroupResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance groupResponseRespondable :: Respondable GroupResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkGroupResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "global_group_id" json
      <*> readProp "organization_id" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_by" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


instance groupResponseIsForeign :: IsForeign GroupResponse where
  read json =
      mkGroupResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "global_group_id" json
      <*> readProp "organization_id" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_by" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


newtype GroupResponses = GroupResponses {
  groupResponses :: (Array GroupResponse)
}


type GroupResponsesR = {
  groupResponses :: (Array GroupResponse)
}


_GroupResponses :: Lens' GroupResponses {
  groupResponses :: (Array GroupResponse)
}
_GroupResponses f (GroupResponses o) = GroupResponses <$> f o


mkGroupResponses :: (Array GroupResponse) -> GroupResponses
mkGroupResponses groupResponses =
  GroupResponses{groupResponses}


unwrapGroupResponses :: GroupResponses -> {
  groupResponses :: (Array GroupResponse)
}
unwrapGroupResponses (GroupResponses r) = r

instance groupResponsesEncodeJson :: EncodeJson GroupResponses where
  encodeJson (GroupResponses o) =
       "tag" := "GroupResponses"
    ~> "group_responses" := o.groupResponses
    ~> jsonEmptyObject


instance groupResponsesDecodeJson :: DecodeJson GroupResponses where
  decodeJson o = do
    obj <- decodeJson o
    groupResponses <- obj .? "group_responses"
    pure $ GroupResponses {
      groupResponses
    }


instance groupResponsesRequestable :: Requestable GroupResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance groupResponsesRespondable :: Respondable GroupResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkGroupResponses
      <$> readProp "group_responses" json


instance groupResponsesIsForeign :: IsForeign GroupResponses where
  read json =
      mkGroupResponses
      <$> readProp "group_responses" json


newtype GroupStatResponse = GroupStatResponse {
  members :: Int
}


type GroupStatResponseR = {
  members :: Int
}


_GroupStatResponse :: Lens' GroupStatResponse {
  members :: Int
}
_GroupStatResponse f (GroupStatResponse o) = GroupStatResponse <$> f o


mkGroupStatResponse :: Int -> GroupStatResponse
mkGroupStatResponse members =
  GroupStatResponse{members}


unwrapGroupStatResponse :: GroupStatResponse -> {
  members :: Int
}
unwrapGroupStatResponse (GroupStatResponse r) = r

instance groupStatResponseEncodeJson :: EncodeJson GroupStatResponse where
  encodeJson (GroupStatResponse o) =
       "tag" := "GroupStatResponse"
    ~> "members" := o.members
    ~> jsonEmptyObject


instance groupStatResponseDecodeJson :: DecodeJson GroupStatResponse where
  decodeJson o = do
    obj <- decodeJson o
    members <- obj .? "members"
    pure $ GroupStatResponse {
      members
    }


instance groupStatResponseRequestable :: Requestable GroupStatResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance groupStatResponseRespondable :: Respondable GroupStatResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkGroupStatResponse
      <$> readProp "members" json


instance groupStatResponseIsForeign :: IsForeign GroupStatResponse where
  read json =
      mkGroupStatResponse
      <$> readProp "members" json


newtype GroupStatResponses = GroupStatResponses {
  groupStatResponses :: (Array GroupStatResponse)
}


type GroupStatResponsesR = {
  groupStatResponses :: (Array GroupStatResponse)
}


_GroupStatResponses :: Lens' GroupStatResponses {
  groupStatResponses :: (Array GroupStatResponse)
}
_GroupStatResponses f (GroupStatResponses o) = GroupStatResponses <$> f o


mkGroupStatResponses :: (Array GroupStatResponse) -> GroupStatResponses
mkGroupStatResponses groupStatResponses =
  GroupStatResponses{groupStatResponses}


unwrapGroupStatResponses :: GroupStatResponses -> {
  groupStatResponses :: (Array GroupStatResponse)
}
unwrapGroupStatResponses (GroupStatResponses r) = r

instance groupStatResponsesEncodeJson :: EncodeJson GroupStatResponses where
  encodeJson (GroupStatResponses o) =
       "tag" := "GroupStatResponses"
    ~> "group_stat_responses" := o.groupStatResponses
    ~> jsonEmptyObject


instance groupStatResponsesDecodeJson :: DecodeJson GroupStatResponses where
  decodeJson o = do
    obj <- decodeJson o
    groupStatResponses <- obj .? "group_stat_responses"
    pure $ GroupStatResponses {
      groupStatResponses
    }


instance groupStatResponsesRequestable :: Requestable GroupStatResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance groupStatResponsesRespondable :: Respondable GroupStatResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkGroupStatResponses
      <$> readProp "group_stat_responses" json


instance groupStatResponsesIsForeign :: IsForeign GroupStatResponses where
  read json =
      mkGroupStatResponses
      <$> readProp "group_stat_responses" json

-- footer