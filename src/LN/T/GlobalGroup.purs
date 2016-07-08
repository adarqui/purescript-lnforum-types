module LN.T.GlobalGroup where
import LN.T.Membership
import LN.T.Visibility


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

newtype GlobalGroupRequest = GlobalGroupRequest {
  displayName :: String,
  description :: (Maybe String),
  membership :: Membership,
  icon :: (Maybe String),
  tags :: (Array String),
  visibility :: Visibility,
  guard :: Int
}


type GlobalGroupRequestR = {
  displayName :: String,
  description :: (Maybe String),
  membership :: Membership,
  icon :: (Maybe String),
  tags :: (Array String),
  visibility :: Visibility,
  guard :: Int
}


_GlobalGroupRequest :: Lens' GlobalGroupRequest {
  displayName :: String,
  description :: (Maybe String),
  membership :: Membership,
  icon :: (Maybe String),
  tags :: (Array String),
  visibility :: Visibility,
  guard :: Int
}
_GlobalGroupRequest f (GlobalGroupRequest o) = GlobalGroupRequest <$> f o


mkGlobalGroupRequest :: String -> (Maybe String) -> Membership -> (Maybe String) -> (Array String) -> Visibility -> Int -> GlobalGroupRequest
mkGlobalGroupRequest displayName description membership icon tags visibility guard =
  GlobalGroupRequest{displayName, description, membership, icon, tags, visibility, guard}


unwrapGlobalGroupRequest :: GlobalGroupRequest -> {
  displayName :: String,
  description :: (Maybe String),
  membership :: Membership,
  icon :: (Maybe String),
  tags :: (Array String),
  visibility :: Visibility,
  guard :: Int
}
unwrapGlobalGroupRequest (GlobalGroupRequest r) = r

instance globalGroupRequestEncodeJson :: EncodeJson GlobalGroupRequest where
  encodeJson (GlobalGroupRequest o) =
       "tag" := "GlobalGroupRequest"
    ~> "display_name" := o.displayName
    ~> "description" := o.description
    ~> "membership" := o.membership
    ~> "icon" := o.icon
    ~> "tags" := o.tags
    ~> "visibility" := o.visibility
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance globalGroupRequestDecodeJson :: DecodeJson GlobalGroupRequest where
  decodeJson o = do
    obj <- decodeJson o
    displayName <- obj .? "display_name"
    description <- obj .? "description"
    membership <- obj .? "membership"
    icon <- obj .? "icon"
    tags <- obj .? "tags"
    visibility <- obj .? "visibility"
    guard <- obj .? "guard"
    pure $ GlobalGroupRequest {
      displayName,
      description,
      membership,
      icon,
      tags,
      visibility,
      guard
    }


instance globalGroupRequestRequestable :: Requestable GlobalGroupRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance globalGroupRequestRespondable :: Respondable GlobalGroupRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkGlobalGroupRequest
      <$> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "membership" json
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "tags" json
      <*> readProp "visibility" json
      <*> readProp "guard" json


instance globalGroupRequestIsForeign :: IsForeign GlobalGroupRequest where
  read json =
      mkGlobalGroupRequest
      <$> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "membership" json
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "tags" json
      <*> readProp "visibility" json
      <*> readProp "guard" json


newtype GlobalGroupResponse = GlobalGroupResponse {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
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


type GlobalGroupResponseR = {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
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


_GlobalGroupResponse :: Lens' GlobalGroupResponse {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
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
_GlobalGroupResponse f (GlobalGroupResponse o) = GlobalGroupResponse <$> f o


mkGlobalGroupResponse :: Int -> Int -> String -> String -> (Maybe String) -> Membership -> (Maybe String) -> (Array String) -> Visibility -> Boolean -> Int -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> GlobalGroupResponse
mkGlobalGroupResponse id userId name displayName description membership icon tags visibility active guard createdAt modifiedBy modifiedAt activityAt =
  GlobalGroupResponse{id, userId, name, displayName, description, membership, icon, tags, visibility, active, guard, createdAt, modifiedBy, modifiedAt, activityAt}


unwrapGlobalGroupResponse :: GlobalGroupResponse -> {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
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
unwrapGlobalGroupResponse (GlobalGroupResponse r) = r

instance globalGroupResponseEncodeJson :: EncodeJson GlobalGroupResponse where
  encodeJson (GlobalGroupResponse o) =
       "tag" := "GlobalGroupResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "name" := o.name
    ~> "display_name" := o.displayName
    ~> "description" := o.description
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


instance globalGroupResponseDecodeJson :: DecodeJson GlobalGroupResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    name <- obj .? "name"
    displayName <- obj .? "display_name"
    description <- obj .? "description"
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
    pure $ GlobalGroupResponse {
      id,
      userId,
      name,
      displayName,
      description,
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


instance globalGroupResponseRequestable :: Requestable GlobalGroupResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance globalGroupResponseRespondable :: Respondable GlobalGroupResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkGlobalGroupResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "name" json
      <*> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
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


instance globalGroupResponseIsForeign :: IsForeign GlobalGroupResponse where
  read json =
      mkGlobalGroupResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "name" json
      <*> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
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


newtype GlobalGroupResponses = GlobalGroupResponses {
  globalGroupResponses :: (Array GlobalGroupResponse)
}


type GlobalGroupResponsesR = {
  globalGroupResponses :: (Array GlobalGroupResponse)
}


_GlobalGroupResponses :: Lens' GlobalGroupResponses {
  globalGroupResponses :: (Array GlobalGroupResponse)
}
_GlobalGroupResponses f (GlobalGroupResponses o) = GlobalGroupResponses <$> f o


mkGlobalGroupResponses :: (Array GlobalGroupResponse) -> GlobalGroupResponses
mkGlobalGroupResponses globalGroupResponses =
  GlobalGroupResponses{globalGroupResponses}


unwrapGlobalGroupResponses :: GlobalGroupResponses -> {
  globalGroupResponses :: (Array GlobalGroupResponse)
}
unwrapGlobalGroupResponses (GlobalGroupResponses r) = r

instance globalGroupResponsesEncodeJson :: EncodeJson GlobalGroupResponses where
  encodeJson (GlobalGroupResponses o) =
       "tag" := "GlobalGroupResponses"
    ~> "global_group_responses" := o.globalGroupResponses
    ~> jsonEmptyObject


instance globalGroupResponsesDecodeJson :: DecodeJson GlobalGroupResponses where
  decodeJson o = do
    obj <- decodeJson o
    globalGroupResponses <- obj .? "global_group_responses"
    pure $ GlobalGroupResponses {
      globalGroupResponses
    }


instance globalGroupResponsesRequestable :: Requestable GlobalGroupResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance globalGroupResponsesRespondable :: Respondable GlobalGroupResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkGlobalGroupResponses
      <$> readProp "global_group_responses" json


instance globalGroupResponsesIsForeign :: IsForeign GlobalGroupResponses where
  read json =
      mkGlobalGroupResponses
      <$> readProp "global_group_responses" json


newtype GlobalGroupStatResponse = GlobalGroupStatResponse {
  groups :: Int
}


type GlobalGroupStatResponseR = {
  groups :: Int
}


_GlobalGroupStatResponse :: Lens' GlobalGroupStatResponse {
  groups :: Int
}
_GlobalGroupStatResponse f (GlobalGroupStatResponse o) = GlobalGroupStatResponse <$> f o


mkGlobalGroupStatResponse :: Int -> GlobalGroupStatResponse
mkGlobalGroupStatResponse groups =
  GlobalGroupStatResponse{groups}


unwrapGlobalGroupStatResponse :: GlobalGroupStatResponse -> {
  groups :: Int
}
unwrapGlobalGroupStatResponse (GlobalGroupStatResponse r) = r

instance globalGroupStatResponseEncodeJson :: EncodeJson GlobalGroupStatResponse where
  encodeJson (GlobalGroupStatResponse o) =
       "tag" := "GlobalGroupStatResponse"
    ~> "groups" := o.groups
    ~> jsonEmptyObject


instance globalGroupStatResponseDecodeJson :: DecodeJson GlobalGroupStatResponse where
  decodeJson o = do
    obj <- decodeJson o
    groups <- obj .? "groups"
    pure $ GlobalGroupStatResponse {
      groups
    }


instance globalGroupStatResponseRequestable :: Requestable GlobalGroupStatResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance globalGroupStatResponseRespondable :: Respondable GlobalGroupStatResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkGlobalGroupStatResponse
      <$> readProp "groups" json


instance globalGroupStatResponseIsForeign :: IsForeign GlobalGroupStatResponse where
  read json =
      mkGlobalGroupStatResponse
      <$> readProp "groups" json


newtype GlobalGroupStatResponses = GlobalGroupStatResponses {
  globalGroupStatResponses :: (Array GlobalGroupStatResponse)
}


type GlobalGroupStatResponsesR = {
  globalGroupStatResponses :: (Array GlobalGroupStatResponse)
}


_GlobalGroupStatResponses :: Lens' GlobalGroupStatResponses {
  globalGroupStatResponses :: (Array GlobalGroupStatResponse)
}
_GlobalGroupStatResponses f (GlobalGroupStatResponses o) = GlobalGroupStatResponses <$> f o


mkGlobalGroupStatResponses :: (Array GlobalGroupStatResponse) -> GlobalGroupStatResponses
mkGlobalGroupStatResponses globalGroupStatResponses =
  GlobalGroupStatResponses{globalGroupStatResponses}


unwrapGlobalGroupStatResponses :: GlobalGroupStatResponses -> {
  globalGroupStatResponses :: (Array GlobalGroupStatResponse)
}
unwrapGlobalGroupStatResponses (GlobalGroupStatResponses r) = r

instance globalGroupStatResponsesEncodeJson :: EncodeJson GlobalGroupStatResponses where
  encodeJson (GlobalGroupStatResponses o) =
       "tag" := "GlobalGroupStatResponses"
    ~> "global_group_stat_responses" := o.globalGroupStatResponses
    ~> jsonEmptyObject


instance globalGroupStatResponsesDecodeJson :: DecodeJson GlobalGroupStatResponses where
  decodeJson o = do
    obj <- decodeJson o
    globalGroupStatResponses <- obj .? "global_group_stat_responses"
    pure $ GlobalGroupStatResponses {
      globalGroupStatResponses
    }


instance globalGroupStatResponsesRequestable :: Requestable GlobalGroupStatResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance globalGroupStatResponsesRespondable :: Respondable GlobalGroupStatResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkGlobalGroupStatResponses
      <$> readProp "global_group_stat_responses" json


instance globalGroupStatResponsesIsForeign :: IsForeign GlobalGroupStatResponses where
  read json =
      mkGlobalGroupStatResponses
      <$> readProp "global_group_stat_responses" json

-- footer