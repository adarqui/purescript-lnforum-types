module LN.T.GroupMember where



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

newtype GroupMemberRequest = GroupMemberRequest {
  guard :: Int
}


type GroupMemberRequestR = {
  guard :: Int
}


_GroupMemberRequest :: Lens' GroupMemberRequest {
  guard :: Int
}
_GroupMemberRequest f (GroupMemberRequest o) = GroupMemberRequest <$> f o


mkGroupMemberRequest :: Int -> GroupMemberRequest
mkGroupMemberRequest guard =
  GroupMemberRequest{guard}


unwrapGroupMemberRequest :: GroupMemberRequest -> {
  guard :: Int
}
unwrapGroupMemberRequest (GroupMemberRequest r) = r

instance groupMemberRequestEncodeJson :: EncodeJson GroupMemberRequest where
  encodeJson (GroupMemberRequest o) =
       "tag" := "GroupMemberRequest"
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance groupMemberRequestDecodeJson :: DecodeJson GroupMemberRequest where
  decodeJson o = do
    obj <- decodeJson o
    guard <- obj .? "guard"
    pure $ GroupMemberRequest {
      guard
    }


instance groupMemberRequestRequestable :: Requestable GroupMemberRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance groupMemberRequestRespondable :: Respondable GroupMemberRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkGroupMemberRequest
      <$> readProp "guard" json


instance groupMemberRequestIsForeign :: IsForeign GroupMemberRequest where
  read json =
      mkGroupMemberRequest
      <$> readProp "guard" json


newtype GroupMemberResponse = GroupMemberResponse {
  id :: Int,
  userId :: Int,
  globalGroupId :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


type GroupMemberResponseR = {
  id :: Int,
  userId :: Int,
  globalGroupId :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


_GroupMemberResponse :: Lens' GroupMemberResponse {
  id :: Int,
  userId :: Int,
  globalGroupId :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
_GroupMemberResponse f (GroupMemberResponse o) = GroupMemberResponse <$> f o


mkGroupMemberResponse :: Int -> Int -> Int -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> GroupMemberResponse
mkGroupMemberResponse id userId globalGroupId createdAt modifiedBy modifiedAt activityAt =
  GroupMemberResponse{id, userId, globalGroupId, createdAt, modifiedBy, modifiedAt, activityAt}


unwrapGroupMemberResponse :: GroupMemberResponse -> {
  id :: Int,
  userId :: Int,
  globalGroupId :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
unwrapGroupMemberResponse (GroupMemberResponse r) = r

instance groupMemberResponseEncodeJson :: EncodeJson GroupMemberResponse where
  encodeJson (GroupMemberResponse o) =
       "tag" := "GroupMemberResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "global_group_id" := o.globalGroupId
    ~> "created_at" := o.createdAt
    ~> "modified_by" := o.modifiedBy
    ~> "modified_at" := o.modifiedAt
    ~> "activity_at" := o.activityAt
    ~> jsonEmptyObject


instance groupMemberResponseDecodeJson :: DecodeJson GroupMemberResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    globalGroupId <- obj .? "global_group_id"
    createdAt <- obj .? "created_at"
    modifiedBy <- obj .? "modified_by"
    modifiedAt <- obj .? "modified_at"
    activityAt <- obj .? "activity_at"
    pure $ GroupMemberResponse {
      id,
      userId,
      globalGroupId,
      createdAt,
      modifiedBy,
      modifiedAt,
      activityAt
    }


instance groupMemberResponseRequestable :: Requestable GroupMemberResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance groupMemberResponseRespondable :: Respondable GroupMemberResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkGroupMemberResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "global_group_id" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_by" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


instance groupMemberResponseIsForeign :: IsForeign GroupMemberResponse where
  read json =
      mkGroupMemberResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "global_group_id" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_by" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


newtype GroupMemberResponses = GroupMemberResponses {
  groupMemberResponses :: (Array GroupMemberResponse)
}


type GroupMemberResponsesR = {
  groupMemberResponses :: (Array GroupMemberResponse)
}


_GroupMemberResponses :: Lens' GroupMemberResponses {
  groupMemberResponses :: (Array GroupMemberResponse)
}
_GroupMemberResponses f (GroupMemberResponses o) = GroupMemberResponses <$> f o


mkGroupMemberResponses :: (Array GroupMemberResponse) -> GroupMemberResponses
mkGroupMemberResponses groupMemberResponses =
  GroupMemberResponses{groupMemberResponses}


unwrapGroupMemberResponses :: GroupMemberResponses -> {
  groupMemberResponses :: (Array GroupMemberResponse)
}
unwrapGroupMemberResponses (GroupMemberResponses r) = r

instance groupMemberResponsesEncodeJson :: EncodeJson GroupMemberResponses where
  encodeJson (GroupMemberResponses o) =
       "tag" := "GroupMemberResponses"
    ~> "group_member_responses" := o.groupMemberResponses
    ~> jsonEmptyObject


instance groupMemberResponsesDecodeJson :: DecodeJson GroupMemberResponses where
  decodeJson o = do
    obj <- decodeJson o
    groupMemberResponses <- obj .? "group_member_responses"
    pure $ GroupMemberResponses {
      groupMemberResponses
    }


instance groupMemberResponsesRequestable :: Requestable GroupMemberResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance groupMemberResponsesRespondable :: Respondable GroupMemberResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkGroupMemberResponses
      <$> readProp "group_member_responses" json


instance groupMemberResponsesIsForeign :: IsForeign GroupMemberResponses where
  read json =
      mkGroupMemberResponses
      <$> readProp "group_member_responses" json


data GroupMemberStatResponse
  = GroupMemberStatResponse 



instance groupMemberStatResponseEncodeJson :: EncodeJson GroupMemberStatResponse where
  encodeJson (GroupMemberStatResponse ) =
       "tag" := "GroupMemberStatResponse"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance groupMemberStatResponseDecodeJson :: DecodeJson GroupMemberStatResponse where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "GroupMemberStatResponse" -> do
        pure GroupMemberStatResponse

      _ -> Left $ "DecodeJson TypeMismatch for GroupMemberStatResponse"



instance groupMemberStatResponseRequestable :: Requestable GroupMemberStatResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance groupMemberStatResponseRespondable :: Respondable GroupMemberStatResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "GroupMemberStatResponse" -> do
        pure GroupMemberStatResponse

      _ -> Left $ TypeMismatch "GroupMemberStatResponse" "Respondable"



instance groupMemberStatResponseIsForeign :: IsForeign GroupMemberStatResponse where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "GroupMemberStatResponse" -> do
        pure GroupMemberStatResponse

      _ -> Left $ TypeMismatch "GroupMemberStatResponse" "IsForeign"



data GroupMemberStatResponses
  = GroupMemberStatResponses 



instance groupMemberStatResponsesEncodeJson :: EncodeJson GroupMemberStatResponses where
  encodeJson (GroupMemberStatResponses ) =
       "tag" := "GroupMemberStatResponses"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance groupMemberStatResponsesDecodeJson :: DecodeJson GroupMemberStatResponses where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "GroupMemberStatResponses" -> do
        pure GroupMemberStatResponses

      _ -> Left $ "DecodeJson TypeMismatch for GroupMemberStatResponses"



instance groupMemberStatResponsesRequestable :: Requestable GroupMemberStatResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance groupMemberStatResponsesRespondable :: Respondable GroupMemberStatResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "GroupMemberStatResponses" -> do
        pure GroupMemberStatResponses

      _ -> Left $ TypeMismatch "GroupMemberStatResponses" "Respondable"



instance groupMemberStatResponsesIsForeign :: IsForeign GroupMemberStatResponses where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "GroupMemberStatResponses" -> do
        pure GroupMemberStatResponses

      _ -> Left $ TypeMismatch "GroupMemberStatResponses" "IsForeign"


-- footer