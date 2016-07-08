module LN.T.Pack.GroupMember where
import LN.T.GroupMember
import LN.T.User


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

newtype GroupMemberPackResponse = GroupMemberPackResponse {
  user :: UserSanitizedResponse,
  userId :: Int,
  groupMember :: GroupMemberResponse,
  groupMemberId :: Int,
  isOwner :: Boolean
}


type GroupMemberPackResponseR = {
  user :: UserSanitizedResponse,
  userId :: Int,
  groupMember :: GroupMemberResponse,
  groupMemberId :: Int,
  isOwner :: Boolean
}


_GroupMemberPackResponse :: Lens' GroupMemberPackResponse {
  user :: UserSanitizedResponse,
  userId :: Int,
  groupMember :: GroupMemberResponse,
  groupMemberId :: Int,
  isOwner :: Boolean
}
_GroupMemberPackResponse f (GroupMemberPackResponse o) = GroupMemberPackResponse <$> f o


mkGroupMemberPackResponse :: UserSanitizedResponse -> Int -> GroupMemberResponse -> Int -> Boolean -> GroupMemberPackResponse
mkGroupMemberPackResponse user userId groupMember groupMemberId isOwner =
  GroupMemberPackResponse{user, userId, groupMember, groupMemberId, isOwner}


unwrapGroupMemberPackResponse :: GroupMemberPackResponse -> {
  user :: UserSanitizedResponse,
  userId :: Int,
  groupMember :: GroupMemberResponse,
  groupMemberId :: Int,
  isOwner :: Boolean
}
unwrapGroupMemberPackResponse (GroupMemberPackResponse r) = r

instance groupMemberPackResponseEncodeJson :: EncodeJson GroupMemberPackResponse where
  encodeJson (GroupMemberPackResponse o) =
       "tag" := "GroupMemberPackResponse"
    ~> "user" := o.user
    ~> "user_id" := o.userId
    ~> "group_member" := o.groupMember
    ~> "group_member_id" := o.groupMemberId
    ~> "is_owner" := o.isOwner
    ~> jsonEmptyObject


instance groupMemberPackResponseDecodeJson :: DecodeJson GroupMemberPackResponse where
  decodeJson o = do
    obj <- decodeJson o
    user <- obj .? "user"
    userId <- obj .? "user_id"
    groupMember <- obj .? "group_member"
    groupMemberId <- obj .? "group_member_id"
    isOwner <- obj .? "is_owner"
    pure $ GroupMemberPackResponse {
      user,
      userId,
      groupMember,
      groupMemberId,
      isOwner
    }


instance groupMemberPackResponseRequestable :: Requestable GroupMemberPackResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance groupMemberPackResponseRespondable :: Respondable GroupMemberPackResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkGroupMemberPackResponse
      <$> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "group_member" json
      <*> readProp "group_member_id" json
      <*> readProp "is_owner" json


instance groupMemberPackResponseIsForeign :: IsForeign GroupMemberPackResponse where
  read json =
      mkGroupMemberPackResponse
      <$> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "group_member" json
      <*> readProp "group_member_id" json
      <*> readProp "is_owner" json


newtype GroupMemberPackResponses = GroupMemberPackResponses {
  groupMemberPackResponses :: (Array GroupMemberPackResponse)
}


type GroupMemberPackResponsesR = {
  groupMemberPackResponses :: (Array GroupMemberPackResponse)
}


_GroupMemberPackResponses :: Lens' GroupMemberPackResponses {
  groupMemberPackResponses :: (Array GroupMemberPackResponse)
}
_GroupMemberPackResponses f (GroupMemberPackResponses o) = GroupMemberPackResponses <$> f o


mkGroupMemberPackResponses :: (Array GroupMemberPackResponse) -> GroupMemberPackResponses
mkGroupMemberPackResponses groupMemberPackResponses =
  GroupMemberPackResponses{groupMemberPackResponses}


unwrapGroupMemberPackResponses :: GroupMemberPackResponses -> {
  groupMemberPackResponses :: (Array GroupMemberPackResponse)
}
unwrapGroupMemberPackResponses (GroupMemberPackResponses r) = r

instance groupMemberPackResponsesEncodeJson :: EncodeJson GroupMemberPackResponses where
  encodeJson (GroupMemberPackResponses o) =
       "tag" := "GroupMemberPackResponses"
    ~> "group_member_pack_responses" := o.groupMemberPackResponses
    ~> jsonEmptyObject


instance groupMemberPackResponsesDecodeJson :: DecodeJson GroupMemberPackResponses where
  decodeJson o = do
    obj <- decodeJson o
    groupMemberPackResponses <- obj .? "group_member_pack_responses"
    pure $ GroupMemberPackResponses {
      groupMemberPackResponses
    }


instance groupMemberPackResponsesRequestable :: Requestable GroupMemberPackResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance groupMemberPackResponsesRespondable :: Respondable GroupMemberPackResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkGroupMemberPackResponses
      <$> readProp "group_member_pack_responses" json


instance groupMemberPackResponsesIsForeign :: IsForeign GroupMemberPackResponses where
  read json =
      mkGroupMemberPackResponses
      <$> readProp "group_member_pack_responses" json

-- footer