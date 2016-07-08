module LN.T.Pack.Thread where
import LN.T.Permission
import LN.T.Organization
import LN.T.User
import LN.T.Forum
import LN.T.Board
import LN.T.Thread
import LN.T.ThreadPost
import LN.T.Like
import LN.T.Star


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

newtype ThreadPackResponse = ThreadPackResponse {
  thread :: ThreadResponse,
  threadId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int,
  stat :: ThreadStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  latestThreadPost :: (Maybe ThreadPostResponse),
  latestThreadPostUser :: (Maybe UserSanitizedResponse),
  withOrganization :: (Maybe OrganizationResponse),
  withForum :: (Maybe ForumResponse),
  withBoard :: (Maybe BoardResponse),
  permissions :: Permissions
}


type ThreadPackResponseR = {
  thread :: ThreadResponse,
  threadId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int,
  stat :: ThreadStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  latestThreadPost :: (Maybe ThreadPostResponse),
  latestThreadPostUser :: (Maybe UserSanitizedResponse),
  withOrganization :: (Maybe OrganizationResponse),
  withForum :: (Maybe ForumResponse),
  withBoard :: (Maybe BoardResponse),
  permissions :: Permissions
}


_ThreadPackResponse :: Lens' ThreadPackResponse {
  thread :: ThreadResponse,
  threadId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int,
  stat :: ThreadStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  latestThreadPost :: (Maybe ThreadPostResponse),
  latestThreadPostUser :: (Maybe UserSanitizedResponse),
  withOrganization :: (Maybe OrganizationResponse),
  withForum :: (Maybe ForumResponse),
  withBoard :: (Maybe BoardResponse),
  permissions :: Permissions
}
_ThreadPackResponse f (ThreadPackResponse o) = ThreadPackResponse <$> f o


mkThreadPackResponse :: ThreadResponse -> Int -> UserSanitizedResponse -> Int -> ThreadStatResponse -> (Maybe LikeResponse) -> (Maybe StarResponse) -> (Maybe ThreadPostResponse) -> (Maybe UserSanitizedResponse) -> (Maybe OrganizationResponse) -> (Maybe ForumResponse) -> (Maybe BoardResponse) -> Permissions -> ThreadPackResponse
mkThreadPackResponse thread threadId user userId stat like star latestThreadPost latestThreadPostUser withOrganization withForum withBoard permissions =
  ThreadPackResponse{thread, threadId, user, userId, stat, like, star, latestThreadPost, latestThreadPostUser, withOrganization, withForum, withBoard, permissions}


unwrapThreadPackResponse :: ThreadPackResponse -> {
  thread :: ThreadResponse,
  threadId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int,
  stat :: ThreadStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  latestThreadPost :: (Maybe ThreadPostResponse),
  latestThreadPostUser :: (Maybe UserSanitizedResponse),
  withOrganization :: (Maybe OrganizationResponse),
  withForum :: (Maybe ForumResponse),
  withBoard :: (Maybe BoardResponse),
  permissions :: Permissions
}
unwrapThreadPackResponse (ThreadPackResponse r) = r

instance threadPackResponseEncodeJson :: EncodeJson ThreadPackResponse where
  encodeJson (ThreadPackResponse o) =
       "tag" := "ThreadPackResponse"
    ~> "thread" := o.thread
    ~> "thread_id" := o.threadId
    ~> "user" := o.user
    ~> "user_id" := o.userId
    ~> "stat" := o.stat
    ~> "like" := o.like
    ~> "star" := o.star
    ~> "latest_thread_post" := o.latestThreadPost
    ~> "latest_thread_post_user" := o.latestThreadPostUser
    ~> "with_organization" := o.withOrganization
    ~> "with_forum" := o.withForum
    ~> "with_board" := o.withBoard
    ~> "permissions" := o.permissions
    ~> jsonEmptyObject


instance threadPackResponseDecodeJson :: DecodeJson ThreadPackResponse where
  decodeJson o = do
    obj <- decodeJson o
    thread <- obj .? "thread"
    threadId <- obj .? "thread_id"
    user <- obj .? "user"
    userId <- obj .? "user_id"
    stat <- obj .? "stat"
    like <- obj .? "like"
    star <- obj .? "star"
    latestThreadPost <- obj .? "latest_thread_post"
    latestThreadPostUser <- obj .? "latest_thread_post_user"
    withOrganization <- obj .? "with_organization"
    withForum <- obj .? "with_forum"
    withBoard <- obj .? "with_board"
    permissions <- obj .? "permissions"
    pure $ ThreadPackResponse {
      thread,
      threadId,
      user,
      userId,
      stat,
      like,
      star,
      latestThreadPost,
      latestThreadPostUser,
      withOrganization,
      withForum,
      withBoard,
      permissions
    }


instance threadPackResponseRequestable :: Requestable ThreadPackResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance threadPackResponseRespondable :: Respondable ThreadPackResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkThreadPackResponse
      <$> readProp "thread" json
      <*> readProp "thread_id" json
      <*> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "stat" json
      <*> (unNullOrUndefined <$> readProp "like" json)
      <*> (unNullOrUndefined <$> readProp "star" json)
      <*> (unNullOrUndefined <$> readProp "latest_thread_post" json)
      <*> (unNullOrUndefined <$> readProp "latest_thread_post_user" json)
      <*> (unNullOrUndefined <$> readProp "with_organization" json)
      <*> (unNullOrUndefined <$> readProp "with_forum" json)
      <*> (unNullOrUndefined <$> readProp "with_board" json)
      <*> readProp "permissions" json


instance threadPackResponseIsForeign :: IsForeign ThreadPackResponse where
  read json =
      mkThreadPackResponse
      <$> readProp "thread" json
      <*> readProp "thread_id" json
      <*> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "stat" json
      <*> (unNullOrUndefined <$> readProp "like" json)
      <*> (unNullOrUndefined <$> readProp "star" json)
      <*> (unNullOrUndefined <$> readProp "latest_thread_post" json)
      <*> (unNullOrUndefined <$> readProp "latest_thread_post_user" json)
      <*> (unNullOrUndefined <$> readProp "with_organization" json)
      <*> (unNullOrUndefined <$> readProp "with_forum" json)
      <*> (unNullOrUndefined <$> readProp "with_board" json)
      <*> readProp "permissions" json


newtype ThreadPackResponses = ThreadPackResponses {
  threadPackResponses :: (Array ThreadPackResponse)
}


type ThreadPackResponsesR = {
  threadPackResponses :: (Array ThreadPackResponse)
}


_ThreadPackResponses :: Lens' ThreadPackResponses {
  threadPackResponses :: (Array ThreadPackResponse)
}
_ThreadPackResponses f (ThreadPackResponses o) = ThreadPackResponses <$> f o


mkThreadPackResponses :: (Array ThreadPackResponse) -> ThreadPackResponses
mkThreadPackResponses threadPackResponses =
  ThreadPackResponses{threadPackResponses}


unwrapThreadPackResponses :: ThreadPackResponses -> {
  threadPackResponses :: (Array ThreadPackResponse)
}
unwrapThreadPackResponses (ThreadPackResponses r) = r

instance threadPackResponsesEncodeJson :: EncodeJson ThreadPackResponses where
  encodeJson (ThreadPackResponses o) =
       "tag" := "ThreadPackResponses"
    ~> "thread_pack_responses" := o.threadPackResponses
    ~> jsonEmptyObject


instance threadPackResponsesDecodeJson :: DecodeJson ThreadPackResponses where
  decodeJson o = do
    obj <- decodeJson o
    threadPackResponses <- obj .? "thread_pack_responses"
    pure $ ThreadPackResponses {
      threadPackResponses
    }


instance threadPackResponsesRequestable :: Requestable ThreadPackResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance threadPackResponsesRespondable :: Respondable ThreadPackResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkThreadPackResponses
      <$> readProp "thread_pack_responses" json


instance threadPackResponsesIsForeign :: IsForeign ThreadPackResponses where
  read json =
      mkThreadPackResponses
      <$> readProp "thread_pack_responses" json

-- footer