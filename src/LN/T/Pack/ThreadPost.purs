module LN.T.Pack.ThreadPost where
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

newtype ThreadPostPackResponse = ThreadPostPackResponse {
  threadPost :: ThreadPostResponse,
  threadPostId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int,
  stat :: ThreadPostStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  withOrganization :: (Maybe OrganizationResponse),
  withForum :: (Maybe ForumResponse),
  withBoard :: (Maybe BoardResponse),
  withThread :: (Maybe ThreadResponse),
  withThreadPosts :: (Maybe (Array Int)),
  withThreadPostsOffset :: (Maybe Int),
  withThreadPostsLimit :: (Maybe Int),
  permissions :: Permissions
}


type ThreadPostPackResponseR = {
  threadPost :: ThreadPostResponse,
  threadPostId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int,
  stat :: ThreadPostStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  withOrganization :: (Maybe OrganizationResponse),
  withForum :: (Maybe ForumResponse),
  withBoard :: (Maybe BoardResponse),
  withThread :: (Maybe ThreadResponse),
  withThreadPosts :: (Maybe (Array Int)),
  withThreadPostsOffset :: (Maybe Int),
  withThreadPostsLimit :: (Maybe Int),
  permissions :: Permissions
}


_ThreadPostPackResponse :: Lens' ThreadPostPackResponse {
  threadPost :: ThreadPostResponse,
  threadPostId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int,
  stat :: ThreadPostStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  withOrganization :: (Maybe OrganizationResponse),
  withForum :: (Maybe ForumResponse),
  withBoard :: (Maybe BoardResponse),
  withThread :: (Maybe ThreadResponse),
  withThreadPosts :: (Maybe (Array Int)),
  withThreadPostsOffset :: (Maybe Int),
  withThreadPostsLimit :: (Maybe Int),
  permissions :: Permissions
}
_ThreadPostPackResponse f (ThreadPostPackResponse o) = ThreadPostPackResponse <$> f o


mkThreadPostPackResponse :: ThreadPostResponse -> Int -> UserSanitizedResponse -> Int -> ThreadPostStatResponse -> (Maybe LikeResponse) -> (Maybe StarResponse) -> (Maybe OrganizationResponse) -> (Maybe ForumResponse) -> (Maybe BoardResponse) -> (Maybe ThreadResponse) -> (Maybe (Array Int)) -> (Maybe Int) -> (Maybe Int) -> Permissions -> ThreadPostPackResponse
mkThreadPostPackResponse threadPost threadPostId user userId stat like star withOrganization withForum withBoard withThread withThreadPosts withThreadPostsOffset withThreadPostsLimit permissions =
  ThreadPostPackResponse{threadPost, threadPostId, user, userId, stat, like, star, withOrganization, withForum, withBoard, withThread, withThreadPosts, withThreadPostsOffset, withThreadPostsLimit, permissions}


unwrapThreadPostPackResponse :: ThreadPostPackResponse -> {
  threadPost :: ThreadPostResponse,
  threadPostId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int,
  stat :: ThreadPostStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  withOrganization :: (Maybe OrganizationResponse),
  withForum :: (Maybe ForumResponse),
  withBoard :: (Maybe BoardResponse),
  withThread :: (Maybe ThreadResponse),
  withThreadPosts :: (Maybe (Array Int)),
  withThreadPostsOffset :: (Maybe Int),
  withThreadPostsLimit :: (Maybe Int),
  permissions :: Permissions
}
unwrapThreadPostPackResponse (ThreadPostPackResponse r) = r

instance threadPostPackResponseEncodeJson :: EncodeJson ThreadPostPackResponse where
  encodeJson (ThreadPostPackResponse o) =
       "tag" := "ThreadPostPackResponse"
    ~> "thread_post" := o.threadPost
    ~> "thread_post_id" := o.threadPostId
    ~> "user" := o.user
    ~> "user_id" := o.userId
    ~> "stat" := o.stat
    ~> "like" := o.like
    ~> "star" := o.star
    ~> "with_organization" := o.withOrganization
    ~> "with_forum" := o.withForum
    ~> "with_board" := o.withBoard
    ~> "with_thread" := o.withThread
    ~> "with_thread_posts" := o.withThreadPosts
    ~> "with_thread_posts_offset" := o.withThreadPostsOffset
    ~> "with_thread_posts_limit" := o.withThreadPostsLimit
    ~> "permissions" := o.permissions
    ~> jsonEmptyObject


instance threadPostPackResponseDecodeJson :: DecodeJson ThreadPostPackResponse where
  decodeJson o = do
    obj <- decodeJson o
    threadPost <- obj .? "thread_post"
    threadPostId <- obj .? "thread_post_id"
    user <- obj .? "user"
    userId <- obj .? "user_id"
    stat <- obj .? "stat"
    like <- obj .? "like"
    star <- obj .? "star"
    withOrganization <- obj .? "with_organization"
    withForum <- obj .? "with_forum"
    withBoard <- obj .? "with_board"
    withThread <- obj .? "with_thread"
    withThreadPosts <- obj .? "with_thread_posts"
    withThreadPostsOffset <- obj .? "with_thread_posts_offset"
    withThreadPostsLimit <- obj .? "with_thread_posts_limit"
    permissions <- obj .? "permissions"
    pure $ ThreadPostPackResponse {
      threadPost,
      threadPostId,
      user,
      userId,
      stat,
      like,
      star,
      withOrganization,
      withForum,
      withBoard,
      withThread,
      withThreadPosts,
      withThreadPostsOffset,
      withThreadPostsLimit,
      permissions
    }


instance threadPostPackResponseRequestable :: Requestable ThreadPostPackResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance threadPostPackResponseRespondable :: Respondable ThreadPostPackResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkThreadPostPackResponse
      <$> readProp "thread_post" json
      <*> readProp "thread_post_id" json
      <*> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "stat" json
      <*> (unNullOrUndefined <$> readProp "like" json)
      <*> (unNullOrUndefined <$> readProp "star" json)
      <*> (unNullOrUndefined <$> readProp "with_organization" json)
      <*> (unNullOrUndefined <$> readProp "with_forum" json)
      <*> (unNullOrUndefined <$> readProp "with_board" json)
      <*> (unNullOrUndefined <$> readProp "with_thread" json)
      <*> (unNullOrUndefined <$> readProp "with_thread_posts" json)
      <*> (unNullOrUndefined <$> readProp "with_thread_posts_offset" json)
      <*> (unNullOrUndefined <$> readProp "with_thread_posts_limit" json)
      <*> readProp "permissions" json


instance threadPostPackResponseIsForeign :: IsForeign ThreadPostPackResponse where
  read json =
      mkThreadPostPackResponse
      <$> readProp "thread_post" json
      <*> readProp "thread_post_id" json
      <*> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "stat" json
      <*> (unNullOrUndefined <$> readProp "like" json)
      <*> (unNullOrUndefined <$> readProp "star" json)
      <*> (unNullOrUndefined <$> readProp "with_organization" json)
      <*> (unNullOrUndefined <$> readProp "with_forum" json)
      <*> (unNullOrUndefined <$> readProp "with_board" json)
      <*> (unNullOrUndefined <$> readProp "with_thread" json)
      <*> (unNullOrUndefined <$> readProp "with_thread_posts" json)
      <*> (unNullOrUndefined <$> readProp "with_thread_posts_offset" json)
      <*> (unNullOrUndefined <$> readProp "with_thread_posts_limit" json)
      <*> readProp "permissions" json


newtype ThreadPostPackResponses = ThreadPostPackResponses {
  threadPostPackResponses :: (Array ThreadPostPackResponse)
}


type ThreadPostPackResponsesR = {
  threadPostPackResponses :: (Array ThreadPostPackResponse)
}


_ThreadPostPackResponses :: Lens' ThreadPostPackResponses {
  threadPostPackResponses :: (Array ThreadPostPackResponse)
}
_ThreadPostPackResponses f (ThreadPostPackResponses o) = ThreadPostPackResponses <$> f o


mkThreadPostPackResponses :: (Array ThreadPostPackResponse) -> ThreadPostPackResponses
mkThreadPostPackResponses threadPostPackResponses =
  ThreadPostPackResponses{threadPostPackResponses}


unwrapThreadPostPackResponses :: ThreadPostPackResponses -> {
  threadPostPackResponses :: (Array ThreadPostPackResponse)
}
unwrapThreadPostPackResponses (ThreadPostPackResponses r) = r

instance threadPostPackResponsesEncodeJson :: EncodeJson ThreadPostPackResponses where
  encodeJson (ThreadPostPackResponses o) =
       "tag" := "ThreadPostPackResponses"
    ~> "thread_post_pack_responses" := o.threadPostPackResponses
    ~> jsonEmptyObject


instance threadPostPackResponsesDecodeJson :: DecodeJson ThreadPostPackResponses where
  decodeJson o = do
    obj <- decodeJson o
    threadPostPackResponses <- obj .? "thread_post_pack_responses"
    pure $ ThreadPostPackResponses {
      threadPostPackResponses
    }


instance threadPostPackResponsesRequestable :: Requestable ThreadPostPackResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance threadPostPackResponsesRespondable :: Respondable ThreadPostPackResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkThreadPostPackResponses
      <$> readProp "thread_post_pack_responses" json


instance threadPostPackResponsesIsForeign :: IsForeign ThreadPostPackResponses where
  read json =
      mkThreadPostPackResponses
      <$> readProp "thread_post_pack_responses" json

-- footer