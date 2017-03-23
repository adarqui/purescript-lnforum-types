module LN.T.Forum where
import LN.T.Visibility


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

newtype ForumRequest = ForumRequest {
  displayName :: String,
  description :: (Maybe String),
  threadsPerBoard :: Int,
  threadPostsPerThread :: Int,
  recentThreadsLimit :: Int,
  recentPostsLimit :: Int,
  motwLimit :: Int,
  icon :: (Maybe String),
  tags :: (Array String),
  visibility :: Visibility,
  guard :: Int,
  stateTag :: (Maybe String)
}


type ForumRequestR = {
  displayName :: String,
  description :: (Maybe String),
  threadsPerBoard :: Int,
  threadPostsPerThread :: Int,
  recentThreadsLimit :: Int,
  recentPostsLimit :: Int,
  motwLimit :: Int,
  icon :: (Maybe String),
  tags :: (Array String),
  visibility :: Visibility,
  guard :: Int,
  stateTag :: (Maybe String)
}


_ForumRequest :: Lens' ForumRequest {
  displayName :: String,
  description :: (Maybe String),
  threadsPerBoard :: Int,
  threadPostsPerThread :: Int,
  recentThreadsLimit :: Int,
  recentPostsLimit :: Int,
  motwLimit :: Int,
  icon :: (Maybe String),
  tags :: (Array String),
  visibility :: Visibility,
  guard :: Int,
  stateTag :: (Maybe String)
}
_ForumRequest f (ForumRequest o) = ForumRequest <$> f o


mkForumRequest :: String -> (Maybe String) -> Int -> Int -> Int -> Int -> Int -> (Maybe String) -> (Array String) -> Visibility -> Int -> (Maybe String) -> ForumRequest
mkForumRequest displayName description threadsPerBoard threadPostsPerThread recentThreadsLimit recentPostsLimit motwLimit icon tags visibility guard stateTag =
  ForumRequest{displayName, description, threadsPerBoard, threadPostsPerThread, recentThreadsLimit, recentPostsLimit, motwLimit, icon, tags, visibility, guard, stateTag}


unwrapForumRequest :: ForumRequest -> {
  displayName :: String,
  description :: (Maybe String),
  threadsPerBoard :: Int,
  threadPostsPerThread :: Int,
  recentThreadsLimit :: Int,
  recentPostsLimit :: Int,
  motwLimit :: Int,
  icon :: (Maybe String),
  tags :: (Array String),
  visibility :: Visibility,
  guard :: Int,
  stateTag :: (Maybe String)
}
unwrapForumRequest (ForumRequest r) = r

instance forumRequestEncodeJson :: EncodeJson ForumRequest where
  encodeJson (ForumRequest o) =
       "tag" := "ForumRequest"
    ~> "display_name" := o.displayName
    ~> "description" := o.description
    ~> "threads_per_board" := o.threadsPerBoard
    ~> "thread_posts_per_thread" := o.threadPostsPerThread
    ~> "recent_threads_limit" := o.recentThreadsLimit
    ~> "recent_posts_limit" := o.recentPostsLimit
    ~> "motw_limit" := o.motwLimit
    ~> "icon" := o.icon
    ~> "tags" := o.tags
    ~> "visibility" := o.visibility
    ~> "guard" := o.guard
    ~> "state_tag" := o.stateTag
    ~> jsonEmptyObject


instance forumRequestDecodeJson :: DecodeJson ForumRequest where
  decodeJson o = do
    obj <- decodeJson o
    displayName <- obj .? "display_name"
    description <- obj .? "description"
    threadsPerBoard <- obj .? "threads_per_board"
    threadPostsPerThread <- obj .? "thread_posts_per_thread"
    recentThreadsLimit <- obj .? "recent_threads_limit"
    recentPostsLimit <- obj .? "recent_posts_limit"
    motwLimit <- obj .? "motw_limit"
    icon <- obj .? "icon"
    tags <- obj .? "tags"
    visibility <- obj .? "visibility"
    guard <- obj .? "guard"
    stateTag <- obj .? "state_tag"
    pure $ ForumRequest {
      displayName,
      description,
      threadsPerBoard,
      threadPostsPerThread,
      recentThreadsLimit,
      recentPostsLimit,
      motwLimit,
      icon,
      tags,
      visibility,
      guard,
      stateTag
    }


instance forumRequestRequestable :: Requestable ForumRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance forumRequestRespondable :: Respondable ForumRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkForumRequest
      <$> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "threads_per_board" json
      <*> readProp "thread_posts_per_thread" json
      <*> readProp "recent_threads_limit" json
      <*> readProp "recent_posts_limit" json
      <*> readProp "motw_limit" json
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "tags" json
      <*> readProp "visibility" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "state_tag" json)


instance forumRequestIsForeign :: IsForeign ForumRequest where
  read json =
      mkForumRequest
      <$> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "threads_per_board" json
      <*> readProp "thread_posts_per_thread" json
      <*> readProp "recent_threads_limit" json
      <*> readProp "recent_posts_limit" json
      <*> readProp "motw_limit" json
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "tags" json
      <*> readProp "visibility" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "state_tag" json)


newtype ForumResponse = ForumResponse {
  id :: Int,
  userId :: Int,
  orgId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  threadsPerBoard :: Int,
  threadPostsPerThread :: Int,
  recentThreadsLimit :: Int,
  recentPostsLimit :: Int,
  motwLimit :: Int,
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


type ForumResponseR = {
  id :: Int,
  userId :: Int,
  orgId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  threadsPerBoard :: Int,
  threadPostsPerThread :: Int,
  recentThreadsLimit :: Int,
  recentPostsLimit :: Int,
  motwLimit :: Int,
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


_ForumResponse :: Lens' ForumResponse {
  id :: Int,
  userId :: Int,
  orgId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  threadsPerBoard :: Int,
  threadPostsPerThread :: Int,
  recentThreadsLimit :: Int,
  recentPostsLimit :: Int,
  motwLimit :: Int,
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
_ForumResponse f (ForumResponse o) = ForumResponse <$> f o


mkForumResponse :: Int -> Int -> Int -> String -> String -> (Maybe String) -> Int -> Int -> Int -> Int -> Int -> (Maybe String) -> (Array String) -> Visibility -> Boolean -> Int -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> ForumResponse
mkForumResponse id userId orgId name displayName description threadsPerBoard threadPostsPerThread recentThreadsLimit recentPostsLimit motwLimit icon tags visibility active guard createdAt modifiedBy modifiedAt activityAt =
  ForumResponse{id, userId, orgId, name, displayName, description, threadsPerBoard, threadPostsPerThread, recentThreadsLimit, recentPostsLimit, motwLimit, icon, tags, visibility, active, guard, createdAt, modifiedBy, modifiedAt, activityAt}


unwrapForumResponse :: ForumResponse -> {
  id :: Int,
  userId :: Int,
  orgId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  threadsPerBoard :: Int,
  threadPostsPerThread :: Int,
  recentThreadsLimit :: Int,
  recentPostsLimit :: Int,
  motwLimit :: Int,
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
unwrapForumResponse (ForumResponse r) = r

instance forumResponseEncodeJson :: EncodeJson ForumResponse where
  encodeJson (ForumResponse o) =
       "tag" := "ForumResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "org_id" := o.orgId
    ~> "name" := o.name
    ~> "display_name" := o.displayName
    ~> "description" := o.description
    ~> "threads_per_board" := o.threadsPerBoard
    ~> "thread_posts_per_thread" := o.threadPostsPerThread
    ~> "recent_threads_limit" := o.recentThreadsLimit
    ~> "recent_posts_limit" := o.recentPostsLimit
    ~> "motw_limit" := o.motwLimit
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


instance forumResponseDecodeJson :: DecodeJson ForumResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    orgId <- obj .? "org_id"
    name <- obj .? "name"
    displayName <- obj .? "display_name"
    description <- obj .? "description"
    threadsPerBoard <- obj .? "threads_per_board"
    threadPostsPerThread <- obj .? "thread_posts_per_thread"
    recentThreadsLimit <- obj .? "recent_threads_limit"
    recentPostsLimit <- obj .? "recent_posts_limit"
    motwLimit <- obj .? "motw_limit"
    icon <- obj .? "icon"
    tags <- obj .? "tags"
    visibility <- obj .? "visibility"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedBy <- obj .? "modified_by"
    modifiedAt <- obj .? "modified_at"
    activityAt <- obj .? "activity_at"
    pure $ ForumResponse {
      id,
      userId,
      orgId,
      name,
      displayName,
      description,
      threadsPerBoard,
      threadPostsPerThread,
      recentThreadsLimit,
      recentPostsLimit,
      motwLimit,
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


instance forumResponseRequestable :: Requestable ForumResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance forumResponseRespondable :: Respondable ForumResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkForumResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "org_id" json
      <*> readProp "name" json
      <*> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "threads_per_board" json
      <*> readProp "thread_posts_per_thread" json
      <*> readProp "recent_threads_limit" json
      <*> readProp "recent_posts_limit" json
      <*> readProp "motw_limit" json
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "tags" json
      <*> readProp "visibility" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_by" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


instance forumResponseIsForeign :: IsForeign ForumResponse where
  read json =
      mkForumResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "org_id" json
      <*> readProp "name" json
      <*> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "threads_per_board" json
      <*> readProp "thread_posts_per_thread" json
      <*> readProp "recent_threads_limit" json
      <*> readProp "recent_posts_limit" json
      <*> readProp "motw_limit" json
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "tags" json
      <*> readProp "visibility" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_by" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


newtype ForumResponses = ForumResponses {
  forumResponses :: (Array ForumResponse)
}


type ForumResponsesR = {
  forumResponses :: (Array ForumResponse)
}


_ForumResponses :: Lens' ForumResponses {
  forumResponses :: (Array ForumResponse)
}
_ForumResponses f (ForumResponses o) = ForumResponses <$> f o


mkForumResponses :: (Array ForumResponse) -> ForumResponses
mkForumResponses forumResponses =
  ForumResponses{forumResponses}


unwrapForumResponses :: ForumResponses -> {
  forumResponses :: (Array ForumResponse)
}
unwrapForumResponses (ForumResponses r) = r

instance forumResponsesEncodeJson :: EncodeJson ForumResponses where
  encodeJson (ForumResponses o) =
       "tag" := "ForumResponses"
    ~> "forum_responses" := o.forumResponses
    ~> jsonEmptyObject


instance forumResponsesDecodeJson :: DecodeJson ForumResponses where
  decodeJson o = do
    obj <- decodeJson o
    forumResponses <- obj .? "forum_responses"
    pure $ ForumResponses {
      forumResponses
    }


instance forumResponsesRequestable :: Requestable ForumResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance forumResponsesRespondable :: Respondable ForumResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkForumResponses
      <$> readProp "forum_responses" json


instance forumResponsesIsForeign :: IsForeign ForumResponses where
  read json =
      mkForumResponses
      <$> readProp "forum_responses" json


newtype ForumStatResponse = ForumStatResponse {
  forumId :: Int,
  boards :: Int,
  threads :: Int,
  threadPosts :: Int,
  views :: Int
}


type ForumStatResponseR = {
  forumId :: Int,
  boards :: Int,
  threads :: Int,
  threadPosts :: Int,
  views :: Int
}


_ForumStatResponse :: Lens' ForumStatResponse {
  forumId :: Int,
  boards :: Int,
  threads :: Int,
  threadPosts :: Int,
  views :: Int
}
_ForumStatResponse f (ForumStatResponse o) = ForumStatResponse <$> f o


mkForumStatResponse :: Int -> Int -> Int -> Int -> Int -> ForumStatResponse
mkForumStatResponse forumId boards threads threadPosts views =
  ForumStatResponse{forumId, boards, threads, threadPosts, views}


unwrapForumStatResponse :: ForumStatResponse -> {
  forumId :: Int,
  boards :: Int,
  threads :: Int,
  threadPosts :: Int,
  views :: Int
}
unwrapForumStatResponse (ForumStatResponse r) = r

instance forumStatResponseEncodeJson :: EncodeJson ForumStatResponse where
  encodeJson (ForumStatResponse o) =
       "tag" := "ForumStatResponse"
    ~> "forum_id" := o.forumId
    ~> "boards" := o.boards
    ~> "threads" := o.threads
    ~> "thread_posts" := o.threadPosts
    ~> "views" := o.views
    ~> jsonEmptyObject


instance forumStatResponseDecodeJson :: DecodeJson ForumStatResponse where
  decodeJson o = do
    obj <- decodeJson o
    forumId <- obj .? "forum_id"
    boards <- obj .? "boards"
    threads <- obj .? "threads"
    threadPosts <- obj .? "thread_posts"
    views <- obj .? "views"
    pure $ ForumStatResponse {
      forumId,
      boards,
      threads,
      threadPosts,
      views
    }


instance forumStatResponseRequestable :: Requestable ForumStatResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance forumStatResponseRespondable :: Respondable ForumStatResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkForumStatResponse
      <$> readProp "forum_id" json
      <*> readProp "boards" json
      <*> readProp "threads" json
      <*> readProp "thread_posts" json
      <*> readProp "views" json


instance forumStatResponseIsForeign :: IsForeign ForumStatResponse where
  read json =
      mkForumStatResponse
      <$> readProp "forum_id" json
      <*> readProp "boards" json
      <*> readProp "threads" json
      <*> readProp "thread_posts" json
      <*> readProp "views" json


newtype ForumStatResponses = ForumStatResponses {
  forumStatResponses :: (Array ForumStatResponse)
}


type ForumStatResponsesR = {
  forumStatResponses :: (Array ForumStatResponse)
}


_ForumStatResponses :: Lens' ForumStatResponses {
  forumStatResponses :: (Array ForumStatResponse)
}
_ForumStatResponses f (ForumStatResponses o) = ForumStatResponses <$> f o


mkForumStatResponses :: (Array ForumStatResponse) -> ForumStatResponses
mkForumStatResponses forumStatResponses =
  ForumStatResponses{forumStatResponses}


unwrapForumStatResponses :: ForumStatResponses -> {
  forumStatResponses :: (Array ForumStatResponse)
}
unwrapForumStatResponses (ForumStatResponses r) = r

instance forumStatResponsesEncodeJson :: EncodeJson ForumStatResponses where
  encodeJson (ForumStatResponses o) =
       "tag" := "ForumStatResponses"
    ~> "forum_stat_responses" := o.forumStatResponses
    ~> jsonEmptyObject


instance forumStatResponsesDecodeJson :: DecodeJson ForumStatResponses where
  decodeJson o = do
    obj <- decodeJson o
    forumStatResponses <- obj .? "forum_stat_responses"
    pure $ ForumStatResponses {
      forumStatResponses
    }


instance forumStatResponsesRequestable :: Requestable ForumStatResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance forumStatResponsesRespondable :: Respondable ForumStatResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkForumStatResponses
      <$> readProp "forum_stat_responses" json


instance forumStatResponsesIsForeign :: IsForeign ForumStatResponses where
  read json =
      mkForumStatResponses
      <$> readProp "forum_stat_responses" json

-- footer