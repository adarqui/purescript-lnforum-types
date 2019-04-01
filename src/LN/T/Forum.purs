module LN.T.Forum where
import LN.T.Visibility


import Control.Monad.Except.Trans       (runExceptT)
import Data.Argonaut.Core               (jsonEmptyObject, stringify)
import Data.Argonaut.Decode             (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode             (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Date.Helpers                (Date)
import Data.Either                      (Either(..), either)
import Data.Foreign                     (ForeignError(..), fail, unsafeFromForeign, toForeign)
import Data.Foreign.NullOrUndefined     (unNullOrUndefined)
import Data.Foreign.Class               (class Decode, decode)
import Data.Foreign.Helpers
import Data.Maybe                       (Maybe(..))
import Data.Tuple                       (Tuple(..))
import Purescript.Api.Helpers           (class QueryParam, qp)
import Network.HTTP.Affjax.Request      (class Requestable, toRequest)
import Network.HTTP.Affjax.Response     (class Respondable, ResponseType(..))
import Optic.Core                       ((^.), (..))
import Optic.Types                      (Lens, Lens')
import Prelude                          (class Show, show, class Eq, eq, pure, bind, const, ($), (<>), (<$>), (<*>), (==), (&&), (<<<))
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
  guard :: Int
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
  guard :: Int
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
  guard :: Int
}
_ForumRequest f (ForumRequest o) = ForumRequest <$> f o


mkForumRequest :: String -> (Maybe String) -> Int -> Int -> Int -> Int -> Int -> (Maybe String) -> (Array String) -> Visibility -> Int -> ForumRequest
mkForumRequest displayName description threadsPerBoard threadPostsPerThread recentThreadsLimit recentPostsLimit motwLimit icon tags visibility guard =
  ForumRequest{displayName, description, threadsPerBoard, threadPostsPerThread, recentThreadsLimit, recentPostsLimit, motwLimit, icon, tags, visibility, guard}


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
  guard :: Int
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
      guard
    }


instance forumRequestRequestable :: Requestable ForumRequest where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance forumRequestRespondable :: Respondable ForumRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype ForumResponse = ForumResponse {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  threadsPerBoard :: Int,
  threadPostsPerThread :: Int,
  recentThreadsLimit :: Int,
  recentPostsLimit :: Int,
  motwLimit :: Int,
  icon :: (Maybe String),
  visibility :: Visibility,
  tags :: (Array String),
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


type ForumResponseR = {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  threadsPerBoard :: Int,
  threadPostsPerThread :: Int,
  recentThreadsLimit :: Int,
  recentPostsLimit :: Int,
  motwLimit :: Int,
  icon :: (Maybe String),
  visibility :: Visibility,
  tags :: (Array String),
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


_ForumResponse :: Lens' ForumResponse {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  threadsPerBoard :: Int,
  threadPostsPerThread :: Int,
  recentThreadsLimit :: Int,
  recentPostsLimit :: Int,
  motwLimit :: Int,
  icon :: (Maybe String),
  visibility :: Visibility,
  tags :: (Array String),
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
_ForumResponse f (ForumResponse o) = ForumResponse <$> f o


mkForumResponse :: Int -> Int -> String -> String -> (Maybe String) -> Int -> Int -> Int -> Int -> Int -> (Maybe String) -> Visibility -> (Array String) -> Int -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> ForumResponse
mkForumResponse id userId name displayName description threadsPerBoard threadPostsPerThread recentThreadsLimit recentPostsLimit motwLimit icon visibility tags guard createdAt modifiedBy modifiedAt activityAt =
  ForumResponse{id, userId, name, displayName, description, threadsPerBoard, threadPostsPerThread, recentThreadsLimit, recentPostsLimit, motwLimit, icon, visibility, tags, guard, createdAt, modifiedBy, modifiedAt, activityAt}


unwrapForumResponse :: ForumResponse -> {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  threadsPerBoard :: Int,
  threadPostsPerThread :: Int,
  recentThreadsLimit :: Int,
  recentPostsLimit :: Int,
  motwLimit :: Int,
  icon :: (Maybe String),
  visibility :: Visibility,
  tags :: (Array String),
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
    ~> "name" := o.name
    ~> "display_name" := o.displayName
    ~> "description" := o.description
    ~> "threads_per_board" := o.threadsPerBoard
    ~> "thread_posts_per_thread" := o.threadPostsPerThread
    ~> "recent_threads_limit" := o.recentThreadsLimit
    ~> "recent_posts_limit" := o.recentPostsLimit
    ~> "motw_limit" := o.motwLimit
    ~> "icon" := o.icon
    ~> "visibility" := o.visibility
    ~> "tags" := o.tags
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
    name <- obj .? "name"
    displayName <- obj .? "display_name"
    description <- obj .? "description"
    threadsPerBoard <- obj .? "threads_per_board"
    threadPostsPerThread <- obj .? "thread_posts_per_thread"
    recentThreadsLimit <- obj .? "recent_threads_limit"
    recentPostsLimit <- obj .? "recent_posts_limit"
    motwLimit <- obj .? "motw_limit"
    icon <- obj .? "icon"
    visibility <- obj .? "visibility"
    tags <- obj .? "tags"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedBy <- obj .? "modified_by"
    modifiedAt <- obj .? "modified_at"
    activityAt <- obj .? "activity_at"
    pure $ ForumResponse {
      id,
      userId,
      name,
      displayName,
      description,
      threadsPerBoard,
      threadPostsPerThread,
      recentThreadsLimit,
      recentPostsLimit,
      motwLimit,
      icon,
      visibility,
      tags,
      guard,
      createdAt,
      modifiedBy,
      modifiedAt,
      activityAt
    }


instance forumResponseRequestable :: Requestable ForumResponse where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance forumResponseRespondable :: Respondable ForumResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


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
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance forumResponsesRespondable :: Respondable ForumResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype ForumStatResponse = ForumStatResponse {
  boards :: Int,
  threads :: Int,
  threadPosts :: Int,
  views :: Int
}


type ForumStatResponseR = {
  boards :: Int,
  threads :: Int,
  threadPosts :: Int,
  views :: Int
}


_ForumStatResponse :: Lens' ForumStatResponse {
  boards :: Int,
  threads :: Int,
  threadPosts :: Int,
  views :: Int
}
_ForumStatResponse f (ForumStatResponse o) = ForumStatResponse <$> f o


mkForumStatResponse :: Int -> Int -> Int -> Int -> ForumStatResponse
mkForumStatResponse boards threads threadPosts views =
  ForumStatResponse{boards, threads, threadPosts, views}


unwrapForumStatResponse :: ForumStatResponse -> {
  boards :: Int,
  threads :: Int,
  threadPosts :: Int,
  views :: Int
}
unwrapForumStatResponse (ForumStatResponse r) = r

instance forumStatResponseEncodeJson :: EncodeJson ForumStatResponse where
  encodeJson (ForumStatResponse o) =
       "tag" := "ForumStatResponse"
    ~> "boards" := o.boards
    ~> "threads" := o.threads
    ~> "thread_posts" := o.threadPosts
    ~> "views" := o.views
    ~> jsonEmptyObject


instance forumStatResponseDecodeJson :: DecodeJson ForumStatResponse where
  decodeJson o = do
    obj <- decodeJson o
    boards <- obj .? "boards"
    threads <- obj .? "threads"
    threadPosts <- obj .? "thread_posts"
    views <- obj .? "views"
    pure $ ForumStatResponse {
      boards,
      threads,
      threadPosts,
      views
    }


instance forumStatResponseRequestable :: Requestable ForumStatResponse where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance forumStatResponseRespondable :: Respondable ForumStatResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson

-- footer