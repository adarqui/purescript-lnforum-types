module LN.T.Thread where
import LN.T.Board


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

newtype ThreadRequest = ThreadRequest {
  displayName :: String,
  description :: (Maybe String),
  sticky :: Boolean,
  locked :: Boolean,
  poll :: (Maybe String),
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int,
  stateTag :: (Maybe String)
}


type ThreadRequestR = {
  displayName :: String,
  description :: (Maybe String),
  sticky :: Boolean,
  locked :: Boolean,
  poll :: (Maybe String),
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int,
  stateTag :: (Maybe String)
}


_ThreadRequest :: Lens' ThreadRequest {
  displayName :: String,
  description :: (Maybe String),
  sticky :: Boolean,
  locked :: Boolean,
  poll :: (Maybe String),
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int,
  stateTag :: (Maybe String)
}
_ThreadRequest f (ThreadRequest o) = ThreadRequest <$> f o


mkThreadRequest :: String -> (Maybe String) -> Boolean -> Boolean -> (Maybe String) -> (Maybe String) -> (Array String) -> Int -> (Maybe String) -> ThreadRequest
mkThreadRequest displayName description sticky locked poll icon tags guard stateTag =
  ThreadRequest{displayName, description, sticky, locked, poll, icon, tags, guard, stateTag}


unwrapThreadRequest :: ThreadRequest -> {
  displayName :: String,
  description :: (Maybe String),
  sticky :: Boolean,
  locked :: Boolean,
  poll :: (Maybe String),
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int,
  stateTag :: (Maybe String)
}
unwrapThreadRequest (ThreadRequest r) = r

instance threadRequestEncodeJson :: EncodeJson ThreadRequest where
  encodeJson (ThreadRequest o) =
       "tag" := "ThreadRequest"
    ~> "display_name" := o.displayName
    ~> "description" := o.description
    ~> "sticky" := o.sticky
    ~> "locked" := o.locked
    ~> "poll" := o.poll
    ~> "icon" := o.icon
    ~> "tags" := o.tags
    ~> "guard" := o.guard
    ~> "state_tag" := o.stateTag
    ~> jsonEmptyObject


instance threadRequestDecodeJson :: DecodeJson ThreadRequest where
  decodeJson o = do
    obj <- decodeJson o
    displayName <- obj .? "display_name"
    description <- obj .? "description"
    sticky <- obj .? "sticky"
    locked <- obj .? "locked"
    poll <- obj .? "poll"
    icon <- obj .? "icon"
    tags <- obj .? "tags"
    guard <- obj .? "guard"
    stateTag <- obj .? "state_tag"
    pure $ ThreadRequest {
      displayName,
      description,
      sticky,
      locked,
      poll,
      icon,
      tags,
      guard,
      stateTag
    }


instance threadRequestRequestable :: Requestable ThreadRequest where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance threadRequestRespondable :: Respondable ThreadRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype ThreadResponse = ThreadResponse {
  id :: Int,
  userId :: Int,
  boardId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  sticky :: Boolean,
  locked :: Boolean,
  poll :: (Maybe String),
  icon :: (Maybe String),
  tags :: (Array String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


type ThreadResponseR = {
  id :: Int,
  userId :: Int,
  boardId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  sticky :: Boolean,
  locked :: Boolean,
  poll :: (Maybe String),
  icon :: (Maybe String),
  tags :: (Array String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


_ThreadResponse :: Lens' ThreadResponse {
  id :: Int,
  userId :: Int,
  boardId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  sticky :: Boolean,
  locked :: Boolean,
  poll :: (Maybe String),
  icon :: (Maybe String),
  tags :: (Array String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
_ThreadResponse f (ThreadResponse o) = ThreadResponse <$> f o


mkThreadResponse :: Int -> Int -> Int -> String -> String -> (Maybe String) -> Boolean -> Boolean -> (Maybe String) -> (Maybe String) -> (Array String) -> Boolean -> Int -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> ThreadResponse
mkThreadResponse id userId boardId name displayName description sticky locked poll icon tags active guard createdAt modifiedBy modifiedAt activityAt =
  ThreadResponse{id, userId, boardId, name, displayName, description, sticky, locked, poll, icon, tags, active, guard, createdAt, modifiedBy, modifiedAt, activityAt}


unwrapThreadResponse :: ThreadResponse -> {
  id :: Int,
  userId :: Int,
  boardId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  sticky :: Boolean,
  locked :: Boolean,
  poll :: (Maybe String),
  icon :: (Maybe String),
  tags :: (Array String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
unwrapThreadResponse (ThreadResponse r) = r

instance threadResponseEncodeJson :: EncodeJson ThreadResponse where
  encodeJson (ThreadResponse o) =
       "tag" := "ThreadResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "board_id" := o.boardId
    ~> "name" := o.name
    ~> "display_name" := o.displayName
    ~> "description" := o.description
    ~> "sticky" := o.sticky
    ~> "locked" := o.locked
    ~> "poll" := o.poll
    ~> "icon" := o.icon
    ~> "tags" := o.tags
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_by" := o.modifiedBy
    ~> "modified_at" := o.modifiedAt
    ~> "activity_at" := o.activityAt
    ~> jsonEmptyObject


instance threadResponseDecodeJson :: DecodeJson ThreadResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    boardId <- obj .? "board_id"
    name <- obj .? "name"
    displayName <- obj .? "display_name"
    description <- obj .? "description"
    sticky <- obj .? "sticky"
    locked <- obj .? "locked"
    poll <- obj .? "poll"
    icon <- obj .? "icon"
    tags <- obj .? "tags"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedBy <- obj .? "modified_by"
    modifiedAt <- obj .? "modified_at"
    activityAt <- obj .? "activity_at"
    pure $ ThreadResponse {
      id,
      userId,
      boardId,
      name,
      displayName,
      description,
      sticky,
      locked,
      poll,
      icon,
      tags,
      active,
      guard,
      createdAt,
      modifiedBy,
      modifiedAt,
      activityAt
    }


instance threadResponseRequestable :: Requestable ThreadResponse where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance threadResponseRespondable :: Respondable ThreadResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype ThreadResponses = ThreadResponses {
  threadResponses :: (Array ThreadResponse)
}


type ThreadResponsesR = {
  threadResponses :: (Array ThreadResponse)
}


_ThreadResponses :: Lens' ThreadResponses {
  threadResponses :: (Array ThreadResponse)
}
_ThreadResponses f (ThreadResponses o) = ThreadResponses <$> f o


mkThreadResponses :: (Array ThreadResponse) -> ThreadResponses
mkThreadResponses threadResponses =
  ThreadResponses{threadResponses}


unwrapThreadResponses :: ThreadResponses -> {
  threadResponses :: (Array ThreadResponse)
}
unwrapThreadResponses (ThreadResponses r) = r

instance threadResponsesEncodeJson :: EncodeJson ThreadResponses where
  encodeJson (ThreadResponses o) =
       "tag" := "ThreadResponses"
    ~> "thread_responses" := o.threadResponses
    ~> jsonEmptyObject


instance threadResponsesDecodeJson :: DecodeJson ThreadResponses where
  decodeJson o = do
    obj <- decodeJson o
    threadResponses <- obj .? "thread_responses"
    pure $ ThreadResponses {
      threadResponses
    }


instance threadResponsesRequestable :: Requestable ThreadResponses where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance threadResponsesRespondable :: Respondable ThreadResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype ThreadStatResponse = ThreadStatResponse {
  threadId :: Int,
  threadPosts :: Int,
  views :: Int
}


type ThreadStatResponseR = {
  threadId :: Int,
  threadPosts :: Int,
  views :: Int
}


_ThreadStatResponse :: Lens' ThreadStatResponse {
  threadId :: Int,
  threadPosts :: Int,
  views :: Int
}
_ThreadStatResponse f (ThreadStatResponse o) = ThreadStatResponse <$> f o


mkThreadStatResponse :: Int -> Int -> Int -> ThreadStatResponse
mkThreadStatResponse threadId threadPosts views =
  ThreadStatResponse{threadId, threadPosts, views}


unwrapThreadStatResponse :: ThreadStatResponse -> {
  threadId :: Int,
  threadPosts :: Int,
  views :: Int
}
unwrapThreadStatResponse (ThreadStatResponse r) = r

instance threadStatResponseEncodeJson :: EncodeJson ThreadStatResponse where
  encodeJson (ThreadStatResponse o) =
       "tag" := "ThreadStatResponse"
    ~> "thread_id" := o.threadId
    ~> "thread_posts" := o.threadPosts
    ~> "views" := o.views
    ~> jsonEmptyObject


instance threadStatResponseDecodeJson :: DecodeJson ThreadStatResponse where
  decodeJson o = do
    obj <- decodeJson o
    threadId <- obj .? "thread_id"
    threadPosts <- obj .? "thread_posts"
    views <- obj .? "views"
    pure $ ThreadStatResponse {
      threadId,
      threadPosts,
      views
    }


instance threadStatResponseRequestable :: Requestable ThreadStatResponse where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance threadStatResponseRespondable :: Respondable ThreadStatResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype ThreadStatResponses = ThreadStatResponses {
  threadStatResponses :: (Array ThreadStatResponse)
}


type ThreadStatResponsesR = {
  threadStatResponses :: (Array ThreadStatResponse)
}


_ThreadStatResponses :: Lens' ThreadStatResponses {
  threadStatResponses :: (Array ThreadStatResponse)
}
_ThreadStatResponses f (ThreadStatResponses o) = ThreadStatResponses <$> f o


mkThreadStatResponses :: (Array ThreadStatResponse) -> ThreadStatResponses
mkThreadStatResponses threadStatResponses =
  ThreadStatResponses{threadStatResponses}


unwrapThreadStatResponses :: ThreadStatResponses -> {
  threadStatResponses :: (Array ThreadStatResponse)
}
unwrapThreadStatResponses (ThreadStatResponses r) = r

instance threadStatResponsesEncodeJson :: EncodeJson ThreadStatResponses where
  encodeJson (ThreadStatResponses o) =
       "tag" := "ThreadStatResponses"
    ~> "thread_stat_responses" := o.threadStatResponses
    ~> jsonEmptyObject


instance threadStatResponsesDecodeJson :: DecodeJson ThreadStatResponses where
  decodeJson o = do
    obj <- decodeJson o
    threadStatResponses <- obj .? "thread_stat_responses"
    pure $ ThreadStatResponses {
      threadStatResponses
    }


instance threadStatResponsesRequestable :: Requestable ThreadStatResponses where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance threadStatResponsesRespondable :: Respondable ThreadStatResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson

-- footer