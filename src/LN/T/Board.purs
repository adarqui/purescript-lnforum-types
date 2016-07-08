module LN.T.Board where



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

newtype BoardRequest = BoardRequest {
  displayName :: String,
  description :: (Maybe String),
  isAnonymous :: Boolean,
  canCreateSubBoards :: Boolean,
  canCreateThreads :: Boolean,
  suggestedTags :: (Array String),
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int
}


type BoardRequestR = {
  displayName :: String,
  description :: (Maybe String),
  isAnonymous :: Boolean,
  canCreateSubBoards :: Boolean,
  canCreateThreads :: Boolean,
  suggestedTags :: (Array String),
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int
}


_BoardRequest :: Lens' BoardRequest {
  displayName :: String,
  description :: (Maybe String),
  isAnonymous :: Boolean,
  canCreateSubBoards :: Boolean,
  canCreateThreads :: Boolean,
  suggestedTags :: (Array String),
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int
}
_BoardRequest f (BoardRequest o) = BoardRequest <$> f o


mkBoardRequest :: String -> (Maybe String) -> Boolean -> Boolean -> Boolean -> (Array String) -> (Maybe String) -> (Array String) -> Int -> BoardRequest
mkBoardRequest displayName description isAnonymous canCreateSubBoards canCreateThreads suggestedTags icon tags guard =
  BoardRequest{displayName, description, isAnonymous, canCreateSubBoards, canCreateThreads, suggestedTags, icon, tags, guard}


unwrapBoardRequest :: BoardRequest -> {
  displayName :: String,
  description :: (Maybe String),
  isAnonymous :: Boolean,
  canCreateSubBoards :: Boolean,
  canCreateThreads :: Boolean,
  suggestedTags :: (Array String),
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int
}
unwrapBoardRequest (BoardRequest r) = r

instance boardRequestEncodeJson :: EncodeJson BoardRequest where
  encodeJson (BoardRequest o) =
       "tag" := "BoardRequest"
    ~> "display_name" := o.displayName
    ~> "description" := o.description
    ~> "is_anonymous" := o.isAnonymous
    ~> "can_create_sub_boards" := o.canCreateSubBoards
    ~> "can_create_threads" := o.canCreateThreads
    ~> "suggested_tags" := o.suggestedTags
    ~> "icon" := o.icon
    ~> "tags" := o.tags
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance boardRequestDecodeJson :: DecodeJson BoardRequest where
  decodeJson o = do
    obj <- decodeJson o
    displayName <- obj .? "display_name"
    description <- obj .? "description"
    isAnonymous <- obj .? "is_anonymous"
    canCreateSubBoards <- obj .? "can_create_sub_boards"
    canCreateThreads <- obj .? "can_create_threads"
    suggestedTags <- obj .? "suggested_tags"
    icon <- obj .? "icon"
    tags <- obj .? "tags"
    guard <- obj .? "guard"
    pure $ BoardRequest {
      displayName,
      description,
      isAnonymous,
      canCreateSubBoards,
      canCreateThreads,
      suggestedTags,
      icon,
      tags,
      guard
    }


instance boardRequestRequestable :: Requestable BoardRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance boardRequestRespondable :: Respondable BoardRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkBoardRequest
      <$> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "is_anonymous" json
      <*> readProp "can_create_sub_boards" json
      <*> readProp "can_create_threads" json
      <*> readProp "suggested_tags" json
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "tags" json
      <*> readProp "guard" json


instance boardRequestIsForeign :: IsForeign BoardRequest where
  read json =
      mkBoardRequest
      <$> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "is_anonymous" json
      <*> readProp "can_create_sub_boards" json
      <*> readProp "can_create_threads" json
      <*> readProp "suggested_tags" json
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "tags" json
      <*> readProp "guard" json


newtype BoardResponse = BoardResponse {
  id :: Int,
  userId :: Int,
  orgId :: Int,
  forumId :: Int,
  parentId :: (Maybe Int),
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  isAnonymous :: Boolean,
  canCreateSubBoards :: Boolean,
  canCreateThreads :: Boolean,
  suggestedTags :: (Array String),
  icon :: (Maybe String),
  tags :: (Array String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


type BoardResponseR = {
  id :: Int,
  userId :: Int,
  orgId :: Int,
  forumId :: Int,
  parentId :: (Maybe Int),
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  isAnonymous :: Boolean,
  canCreateSubBoards :: Boolean,
  canCreateThreads :: Boolean,
  suggestedTags :: (Array String),
  icon :: (Maybe String),
  tags :: (Array String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


_BoardResponse :: Lens' BoardResponse {
  id :: Int,
  userId :: Int,
  orgId :: Int,
  forumId :: Int,
  parentId :: (Maybe Int),
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  isAnonymous :: Boolean,
  canCreateSubBoards :: Boolean,
  canCreateThreads :: Boolean,
  suggestedTags :: (Array String),
  icon :: (Maybe String),
  tags :: (Array String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
_BoardResponse f (BoardResponse o) = BoardResponse <$> f o


mkBoardResponse :: Int -> Int -> Int -> Int -> (Maybe Int) -> String -> String -> (Maybe String) -> Boolean -> Boolean -> Boolean -> (Array String) -> (Maybe String) -> (Array String) -> Boolean -> Int -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> BoardResponse
mkBoardResponse id userId orgId forumId parentId name displayName description isAnonymous canCreateSubBoards canCreateThreads suggestedTags icon tags active guard createdAt modifiedBy modifiedAt activityAt =
  BoardResponse{id, userId, orgId, forumId, parentId, name, displayName, description, isAnonymous, canCreateSubBoards, canCreateThreads, suggestedTags, icon, tags, active, guard, createdAt, modifiedBy, modifiedAt, activityAt}


unwrapBoardResponse :: BoardResponse -> {
  id :: Int,
  userId :: Int,
  orgId :: Int,
  forumId :: Int,
  parentId :: (Maybe Int),
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  isAnonymous :: Boolean,
  canCreateSubBoards :: Boolean,
  canCreateThreads :: Boolean,
  suggestedTags :: (Array String),
  icon :: (Maybe String),
  tags :: (Array String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
unwrapBoardResponse (BoardResponse r) = r

instance boardResponseEncodeJson :: EncodeJson BoardResponse where
  encodeJson (BoardResponse o) =
       "tag" := "BoardResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "org_id" := o.orgId
    ~> "forum_id" := o.forumId
    ~> "parent_id" := o.parentId
    ~> "name" := o.name
    ~> "display_name" := o.displayName
    ~> "description" := o.description
    ~> "is_anonymous" := o.isAnonymous
    ~> "can_create_sub_boards" := o.canCreateSubBoards
    ~> "can_create_threads" := o.canCreateThreads
    ~> "suggested_tags" := o.suggestedTags
    ~> "icon" := o.icon
    ~> "tags" := o.tags
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_by" := o.modifiedBy
    ~> "modified_at" := o.modifiedAt
    ~> "activity_at" := o.activityAt
    ~> jsonEmptyObject


instance boardResponseDecodeJson :: DecodeJson BoardResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    orgId <- obj .? "org_id"
    forumId <- obj .? "forum_id"
    parentId <- obj .? "parent_id"
    name <- obj .? "name"
    displayName <- obj .? "display_name"
    description <- obj .? "description"
    isAnonymous <- obj .? "is_anonymous"
    canCreateSubBoards <- obj .? "can_create_sub_boards"
    canCreateThreads <- obj .? "can_create_threads"
    suggestedTags <- obj .? "suggested_tags"
    icon <- obj .? "icon"
    tags <- obj .? "tags"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedBy <- obj .? "modified_by"
    modifiedAt <- obj .? "modified_at"
    activityAt <- obj .? "activity_at"
    pure $ BoardResponse {
      id,
      userId,
      orgId,
      forumId,
      parentId,
      name,
      displayName,
      description,
      isAnonymous,
      canCreateSubBoards,
      canCreateThreads,
      suggestedTags,
      icon,
      tags,
      active,
      guard,
      createdAt,
      modifiedBy,
      modifiedAt,
      activityAt
    }


instance boardResponseRequestable :: Requestable BoardResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance boardResponseRespondable :: Respondable BoardResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkBoardResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "org_id" json
      <*> readProp "forum_id" json
      <*> (unNullOrUndefined <$> readProp "parent_id" json)
      <*> readProp "name" json
      <*> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "is_anonymous" json
      <*> readProp "can_create_sub_boards" json
      <*> readProp "can_create_threads" json
      <*> readProp "suggested_tags" json
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "tags" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_by" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


instance boardResponseIsForeign :: IsForeign BoardResponse where
  read json =
      mkBoardResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "org_id" json
      <*> readProp "forum_id" json
      <*> (unNullOrUndefined <$> readProp "parent_id" json)
      <*> readProp "name" json
      <*> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "is_anonymous" json
      <*> readProp "can_create_sub_boards" json
      <*> readProp "can_create_threads" json
      <*> readProp "suggested_tags" json
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "tags" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_by" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


newtype BoardResponses = BoardResponses {
  boardResponses :: (Array BoardResponse)
}


type BoardResponsesR = {
  boardResponses :: (Array BoardResponse)
}


_BoardResponses :: Lens' BoardResponses {
  boardResponses :: (Array BoardResponse)
}
_BoardResponses f (BoardResponses o) = BoardResponses <$> f o


mkBoardResponses :: (Array BoardResponse) -> BoardResponses
mkBoardResponses boardResponses =
  BoardResponses{boardResponses}


unwrapBoardResponses :: BoardResponses -> {
  boardResponses :: (Array BoardResponse)
}
unwrapBoardResponses (BoardResponses r) = r

instance boardResponsesEncodeJson :: EncodeJson BoardResponses where
  encodeJson (BoardResponses o) =
       "tag" := "BoardResponses"
    ~> "board_responses" := o.boardResponses
    ~> jsonEmptyObject


instance boardResponsesDecodeJson :: DecodeJson BoardResponses where
  decodeJson o = do
    obj <- decodeJson o
    boardResponses <- obj .? "board_responses"
    pure $ BoardResponses {
      boardResponses
    }


instance boardResponsesRequestable :: Requestable BoardResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance boardResponsesRespondable :: Respondable BoardResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkBoardResponses
      <$> readProp "board_responses" json


instance boardResponsesIsForeign :: IsForeign BoardResponses where
  read json =
      mkBoardResponses
      <$> readProp "board_responses" json


newtype BoardStatResponse = BoardStatResponse {
  boardId :: Int,
  threads :: Int,
  threadPosts :: Int,
  views :: Int
}


type BoardStatResponseR = {
  boardId :: Int,
  threads :: Int,
  threadPosts :: Int,
  views :: Int
}


_BoardStatResponse :: Lens' BoardStatResponse {
  boardId :: Int,
  threads :: Int,
  threadPosts :: Int,
  views :: Int
}
_BoardStatResponse f (BoardStatResponse o) = BoardStatResponse <$> f o


mkBoardStatResponse :: Int -> Int -> Int -> Int -> BoardStatResponse
mkBoardStatResponse boardId threads threadPosts views =
  BoardStatResponse{boardId, threads, threadPosts, views}


unwrapBoardStatResponse :: BoardStatResponse -> {
  boardId :: Int,
  threads :: Int,
  threadPosts :: Int,
  views :: Int
}
unwrapBoardStatResponse (BoardStatResponse r) = r

instance boardStatResponseEncodeJson :: EncodeJson BoardStatResponse where
  encodeJson (BoardStatResponse o) =
       "tag" := "BoardStatResponse"
    ~> "board_id" := o.boardId
    ~> "threads" := o.threads
    ~> "thread_posts" := o.threadPosts
    ~> "views" := o.views
    ~> jsonEmptyObject


instance boardStatResponseDecodeJson :: DecodeJson BoardStatResponse where
  decodeJson o = do
    obj <- decodeJson o
    boardId <- obj .? "board_id"
    threads <- obj .? "threads"
    threadPosts <- obj .? "thread_posts"
    views <- obj .? "views"
    pure $ BoardStatResponse {
      boardId,
      threads,
      threadPosts,
      views
    }


instance boardStatResponseRequestable :: Requestable BoardStatResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance boardStatResponseRespondable :: Respondable BoardStatResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkBoardStatResponse
      <$> readProp "board_id" json
      <*> readProp "threads" json
      <*> readProp "thread_posts" json
      <*> readProp "views" json


instance boardStatResponseIsForeign :: IsForeign BoardStatResponse where
  read json =
      mkBoardStatResponse
      <$> readProp "board_id" json
      <*> readProp "threads" json
      <*> readProp "thread_posts" json
      <*> readProp "views" json


newtype BoardStatResponses = BoardStatResponses {
  boardStatResponses :: (Array BoardStatResponse)
}


type BoardStatResponsesR = {
  boardStatResponses :: (Array BoardStatResponse)
}


_BoardStatResponses :: Lens' BoardStatResponses {
  boardStatResponses :: (Array BoardStatResponse)
}
_BoardStatResponses f (BoardStatResponses o) = BoardStatResponses <$> f o


mkBoardStatResponses :: (Array BoardStatResponse) -> BoardStatResponses
mkBoardStatResponses boardStatResponses =
  BoardStatResponses{boardStatResponses}


unwrapBoardStatResponses :: BoardStatResponses -> {
  boardStatResponses :: (Array BoardStatResponse)
}
unwrapBoardStatResponses (BoardStatResponses r) = r

instance boardStatResponsesEncodeJson :: EncodeJson BoardStatResponses where
  encodeJson (BoardStatResponses o) =
       "tag" := "BoardStatResponses"
    ~> "board_stat_responses" := o.boardStatResponses
    ~> jsonEmptyObject


instance boardStatResponsesDecodeJson :: DecodeJson BoardStatResponses where
  decodeJson o = do
    obj <- decodeJson o
    boardStatResponses <- obj .? "board_stat_responses"
    pure $ BoardStatResponses {
      boardStatResponses
    }


instance boardStatResponsesRequestable :: Requestable BoardStatResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance boardStatResponsesRespondable :: Respondable BoardStatResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkBoardStatResponses
      <$> readProp "board_stat_responses" json


instance boardStatResponsesIsForeign :: IsForeign BoardStatResponses where
  read json =
      mkBoardStatResponses
      <$> readProp "board_stat_responses" json

-- footer