module LN.T.Board where
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

data BoardType
  = FixMe 



instance boardTypeEncodeJson :: EncodeJson BoardType where
  encodeJson (FixMe ) =
       "tag" := "FixMe"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance boardTypeDecodeJson :: DecodeJson BoardType where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "FixMe" -> do
        pure FixMe

      _ -> Left $ "DecodeJson TypeMismatch for BoardType"



instance boardTypeRequestable :: Requestable BoardType where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance boardTypeRespondable :: Respondable BoardType where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "FixMe" -> do
        pure FixMe

      _ -> fail $ TypeMismatch "BoardType" "Respondable"



data TyBoardType
  = TyFixMe 



instance tyBoardTypeEncodeJson :: EncodeJson TyBoardType where
  encodeJson (TyFixMe ) =
       "tag" := "TyFixMe"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance tyBoardTypeDecodeJson :: DecodeJson TyBoardType where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "TyFixMe" -> do
        pure TyFixMe

      _ -> Left $ "DecodeJson TypeMismatch for TyBoardType"



instance tyBoardTypeRequestable :: Requestable TyBoardType where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance tyBoardTypeRespondable :: Respondable TyBoardType where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "TyFixMe" -> do
        pure TyFixMe

      _ -> fail $ TypeMismatch "TyBoardType" "Respondable"



instance tyBoardTypeEq :: Eq TyBoardType where
  eq TyFixMe TyFixMe = true


instance tyBoardTypeShow :: Show TyBoardType where
  show TyFixMe = "ty_fix_me"


newtype BoardRequest = BoardRequest {
  displayName :: String,
  description :: (Maybe String),
  boardType :: BoardType,
  active :: Boolean,
  isAnonymous :: Boolean,
  canCreateBoards :: Boolean,
  canCreateThreads :: Boolean,
  visibility :: Visibility,
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int
}


type BoardRequestR = {
  displayName :: String,
  description :: (Maybe String),
  boardType :: BoardType,
  active :: Boolean,
  isAnonymous :: Boolean,
  canCreateBoards :: Boolean,
  canCreateThreads :: Boolean,
  visibility :: Visibility,
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int
}


_BoardRequest :: Lens' BoardRequest {
  displayName :: String,
  description :: (Maybe String),
  boardType :: BoardType,
  active :: Boolean,
  isAnonymous :: Boolean,
  canCreateBoards :: Boolean,
  canCreateThreads :: Boolean,
  visibility :: Visibility,
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int
}
_BoardRequest f (BoardRequest o) = BoardRequest <$> f o


mkBoardRequest :: String -> (Maybe String) -> BoardType -> Boolean -> Boolean -> Boolean -> Boolean -> Visibility -> (Maybe String) -> (Array String) -> Int -> BoardRequest
mkBoardRequest displayName description boardType active isAnonymous canCreateBoards canCreateThreads visibility icon tags guard =
  BoardRequest{displayName, description, boardType, active, isAnonymous, canCreateBoards, canCreateThreads, visibility, icon, tags, guard}


unwrapBoardRequest :: BoardRequest -> {
  displayName :: String,
  description :: (Maybe String),
  boardType :: BoardType,
  active :: Boolean,
  isAnonymous :: Boolean,
  canCreateBoards :: Boolean,
  canCreateThreads :: Boolean,
  visibility :: Visibility,
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
    ~> "board_type" := o.boardType
    ~> "active" := o.active
    ~> "is_anonymous" := o.isAnonymous
    ~> "can_create_boards" := o.canCreateBoards
    ~> "can_create_threads" := o.canCreateThreads
    ~> "visibility" := o.visibility
    ~> "icon" := o.icon
    ~> "tags" := o.tags
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance boardRequestDecodeJson :: DecodeJson BoardRequest where
  decodeJson o = do
    obj <- decodeJson o
    displayName <- obj .? "display_name"
    description <- obj .? "description"
    boardType <- obj .? "board_type"
    active <- obj .? "active"
    isAnonymous <- obj .? "is_anonymous"
    canCreateBoards <- obj .? "can_create_boards"
    canCreateThreads <- obj .? "can_create_threads"
    visibility <- obj .? "visibility"
    icon <- obj .? "icon"
    tags <- obj .? "tags"
    guard <- obj .? "guard"
    pure $ BoardRequest {
      displayName,
      description,
      boardType,
      active,
      isAnonymous,
      canCreateBoards,
      canCreateThreads,
      visibility,
      icon,
      tags,
      guard
    }


instance boardRequestRequestable :: Requestable BoardRequest where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance boardRequestRespondable :: Respondable BoardRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype BoardResponse = BoardResponse {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  boardType :: BoardType,
  active :: Boolean,
  isAnonymous :: Boolean,
  canCreateBoards :: Boolean,
  canCreateThreads :: Boolean,
  visibility :: Visibility,
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  activityAt :: (Maybe Date)
}


type BoardResponseR = {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  boardType :: BoardType,
  active :: Boolean,
  isAnonymous :: Boolean,
  canCreateBoards :: Boolean,
  canCreateThreads :: Boolean,
  visibility :: Visibility,
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  activityAt :: (Maybe Date)
}


_BoardResponse :: Lens' BoardResponse {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  boardType :: BoardType,
  active :: Boolean,
  isAnonymous :: Boolean,
  canCreateBoards :: Boolean,
  canCreateThreads :: Boolean,
  visibility :: Visibility,
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  activityAt :: (Maybe Date)
}
_BoardResponse f (BoardResponse o) = BoardResponse <$> f o


mkBoardResponse :: Int -> Int -> String -> String -> (Maybe String) -> BoardType -> Boolean -> Boolean -> Boolean -> Boolean -> Visibility -> (Maybe String) -> (Array String) -> Int -> (Maybe Date) -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> BoardResponse
mkBoardResponse id userId name displayName description boardType active isAnonymous canCreateBoards canCreateThreads visibility icon tags guard createdAt modifiedAt modifiedBy activityAt =
  BoardResponse{id, userId, name, displayName, description, boardType, active, isAnonymous, canCreateBoards, canCreateThreads, visibility, icon, tags, guard, createdAt, modifiedAt, modifiedBy, activityAt}


unwrapBoardResponse :: BoardResponse -> {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  boardType :: BoardType,
  active :: Boolean,
  isAnonymous :: Boolean,
  canCreateBoards :: Boolean,
  canCreateThreads :: Boolean,
  visibility :: Visibility,
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  activityAt :: (Maybe Date)
}
unwrapBoardResponse (BoardResponse r) = r

instance boardResponseEncodeJson :: EncodeJson BoardResponse where
  encodeJson (BoardResponse o) =
       "tag" := "BoardResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "name" := o.name
    ~> "display_name" := o.displayName
    ~> "description" := o.description
    ~> "board_type" := o.boardType
    ~> "active" := o.active
    ~> "is_anonymous" := o.isAnonymous
    ~> "can_create_boards" := o.canCreateBoards
    ~> "can_create_threads" := o.canCreateThreads
    ~> "visibility" := o.visibility
    ~> "icon" := o.icon
    ~> "tags" := o.tags
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
    ~> "modified_by" := o.modifiedBy
    ~> "activity_at" := o.activityAt
    ~> jsonEmptyObject


instance boardResponseDecodeJson :: DecodeJson BoardResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    name <- obj .? "name"
    displayName <- obj .? "display_name"
    description <- obj .? "description"
    boardType <- obj .? "board_type"
    active <- obj .? "active"
    isAnonymous <- obj .? "is_anonymous"
    canCreateBoards <- obj .? "can_create_boards"
    canCreateThreads <- obj .? "can_create_threads"
    visibility <- obj .? "visibility"
    icon <- obj .? "icon"
    tags <- obj .? "tags"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    modifiedBy <- obj .? "modified_by"
    activityAt <- obj .? "activity_at"
    pure $ BoardResponse {
      id,
      userId,
      name,
      displayName,
      description,
      boardType,
      active,
      isAnonymous,
      canCreateBoards,
      canCreateThreads,
      visibility,
      icon,
      tags,
      guard,
      createdAt,
      modifiedAt,
      modifiedBy,
      activityAt
    }


instance boardResponseRequestable :: Requestable BoardResponse where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance boardResponseRespondable :: Respondable BoardResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


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
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance boardResponsesRespondable :: Respondable BoardResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


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
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance boardStatResponseRespondable :: Respondable BoardStatResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


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
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance boardStatResponsesRespondable :: Respondable BoardStatResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson

-- footer