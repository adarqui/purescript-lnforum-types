module LN.T.ThreadPost where
import LN.T.Board
import LN.T.Thread


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

data PostData
  = PostDataRaw String
  | PostDataMarkdown String
  | PostDataBBCode String
  | PostDataCode String String
  | PostDataOther String String
  | PostDataEmpty 



instance postDataEncodeJson :: EncodeJson PostData where
  encodeJson (PostDataRaw x0) =
       "tag" := "PostDataRaw"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (PostDataMarkdown x0) =
       "tag" := "PostDataMarkdown"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (PostDataBBCode x0) =
       "tag" := "PostDataBBCode"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (PostDataCode x0 x1) =
       "tag" := "PostDataCode"
    ~> "contents" := [encodeJson x0, encodeJson x1]
    ~> jsonEmptyObject
  encodeJson (PostDataOther x0 x1) =
       "tag" := "PostDataOther"
    ~> "contents" := [encodeJson x0, encodeJson x1]
    ~> jsonEmptyObject
  encodeJson (PostDataEmpty ) =
       "tag" := "PostDataEmpty"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance postDataDecodeJson :: DecodeJson PostData where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "PostDataRaw" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> PostDataRaw <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for PostDataRaw"


      "PostDataMarkdown" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> PostDataMarkdown <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for PostDataMarkdown"


      "PostDataBBCode" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> PostDataBBCode <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for PostDataBBCode"


      "PostDataCode" -> do
        r <- obj .? "contents"
        case r of
          [x0, x1] -> PostDataCode <$> decodeJson x0 <*> decodeJson x1
          _ -> Left $ "DecodeJson TypeMismatch for PostDataCode"


      "PostDataOther" -> do
        r <- obj .? "contents"
        case r of
          [x0, x1] -> PostDataOther <$> decodeJson x0 <*> decodeJson x1
          _ -> Left $ "DecodeJson TypeMismatch for PostDataOther"


      "PostDataEmpty" -> do
        pure PostDataEmpty

      _ -> Left $ "DecodeJson TypeMismatch for PostData"



instance postDataRequestable :: Requestable PostData where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance postDataRespondable :: Respondable PostData where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "PostDataRaw" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> PostDataRaw <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "PostDataRaw" "Respondable"


      "PostDataMarkdown" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> PostDataMarkdown <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "PostDataMarkdown" "Respondable"


      "PostDataBBCode" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> PostDataBBCode <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "PostDataBBCode" "Respondable"


      "PostDataCode" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0, x1] -> PostDataCode <$> exceptDecodeJsonRespondable x0 <*> exceptDecodeJsonRespondable x1
          _ -> fail $ TypeMismatch "PostDataCode" "Respondable"


      "PostDataOther" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0, x1] -> PostDataOther <$> exceptDecodeJsonRespondable x0 <*> exceptDecodeJsonRespondable x1
          _ -> fail $ TypeMismatch "PostDataOther" "Respondable"


      "PostDataEmpty" -> do
        pure PostDataEmpty

      _ -> fail $ TypeMismatch "PostData" "Respondable"



data TyPostData
  = TyPostDataRaw 
  | TyPostDataMarkdown 
  | TyPostDataBBCode 
  | TyPostDataCode 
  | TyPostDataOther 
  | TyPostDataEmpty 



instance tyPostDataEncodeJson :: EncodeJson TyPostData where
  encodeJson (TyPostDataRaw ) =
       "tag" := "TyPostDataRaw"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyPostDataMarkdown ) =
       "tag" := "TyPostDataMarkdown"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyPostDataBBCode ) =
       "tag" := "TyPostDataBBCode"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyPostDataCode ) =
       "tag" := "TyPostDataCode"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyPostDataOther ) =
       "tag" := "TyPostDataOther"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyPostDataEmpty ) =
       "tag" := "TyPostDataEmpty"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance tyPostDataDecodeJson :: DecodeJson TyPostData where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "TyPostDataRaw" -> do
        pure TyPostDataRaw

      "TyPostDataMarkdown" -> do
        pure TyPostDataMarkdown

      "TyPostDataBBCode" -> do
        pure TyPostDataBBCode

      "TyPostDataCode" -> do
        pure TyPostDataCode

      "TyPostDataOther" -> do
        pure TyPostDataOther

      "TyPostDataEmpty" -> do
        pure TyPostDataEmpty

      _ -> Left $ "DecodeJson TypeMismatch for TyPostData"



instance tyPostDataRequestable :: Requestable TyPostData where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance tyPostDataRespondable :: Respondable TyPostData where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "TyPostDataRaw" -> do
        pure TyPostDataRaw

      "TyPostDataMarkdown" -> do
        pure TyPostDataMarkdown

      "TyPostDataBBCode" -> do
        pure TyPostDataBBCode

      "TyPostDataCode" -> do
        pure TyPostDataCode

      "TyPostDataOther" -> do
        pure TyPostDataOther

      "TyPostDataEmpty" -> do
        pure TyPostDataEmpty

      _ -> fail $ TypeMismatch "TyPostData" "Respondable"



instance tyPostDataEq :: Eq TyPostData where
  eq TyPostDataRaw TyPostDataRaw = true
  eq TyPostDataMarkdown TyPostDataMarkdown = true
  eq TyPostDataBBCode TyPostDataBBCode = true
  eq TyPostDataCode TyPostDataCode = true
  eq TyPostDataOther TyPostDataOther = true
  eq TyPostDataEmpty TyPostDataEmpty = true
  eq _ _ = false

instance tyPostDataShow :: Show TyPostData where
  show TyPostDataRaw = "raw"
  show TyPostDataMarkdown = "markdown"
  show TyPostDataBBCode = "bbcode"
  show TyPostDataCode = "code"
  show TyPostDataOther = "other"
  show TyPostDataEmpty = "empty"


newtype ThreadPostRequest = ThreadPostRequest {
  title :: (Maybe String),
  body :: PostData,
  tags :: (Array String),
  privateTags :: (Array String),
  guard :: Int,
  stateTag :: (Maybe String),
  statePrivateTag :: (Maybe String)
}


type ThreadPostRequestR = {
  title :: (Maybe String),
  body :: PostData,
  tags :: (Array String),
  privateTags :: (Array String),
  guard :: Int,
  stateTag :: (Maybe String),
  statePrivateTag :: (Maybe String)
}


_ThreadPostRequest :: Lens' ThreadPostRequest {
  title :: (Maybe String),
  body :: PostData,
  tags :: (Array String),
  privateTags :: (Array String),
  guard :: Int,
  stateTag :: (Maybe String),
  statePrivateTag :: (Maybe String)
}
_ThreadPostRequest f (ThreadPostRequest o) = ThreadPostRequest <$> f o


mkThreadPostRequest :: (Maybe String) -> PostData -> (Array String) -> (Array String) -> Int -> (Maybe String) -> (Maybe String) -> ThreadPostRequest
mkThreadPostRequest title body tags privateTags guard stateTag statePrivateTag =
  ThreadPostRequest{title, body, tags, privateTags, guard, stateTag, statePrivateTag}


unwrapThreadPostRequest :: ThreadPostRequest -> {
  title :: (Maybe String),
  body :: PostData,
  tags :: (Array String),
  privateTags :: (Array String),
  guard :: Int,
  stateTag :: (Maybe String),
  statePrivateTag :: (Maybe String)
}
unwrapThreadPostRequest (ThreadPostRequest r) = r

instance threadPostRequestEncodeJson :: EncodeJson ThreadPostRequest where
  encodeJson (ThreadPostRequest o) =
       "tag" := "ThreadPostRequest"
    ~> "title" := o.title
    ~> "body" := o.body
    ~> "tags" := o.tags
    ~> "private_tags" := o.privateTags
    ~> "guard" := o.guard
    ~> "state_tag" := o.stateTag
    ~> "state_private_tag" := o.statePrivateTag
    ~> jsonEmptyObject


instance threadPostRequestDecodeJson :: DecodeJson ThreadPostRequest where
  decodeJson o = do
    obj <- decodeJson o
    title <- obj .? "title"
    body <- obj .? "body"
    tags <- obj .? "tags"
    privateTags <- obj .? "private_tags"
    guard <- obj .? "guard"
    stateTag <- obj .? "state_tag"
    statePrivateTag <- obj .? "state_private_tag"
    pure $ ThreadPostRequest {
      title,
      body,
      tags,
      privateTags,
      guard,
      stateTag,
      statePrivateTag
    }


instance threadPostRequestRequestable :: Requestable ThreadPostRequest where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance threadPostRequestRespondable :: Respondable ThreadPostRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype ThreadPostResponse = ThreadPostResponse {
  id :: Int,
  userId :: Int,
  boardId :: Int,
  threadId :: Int,
  parentId :: (Maybe Int),
  title :: (Maybe String),
  body :: PostData,
  tags :: (Array String),
  privateTags :: (Array String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


type ThreadPostResponseR = {
  id :: Int,
  userId :: Int,
  boardId :: Int,
  threadId :: Int,
  parentId :: (Maybe Int),
  title :: (Maybe String),
  body :: PostData,
  tags :: (Array String),
  privateTags :: (Array String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


_ThreadPostResponse :: Lens' ThreadPostResponse {
  id :: Int,
  userId :: Int,
  boardId :: Int,
  threadId :: Int,
  parentId :: (Maybe Int),
  title :: (Maybe String),
  body :: PostData,
  tags :: (Array String),
  privateTags :: (Array String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
_ThreadPostResponse f (ThreadPostResponse o) = ThreadPostResponse <$> f o


mkThreadPostResponse :: Int -> Int -> Int -> Int -> (Maybe Int) -> (Maybe String) -> PostData -> (Array String) -> (Array String) -> Boolean -> Int -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> ThreadPostResponse
mkThreadPostResponse id userId boardId threadId parentId title body tags privateTags active guard createdAt modifiedBy modifiedAt activityAt =
  ThreadPostResponse{id, userId, boardId, threadId, parentId, title, body, tags, privateTags, active, guard, createdAt, modifiedBy, modifiedAt, activityAt}


unwrapThreadPostResponse :: ThreadPostResponse -> {
  id :: Int,
  userId :: Int,
  boardId :: Int,
  threadId :: Int,
  parentId :: (Maybe Int),
  title :: (Maybe String),
  body :: PostData,
  tags :: (Array String),
  privateTags :: (Array String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
unwrapThreadPostResponse (ThreadPostResponse r) = r

instance threadPostResponseEncodeJson :: EncodeJson ThreadPostResponse where
  encodeJson (ThreadPostResponse o) =
       "tag" := "ThreadPostResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "board_id" := o.boardId
    ~> "thread_id" := o.threadId
    ~> "parent_id" := o.parentId
    ~> "title" := o.title
    ~> "body" := o.body
    ~> "tags" := o.tags
    ~> "private_tags" := o.privateTags
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_by" := o.modifiedBy
    ~> "modified_at" := o.modifiedAt
    ~> "activity_at" := o.activityAt
    ~> jsonEmptyObject


instance threadPostResponseDecodeJson :: DecodeJson ThreadPostResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    boardId <- obj .? "board_id"
    threadId <- obj .? "thread_id"
    parentId <- obj .? "parent_id"
    title <- obj .? "title"
    body <- obj .? "body"
    tags <- obj .? "tags"
    privateTags <- obj .? "private_tags"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedBy <- obj .? "modified_by"
    modifiedAt <- obj .? "modified_at"
    activityAt <- obj .? "activity_at"
    pure $ ThreadPostResponse {
      id,
      userId,
      boardId,
      threadId,
      parentId,
      title,
      body,
      tags,
      privateTags,
      active,
      guard,
      createdAt,
      modifiedBy,
      modifiedAt,
      activityAt
    }


instance threadPostResponseRequestable :: Requestable ThreadPostResponse where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance threadPostResponseRespondable :: Respondable ThreadPostResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype ThreadPostResponses = ThreadPostResponses {
  threadPostResponses :: (Array ThreadPostResponse)
}


type ThreadPostResponsesR = {
  threadPostResponses :: (Array ThreadPostResponse)
}


_ThreadPostResponses :: Lens' ThreadPostResponses {
  threadPostResponses :: (Array ThreadPostResponse)
}
_ThreadPostResponses f (ThreadPostResponses o) = ThreadPostResponses <$> f o


mkThreadPostResponses :: (Array ThreadPostResponse) -> ThreadPostResponses
mkThreadPostResponses threadPostResponses =
  ThreadPostResponses{threadPostResponses}


unwrapThreadPostResponses :: ThreadPostResponses -> {
  threadPostResponses :: (Array ThreadPostResponse)
}
unwrapThreadPostResponses (ThreadPostResponses r) = r

instance threadPostResponsesEncodeJson :: EncodeJson ThreadPostResponses where
  encodeJson (ThreadPostResponses o) =
       "tag" := "ThreadPostResponses"
    ~> "thread_post_responses" := o.threadPostResponses
    ~> jsonEmptyObject


instance threadPostResponsesDecodeJson :: DecodeJson ThreadPostResponses where
  decodeJson o = do
    obj <- decodeJson o
    threadPostResponses <- obj .? "thread_post_responses"
    pure $ ThreadPostResponses {
      threadPostResponses
    }


instance threadPostResponsesRequestable :: Requestable ThreadPostResponses where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance threadPostResponsesRespondable :: Respondable ThreadPostResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype ThreadPostStatResponse = ThreadPostStatResponse {
  threadPostId :: Int,
  likes :: Int,
  neutral :: Int,
  dislikes :: Int,
  views :: Int
}


type ThreadPostStatResponseR = {
  threadPostId :: Int,
  likes :: Int,
  neutral :: Int,
  dislikes :: Int,
  views :: Int
}


_ThreadPostStatResponse :: Lens' ThreadPostStatResponse {
  threadPostId :: Int,
  likes :: Int,
  neutral :: Int,
  dislikes :: Int,
  views :: Int
}
_ThreadPostStatResponse f (ThreadPostStatResponse o) = ThreadPostStatResponse <$> f o


mkThreadPostStatResponse :: Int -> Int -> Int -> Int -> Int -> ThreadPostStatResponse
mkThreadPostStatResponse threadPostId likes neutral dislikes views =
  ThreadPostStatResponse{threadPostId, likes, neutral, dislikes, views}


unwrapThreadPostStatResponse :: ThreadPostStatResponse -> {
  threadPostId :: Int,
  likes :: Int,
  neutral :: Int,
  dislikes :: Int,
  views :: Int
}
unwrapThreadPostStatResponse (ThreadPostStatResponse r) = r

instance threadPostStatResponseEncodeJson :: EncodeJson ThreadPostStatResponse where
  encodeJson (ThreadPostStatResponse o) =
       "tag" := "ThreadPostStatResponse"
    ~> "thread_post_id" := o.threadPostId
    ~> "likes" := o.likes
    ~> "neutral" := o.neutral
    ~> "dislikes" := o.dislikes
    ~> "views" := o.views
    ~> jsonEmptyObject


instance threadPostStatResponseDecodeJson :: DecodeJson ThreadPostStatResponse where
  decodeJson o = do
    obj <- decodeJson o
    threadPostId <- obj .? "thread_post_id"
    likes <- obj .? "likes"
    neutral <- obj .? "neutral"
    dislikes <- obj .? "dislikes"
    views <- obj .? "views"
    pure $ ThreadPostStatResponse {
      threadPostId,
      likes,
      neutral,
      dislikes,
      views
    }


instance threadPostStatResponseRequestable :: Requestable ThreadPostStatResponse where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance threadPostStatResponseRespondable :: Respondable ThreadPostStatResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype ThreadPostStatResponses = ThreadPostStatResponses {
  threadPostStatResponses :: (Array ThreadPostStatResponse)
}


type ThreadPostStatResponsesR = {
  threadPostStatResponses :: (Array ThreadPostStatResponse)
}


_ThreadPostStatResponses :: Lens' ThreadPostStatResponses {
  threadPostStatResponses :: (Array ThreadPostStatResponse)
}
_ThreadPostStatResponses f (ThreadPostStatResponses o) = ThreadPostStatResponses <$> f o


mkThreadPostStatResponses :: (Array ThreadPostStatResponse) -> ThreadPostStatResponses
mkThreadPostStatResponses threadPostStatResponses =
  ThreadPostStatResponses{threadPostStatResponses}


unwrapThreadPostStatResponses :: ThreadPostStatResponses -> {
  threadPostStatResponses :: (Array ThreadPostStatResponse)
}
unwrapThreadPostStatResponses (ThreadPostStatResponses r) = r

instance threadPostStatResponsesEncodeJson :: EncodeJson ThreadPostStatResponses where
  encodeJson (ThreadPostStatResponses o) =
       "tag" := "ThreadPostStatResponses"
    ~> "thread_post_stat_responses" := o.threadPostStatResponses
    ~> jsonEmptyObject


instance threadPostStatResponsesDecodeJson :: DecodeJson ThreadPostStatResponses where
  decodeJson o = do
    obj <- decodeJson o
    threadPostStatResponses <- obj .? "thread_post_stat_responses"
    pure $ ThreadPostStatResponses {
      threadPostStatResponses
    }


instance threadPostStatResponsesRequestable :: Requestable ThreadPostStatResponses where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance threadPostStatResponsesRespondable :: Respondable ThreadPostStatResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson

-- footer