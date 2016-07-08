module LN.T.ThreadPost where



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
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance postDataRespondable :: Respondable PostData where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "PostDataRaw" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> PostDataRaw <$> read x0
          _ -> Left $ TypeMismatch "PostDataRaw" "Respondable"


      "PostDataMarkdown" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> PostDataMarkdown <$> read x0
          _ -> Left $ TypeMismatch "PostDataMarkdown" "Respondable"


      "PostDataBBCode" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> PostDataBBCode <$> read x0
          _ -> Left $ TypeMismatch "PostDataBBCode" "Respondable"


      "PostDataCode" -> do
        r <- readProp "contents" json
        case r of
          [x0, x1] -> PostDataCode <$> read x0 <*> read x1
          _ -> Left $ TypeMismatch "PostDataCode" "Respondable"


      "PostDataOther" -> do
        r <- readProp "contents" json
        case r of
          [x0, x1] -> PostDataOther <$> read x0 <*> read x1
          _ -> Left $ TypeMismatch "PostDataOther" "Respondable"


      "PostDataEmpty" -> do
        pure PostDataEmpty

      _ -> Left $ TypeMismatch "PostData" "Respondable"



instance postDataIsForeign :: IsForeign PostData where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "PostDataRaw" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> PostDataRaw <$> read x0
          _ -> Left $ TypeMismatch "PostDataRaw" "IsForeign"


      "PostDataMarkdown" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> PostDataMarkdown <$> read x0
          _ -> Left $ TypeMismatch "PostDataMarkdown" "IsForeign"


      "PostDataBBCode" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> PostDataBBCode <$> read x0
          _ -> Left $ TypeMismatch "PostDataBBCode" "IsForeign"


      "PostDataCode" -> do
        r <- readProp "contents" json
        case r of
          [x0, x1] -> PostDataCode <$> read x0 <*> read x1
          _ -> Left $ TypeMismatch "PostDataCode" "IsForeign"


      "PostDataOther" -> do
        r <- readProp "contents" json
        case r of
          [x0, x1] -> PostDataOther <$> read x0 <*> read x1
          _ -> Left $ TypeMismatch "PostDataOther" "IsForeign"


      "PostDataEmpty" -> do
        pure PostDataEmpty

      _ -> Left $ TypeMismatch "PostData" "IsForeign"



instance postDataEq :: Eq PostData where
  eq (PostDataRaw x0a) (PostDataRaw x0b) = x0a == x0b
  eq (PostDataMarkdown x0a) (PostDataMarkdown x0b) = x0a == x0b
  eq (PostDataBBCode x0a) (PostDataBBCode x0b) = x0a == x0b
  eq (PostDataCode x0a x1a) (PostDataCode x0b x1b) = x0a == x0b && x1a == x1b
  eq (PostDataOther x0a x1a) (PostDataOther x0b x1b) = x0a == x0b && x1a == x1b
  eq PostDataEmpty PostDataEmpty = true
  eq _ _ = false

newtype ThreadPostRequest = ThreadPostRequest {
  title :: (Maybe String),
  body :: PostData,
  tags :: (Array String),
  privateTags :: (Array String),
  guard :: Int
}


type ThreadPostRequestR = {
  title :: (Maybe String),
  body :: PostData,
  tags :: (Array String),
  privateTags :: (Array String),
  guard :: Int
}


_ThreadPostRequest :: Lens' ThreadPostRequest {
  title :: (Maybe String),
  body :: PostData,
  tags :: (Array String),
  privateTags :: (Array String),
  guard :: Int
}
_ThreadPostRequest f (ThreadPostRequest o) = ThreadPostRequest <$> f o


mkThreadPostRequest :: (Maybe String) -> PostData -> (Array String) -> (Array String) -> Int -> ThreadPostRequest
mkThreadPostRequest title body tags privateTags guard =
  ThreadPostRequest{title, body, tags, privateTags, guard}


unwrapThreadPostRequest :: ThreadPostRequest -> {
  title :: (Maybe String),
  body :: PostData,
  tags :: (Array String),
  privateTags :: (Array String),
  guard :: Int
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
    ~> jsonEmptyObject


instance threadPostRequestDecodeJson :: DecodeJson ThreadPostRequest where
  decodeJson o = do
    obj <- decodeJson o
    title <- obj .? "title"
    body <- obj .? "body"
    tags <- obj .? "tags"
    privateTags <- obj .? "private_tags"
    guard <- obj .? "guard"
    pure $ ThreadPostRequest {
      title,
      body,
      tags,
      privateTags,
      guard
    }


instance threadPostRequestRequestable :: Requestable ThreadPostRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance threadPostRequestRespondable :: Respondable ThreadPostRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkThreadPostRequest
      <$> (unNullOrUndefined <$> readProp "title" json)
      <*> readProp "body" json
      <*> readProp "tags" json
      <*> readProp "private_tags" json
      <*> readProp "guard" json


instance threadPostRequestIsForeign :: IsForeign ThreadPostRequest where
  read json =
      mkThreadPostRequest
      <$> (unNullOrUndefined <$> readProp "title" json)
      <*> readProp "body" json
      <*> readProp "tags" json
      <*> readProp "private_tags" json
      <*> readProp "guard" json


newtype ThreadPostResponse = ThreadPostResponse {
  id :: Int,
  userId :: Int,
  orgId :: Int,
  forumId :: Int,
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
  orgId :: Int,
  forumId :: Int,
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
  orgId :: Int,
  forumId :: Int,
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


mkThreadPostResponse :: Int -> Int -> Int -> Int -> Int -> Int -> (Maybe Int) -> (Maybe String) -> PostData -> (Array String) -> (Array String) -> Boolean -> Int -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> ThreadPostResponse
mkThreadPostResponse id userId orgId forumId boardId threadId parentId title body tags privateTags active guard createdAt modifiedBy modifiedAt activityAt =
  ThreadPostResponse{id, userId, orgId, forumId, boardId, threadId, parentId, title, body, tags, privateTags, active, guard, createdAt, modifiedBy, modifiedAt, activityAt}


unwrapThreadPostResponse :: ThreadPostResponse -> {
  id :: Int,
  userId :: Int,
  orgId :: Int,
  forumId :: Int,
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
    ~> "org_id" := o.orgId
    ~> "forum_id" := o.forumId
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
    orgId <- obj .? "org_id"
    forumId <- obj .? "forum_id"
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
      orgId,
      forumId,
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
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance threadPostResponseRespondable :: Respondable ThreadPostResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkThreadPostResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "org_id" json
      <*> readProp "forum_id" json
      <*> readProp "board_id" json
      <*> readProp "thread_id" json
      <*> (unNullOrUndefined <$> readProp "parent_id" json)
      <*> (unNullOrUndefined <$> readProp "title" json)
      <*> readProp "body" json
      <*> readProp "tags" json
      <*> readProp "private_tags" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_by" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


instance threadPostResponseIsForeign :: IsForeign ThreadPostResponse where
  read json =
      mkThreadPostResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "org_id" json
      <*> readProp "forum_id" json
      <*> readProp "board_id" json
      <*> readProp "thread_id" json
      <*> (unNullOrUndefined <$> readProp "parent_id" json)
      <*> (unNullOrUndefined <$> readProp "title" json)
      <*> readProp "body" json
      <*> readProp "tags" json
      <*> readProp "private_tags" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_by" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


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
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance threadPostResponsesRespondable :: Respondable ThreadPostResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkThreadPostResponses
      <$> readProp "thread_post_responses" json


instance threadPostResponsesIsForeign :: IsForeign ThreadPostResponses where
  read json =
      mkThreadPostResponses
      <$> readProp "thread_post_responses" json


newtype ThreadPostStatResponse = ThreadPostStatResponse {
  threadPostId :: Int,
  likes :: Int,
  neutral :: Int,
  dislikes :: Int,
  stars :: Int,
  views :: Int
}


type ThreadPostStatResponseR = {
  threadPostId :: Int,
  likes :: Int,
  neutral :: Int,
  dislikes :: Int,
  stars :: Int,
  views :: Int
}


_ThreadPostStatResponse :: Lens' ThreadPostStatResponse {
  threadPostId :: Int,
  likes :: Int,
  neutral :: Int,
  dislikes :: Int,
  stars :: Int,
  views :: Int
}
_ThreadPostStatResponse f (ThreadPostStatResponse o) = ThreadPostStatResponse <$> f o


mkThreadPostStatResponse :: Int -> Int -> Int -> Int -> Int -> Int -> ThreadPostStatResponse
mkThreadPostStatResponse threadPostId likes neutral dislikes stars views =
  ThreadPostStatResponse{threadPostId, likes, neutral, dislikes, stars, views}


unwrapThreadPostStatResponse :: ThreadPostStatResponse -> {
  threadPostId :: Int,
  likes :: Int,
  neutral :: Int,
  dislikes :: Int,
  stars :: Int,
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
    ~> "stars" := o.stars
    ~> "views" := o.views
    ~> jsonEmptyObject


instance threadPostStatResponseDecodeJson :: DecodeJson ThreadPostStatResponse where
  decodeJson o = do
    obj <- decodeJson o
    threadPostId <- obj .? "thread_post_id"
    likes <- obj .? "likes"
    neutral <- obj .? "neutral"
    dislikes <- obj .? "dislikes"
    stars <- obj .? "stars"
    views <- obj .? "views"
    pure $ ThreadPostStatResponse {
      threadPostId,
      likes,
      neutral,
      dislikes,
      stars,
      views
    }


instance threadPostStatResponseRequestable :: Requestable ThreadPostStatResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance threadPostStatResponseRespondable :: Respondable ThreadPostStatResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkThreadPostStatResponse
      <$> readProp "thread_post_id" json
      <*> readProp "likes" json
      <*> readProp "neutral" json
      <*> readProp "dislikes" json
      <*> readProp "stars" json
      <*> readProp "views" json


instance threadPostStatResponseIsForeign :: IsForeign ThreadPostStatResponse where
  read json =
      mkThreadPostStatResponse
      <$> readProp "thread_post_id" json
      <*> readProp "likes" json
      <*> readProp "neutral" json
      <*> readProp "dislikes" json
      <*> readProp "stars" json
      <*> readProp "views" json


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
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance threadPostStatResponsesRespondable :: Respondable ThreadPostStatResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkThreadPostStatResponses
      <$> readProp "thread_post_stat_responses" json


instance threadPostStatResponsesIsForeign :: IsForeign ThreadPostStatResponses where
  read json =
      mkThreadPostStatResponses
      <$> readProp "thread_post_stat_responses" json

-- footer