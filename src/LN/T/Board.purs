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
  = ISBN13 String
  | ISBN10 String
  | ISBN String
  | URL String
  | SourceNone 



instance boardTypeEncodeJson :: EncodeJson BoardType where
  encodeJson (ISBN13 x0) =
       "tag" := "ISBN13"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ISBN10 x0) =
       "tag" := "ISBN10"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ISBN x0) =
       "tag" := "ISBN"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (URL x0) =
       "tag" := "URL"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (SourceNone ) =
       "tag" := "SourceNone"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance boardTypeDecodeJson :: DecodeJson BoardType where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "ISBN13" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ISBN13 <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ISBN13"


      "ISBN10" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ISBN10 <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ISBN10"


      "ISBN" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ISBN <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ISBN"


      "URL" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> URL <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for URL"


      "SourceNone" -> do
        pure SourceNone

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
      "ISBN13" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ISBN13 <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ISBN13" "Respondable"


      "ISBN10" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ISBN10 <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ISBN10" "Respondable"


      "ISBN" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ISBN <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ISBN" "Respondable"


      "URL" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> URL <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "URL" "Respondable"


      "SourceNone" -> do
        pure SourceNone

      _ -> fail $ TypeMismatch "BoardType" "Respondable"



data TyBoardType
  = TyISBN13 
  | TyISBN10 
  | TyISBN 
  | TyURL 
  | TySourceNone 



instance tyBoardTypeEncodeJson :: EncodeJson TyBoardType where
  encodeJson (TyISBN13 ) =
       "tag" := "TyISBN13"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyISBN10 ) =
       "tag" := "TyISBN10"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyISBN ) =
       "tag" := "TyISBN"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyURL ) =
       "tag" := "TyURL"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TySourceNone ) =
       "tag" := "TySourceNone"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance tyBoardTypeDecodeJson :: DecodeJson TyBoardType where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "TyISBN13" -> do
        pure TyISBN13

      "TyISBN10" -> do
        pure TyISBN10

      "TyISBN" -> do
        pure TyISBN

      "TyURL" -> do
        pure TyURL

      "TySourceNone" -> do
        pure TySourceNone

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
      "TyISBN13" -> do
        pure TyISBN13

      "TyISBN10" -> do
        pure TyISBN10

      "TyISBN" -> do
        pure TyISBN

      "TyURL" -> do
        pure TyURL

      "TySourceNone" -> do
        pure TySourceNone

      _ -> fail $ TypeMismatch "TyBoardType" "Respondable"



instance tyBoardTypeEq :: Eq TyBoardType where
  eq TyISBN13 TyISBN13 = true
  eq TyISBN10 TyISBN10 = true
  eq TyISBN TyISBN = true
  eq TyURL TyURL = true
  eq TySourceNone TySourceNone = true
  eq _ _ = false

instance tyBoardTypeShow :: Show TyBoardType where
  show TyISBN13 = "ty_isbn13"
  show TyISBN10 = "ty_isbn10"
  show TyISBN = "ty_isbn"
  show TyURL = "ty_url"
  show TySourceNone = "ty_source_none"


newtype BoardRequest = BoardRequest {
  displayName :: String,
  description :: String,
  source :: BoardType,
  author :: (Maybe (Array String)),
  prerequisites :: (Array String),
  categories :: (Array String),
  visibility :: Visibility,
  counter :: Int,
  version :: (Maybe String),
  urls :: (Maybe (Array String)),
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int
}


type BoardRequestR = {
  displayName :: String,
  description :: String,
  source :: BoardType,
  author :: (Maybe (Array String)),
  prerequisites :: (Array String),
  categories :: (Array String),
  visibility :: Visibility,
  counter :: Int,
  version :: (Maybe String),
  urls :: (Maybe (Array String)),
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int
}


_BoardRequest :: Lens' BoardRequest {
  displayName :: String,
  description :: String,
  source :: BoardType,
  author :: (Maybe (Array String)),
  prerequisites :: (Array String),
  categories :: (Array String),
  visibility :: Visibility,
  counter :: Int,
  version :: (Maybe String),
  urls :: (Maybe (Array String)),
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int
}
_BoardRequest f (BoardRequest o) = BoardRequest <$> f o


mkBoardRequest :: String -> String -> BoardType -> (Maybe (Array String)) -> (Array String) -> (Array String) -> Visibility -> Int -> (Maybe String) -> (Maybe (Array String)) -> (Maybe String) -> (Array String) -> Int -> BoardRequest
mkBoardRequest displayName description source author prerequisites categories visibility counter version urls icon tags guard =
  BoardRequest{displayName, description, source, author, prerequisites, categories, visibility, counter, version, urls, icon, tags, guard}


unwrapBoardRequest :: BoardRequest -> {
  displayName :: String,
  description :: String,
  source :: BoardType,
  author :: (Maybe (Array String)),
  prerequisites :: (Array String),
  categories :: (Array String),
  visibility :: Visibility,
  counter :: Int,
  version :: (Maybe String),
  urls :: (Maybe (Array String)),
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
    ~> "source" := o.source
    ~> "author" := o.author
    ~> "prerequisites" := o.prerequisites
    ~> "categories" := o.categories
    ~> "visibility" := o.visibility
    ~> "counter" := o.counter
    ~> "version" := o.version
    ~> "urls" := o.urls
    ~> "icon" := o.icon
    ~> "tags" := o.tags
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance boardRequestDecodeJson :: DecodeJson BoardRequest where
  decodeJson o = do
    obj <- decodeJson o
    displayName <- obj .? "display_name"
    description <- obj .? "description"
    source <- obj .? "source"
    author <- obj .? "author"
    prerequisites <- obj .? "prerequisites"
    categories <- obj .? "categories"
    visibility <- obj .? "visibility"
    counter <- obj .? "counter"
    version <- obj .? "version"
    urls <- obj .? "urls"
    icon <- obj .? "icon"
    tags <- obj .? "tags"
    guard <- obj .? "guard"
    pure $ BoardRequest {
      displayName,
      description,
      source,
      author,
      prerequisites,
      categories,
      visibility,
      counter,
      version,
      urls,
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
  description :: String,
  source :: BoardType,
  author :: (Maybe (Array String)),
  prerequisites :: (Array String),
  categories :: (Array String),
  visibility :: Visibility,
  counter :: Int,
  version :: (Maybe String),
  urls :: (Maybe (Array String)),
  icon :: (Maybe String),
  tags :: (Array String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


type BoardResponseR = {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: String,
  source :: BoardType,
  author :: (Maybe (Array String)),
  prerequisites :: (Array String),
  categories :: (Array String),
  visibility :: Visibility,
  counter :: Int,
  version :: (Maybe String),
  urls :: (Maybe (Array String)),
  icon :: (Maybe String),
  tags :: (Array String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


_BoardResponse :: Lens' BoardResponse {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: String,
  source :: BoardType,
  author :: (Maybe (Array String)),
  prerequisites :: (Array String),
  categories :: (Array String),
  visibility :: Visibility,
  counter :: Int,
  version :: (Maybe String),
  urls :: (Maybe (Array String)),
  icon :: (Maybe String),
  tags :: (Array String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
_BoardResponse f (BoardResponse o) = BoardResponse <$> f o


mkBoardResponse :: Int -> Int -> String -> String -> String -> BoardType -> (Maybe (Array String)) -> (Array String) -> (Array String) -> Visibility -> Int -> (Maybe String) -> (Maybe (Array String)) -> (Maybe String) -> (Array String) -> Boolean -> Int -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> BoardResponse
mkBoardResponse id userId name displayName description source author prerequisites categories visibility counter version urls icon tags active guard createdAt modifiedAt activityAt =
  BoardResponse{id, userId, name, displayName, description, source, author, prerequisites, categories, visibility, counter, version, urls, icon, tags, active, guard, createdAt, modifiedAt, activityAt}


unwrapBoardResponse :: BoardResponse -> {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: String,
  source :: BoardType,
  author :: (Maybe (Array String)),
  prerequisites :: (Array String),
  categories :: (Array String),
  visibility :: Visibility,
  counter :: Int,
  version :: (Maybe String),
  urls :: (Maybe (Array String)),
  icon :: (Maybe String),
  tags :: (Array String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
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
    ~> "source" := o.source
    ~> "author" := o.author
    ~> "prerequisites" := o.prerequisites
    ~> "categories" := o.categories
    ~> "visibility" := o.visibility
    ~> "counter" := o.counter
    ~> "version" := o.version
    ~> "urls" := o.urls
    ~> "icon" := o.icon
    ~> "tags" := o.tags
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
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
    source <- obj .? "source"
    author <- obj .? "author"
    prerequisites <- obj .? "prerequisites"
    categories <- obj .? "categories"
    visibility <- obj .? "visibility"
    counter <- obj .? "counter"
    version <- obj .? "version"
    urls <- obj .? "urls"
    icon <- obj .? "icon"
    tags <- obj .? "tags"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    activityAt <- obj .? "activity_at"
    pure $ BoardResponse {
      id,
      userId,
      name,
      displayName,
      description,
      source,
      author,
      prerequisites,
      categories,
      visibility,
      counter,
      version,
      urls,
      icon,
      tags,
      active,
      guard,
      createdAt,
      modifiedAt,
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
  likes :: Int,
  neutral :: Int,
  dislikes :: Int,
  views :: Int
}


type BoardStatResponseR = {
  boardId :: Int,
  likes :: Int,
  neutral :: Int,
  dislikes :: Int,
  views :: Int
}


_BoardStatResponse :: Lens' BoardStatResponse {
  boardId :: Int,
  likes :: Int,
  neutral :: Int,
  dislikes :: Int,
  views :: Int
}
_BoardStatResponse f (BoardStatResponse o) = BoardStatResponse <$> f o


mkBoardStatResponse :: Int -> Int -> Int -> Int -> Int -> BoardStatResponse
mkBoardStatResponse boardId likes neutral dislikes views =
  BoardStatResponse{boardId, likes, neutral, dislikes, views}


unwrapBoardStatResponse :: BoardStatResponse -> {
  boardId :: Int,
  likes :: Int,
  neutral :: Int,
  dislikes :: Int,
  views :: Int
}
unwrapBoardStatResponse (BoardStatResponse r) = r

instance boardStatResponseEncodeJson :: EncodeJson BoardStatResponse where
  encodeJson (BoardStatResponse o) =
       "tag" := "BoardStatResponse"
    ~> "board_id" := o.boardId
    ~> "likes" := o.likes
    ~> "neutral" := o.neutral
    ~> "dislikes" := o.dislikes
    ~> "views" := o.views
    ~> jsonEmptyObject


instance boardStatResponseDecodeJson :: DecodeJson BoardStatResponse where
  decodeJson o = do
    obj <- decodeJson o
    boardId <- obj .? "board_id"
    likes <- obj .? "likes"
    neutral <- obj .? "neutral"
    dislikes <- obj .? "dislikes"
    views <- obj .? "views"
    pure $ BoardStatResponse {
      boardId,
      likes,
      neutral,
      dislikes,
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