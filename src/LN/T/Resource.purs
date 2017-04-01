module LN.T.Resource where
import LN.T.DepList
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

data ResourceType
  = ISBN13 String
  | ISBN10 String
  | ISBN String
  | URL String
  | SourceNone 



instance resourceTypeEncodeJson :: EncodeJson ResourceType where
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


instance resourceTypeDecodeJson :: DecodeJson ResourceType where
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

      _ -> Left $ "DecodeJson TypeMismatch for ResourceType"



instance resourceTypeRequestable :: Requestable ResourceType where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance resourceTypeRespondable :: Respondable ResourceType where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "ISBN13" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> ISBN13 <$> read x0
          _ -> fail $ TypeMismatch "ISBN13" "Respondable"


      "ISBN10" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> ISBN10 <$> read x0
          _ -> fail $ TypeMismatch "ISBN10" "Respondable"


      "ISBN" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> ISBN <$> read x0
          _ -> fail $ TypeMismatch "ISBN" "Respondable"


      "URL" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> URL <$> read x0
          _ -> fail $ TypeMismatch "URL" "Respondable"


      "SourceNone" -> do
        pure SourceNone

      _ -> fail $ TypeMismatch "ResourceType" "Respondable"



instance resourceTypeIsForeign :: IsForeign ResourceType where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "ISBN13" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> ISBN13 <$> read x0
          _ -> fail $ TypeMismatch "ISBN13" "IsForeign"


      "ISBN10" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> ISBN10 <$> read x0
          _ -> fail $ TypeMismatch "ISBN10" "IsForeign"


      "ISBN" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> ISBN <$> read x0
          _ -> fail $ TypeMismatch "ISBN" "IsForeign"


      "URL" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> URL <$> read x0
          _ -> fail $ TypeMismatch "URL" "IsForeign"


      "SourceNone" -> do
        pure SourceNone

      _ -> fail $ TypeMismatch "ResourceType" "IsForeign"



data TyResourceType
  = TyISBN13 
  | TyISBN10 
  | TyISBN 
  | TyURL 
  | TySourceNone 



instance tyResourceTypeEncodeJson :: EncodeJson TyResourceType where
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


instance tyResourceTypeDecodeJson :: DecodeJson TyResourceType where
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

      _ -> Left $ "DecodeJson TypeMismatch for TyResourceType"



instance tyResourceTypeRequestable :: Requestable TyResourceType where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance tyResourceTypeRespondable :: Respondable TyResourceType where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
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

      _ -> fail $ TypeMismatch "TyResourceType" "Respondable"



instance tyResourceTypeIsForeign :: IsForeign TyResourceType where
  read json = do
    tag <- readProp "tag" json
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

      _ -> fail $ TypeMismatch "TyResourceType" "IsForeign"



instance tyResourceTypeEq :: Eq TyResourceType where
  eq TyISBN13 TyISBN13 = true
  eq TyISBN10 TyISBN10 = true
  eq TyISBN TyISBN = true
  eq TyURL TyURL = true
  eq TySourceNone TySourceNone = true
  eq _ _ = false

instance tyResourceTypeShow :: Show TyResourceType where
  show TyISBN13 = "ty_isbn13"
  show TyISBN10 = "ty_isbn10"
  show TyISBN = "ty_isbn"
  show TyURL = "ty_url"
  show TySourceNone = "ty_source_none"


newtype ResourceRequest = ResourceRequest {
  displayName :: String,
  description :: String,
  source :: ResourceType,
  author :: (Maybe (Array String)),
  prerequisites :: (DepList String),
  categories :: (DepList String),
  visibility :: Visibility,
  counter :: Int,
  version :: (Maybe String),
  urls :: (Maybe (Array String)),
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int
}


type ResourceRequestR = {
  displayName :: String,
  description :: String,
  source :: ResourceType,
  author :: (Maybe (Array String)),
  prerequisites :: (DepList String),
  categories :: (DepList String),
  visibility :: Visibility,
  counter :: Int,
  version :: (Maybe String),
  urls :: (Maybe (Array String)),
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int
}


_ResourceRequest :: Lens' ResourceRequest {
  displayName :: String,
  description :: String,
  source :: ResourceType,
  author :: (Maybe (Array String)),
  prerequisites :: (DepList String),
  categories :: (DepList String),
  visibility :: Visibility,
  counter :: Int,
  version :: (Maybe String),
  urls :: (Maybe (Array String)),
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int
}
_ResourceRequest f (ResourceRequest o) = ResourceRequest <$> f o


mkResourceRequest :: String -> String -> ResourceType -> (Maybe (Array String)) -> (DepList String) -> (DepList String) -> Visibility -> Int -> (Maybe String) -> (Maybe (Array String)) -> (Maybe String) -> (Array String) -> Int -> ResourceRequest
mkResourceRequest displayName description source author prerequisites categories visibility counter version urls icon tags guard =
  ResourceRequest{displayName, description, source, author, prerequisites, categories, visibility, counter, version, urls, icon, tags, guard}


unwrapResourceRequest :: ResourceRequest -> {
  displayName :: String,
  description :: String,
  source :: ResourceType,
  author :: (Maybe (Array String)),
  prerequisites :: (DepList String),
  categories :: (DepList String),
  visibility :: Visibility,
  counter :: Int,
  version :: (Maybe String),
  urls :: (Maybe (Array String)),
  icon :: (Maybe String),
  tags :: (Array String),
  guard :: Int
}
unwrapResourceRequest (ResourceRequest r) = r

instance resourceRequestEncodeJson :: EncodeJson ResourceRequest where
  encodeJson (ResourceRequest o) =
       "tag" := "ResourceRequest"
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


instance resourceRequestDecodeJson :: DecodeJson ResourceRequest where
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
    pure $ ResourceRequest {
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


instance resourceRequestRequestable :: Requestable ResourceRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance resourceRequestRespondable :: Respondable ResourceRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkResourceRequest
      <$> readProp "display_name" json
      <*> readProp "description" json
      <*> readProp "source" json
      <*> (unNullOrUndefined <$> readProp "author" json)
      <*> readProp "prerequisites" json
      <*> readProp "categories" json
      <*> readProp "visibility" json
      <*> readProp "counter" json
      <*> (unNullOrUndefined <$> readProp "version" json)
      <*> (unNullOrUndefined <$> readProp "urls" json)
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "tags" json
      <*> readProp "guard" json


instance resourceRequestIsForeign :: IsForeign ResourceRequest where
  read json =
      mkResourceRequest
      <$> readProp "display_name" json
      <*> readProp "description" json
      <*> readProp "source" json
      <*> (unNullOrUndefined <$> readProp "author" json)
      <*> readProp "prerequisites" json
      <*> readProp "categories" json
      <*> readProp "visibility" json
      <*> readProp "counter" json
      <*> (unNullOrUndefined <$> readProp "version" json)
      <*> (unNullOrUndefined <$> readProp "urls" json)
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "tags" json
      <*> readProp "guard" json


newtype ResourceResponse = ResourceResponse {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: String,
  source :: ResourceType,
  author :: (Maybe (Array String)),
  prerequisites :: (DepList String),
  categories :: (DepList String),
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


type ResourceResponseR = {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: String,
  source :: ResourceType,
  author :: (Maybe (Array String)),
  prerequisites :: (DepList String),
  categories :: (DepList String),
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


_ResourceResponse :: Lens' ResourceResponse {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: String,
  source :: ResourceType,
  author :: (Maybe (Array String)),
  prerequisites :: (DepList String),
  categories :: (DepList String),
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
_ResourceResponse f (ResourceResponse o) = ResourceResponse <$> f o


mkResourceResponse :: Int -> Int -> String -> String -> String -> ResourceType -> (Maybe (Array String)) -> (DepList String) -> (DepList String) -> Visibility -> Int -> (Maybe String) -> (Maybe (Array String)) -> (Maybe String) -> (Array String) -> Boolean -> Int -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> ResourceResponse
mkResourceResponse id userId name displayName description source author prerequisites categories visibility counter version urls icon tags active guard createdAt modifiedAt activityAt =
  ResourceResponse{id, userId, name, displayName, description, source, author, prerequisites, categories, visibility, counter, version, urls, icon, tags, active, guard, createdAt, modifiedAt, activityAt}


unwrapResourceResponse :: ResourceResponse -> {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: String,
  source :: ResourceType,
  author :: (Maybe (Array String)),
  prerequisites :: (DepList String),
  categories :: (DepList String),
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
unwrapResourceResponse (ResourceResponse r) = r

instance resourceResponseEncodeJson :: EncodeJson ResourceResponse where
  encodeJson (ResourceResponse o) =
       "tag" := "ResourceResponse"
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


instance resourceResponseDecodeJson :: DecodeJson ResourceResponse where
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
    pure $ ResourceResponse {
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


instance resourceResponseRequestable :: Requestable ResourceResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance resourceResponseRespondable :: Respondable ResourceResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkResourceResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "name" json
      <*> readProp "display_name" json
      <*> readProp "description" json
      <*> readProp "source" json
      <*> (unNullOrUndefined <$> readProp "author" json)
      <*> readProp "prerequisites" json
      <*> readProp "categories" json
      <*> readProp "visibility" json
      <*> readProp "counter" json
      <*> (unNullOrUndefined <$> readProp "version" json)
      <*> (unNullOrUndefined <$> readProp "urls" json)
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "tags" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


instance resourceResponseIsForeign :: IsForeign ResourceResponse where
  read json =
      mkResourceResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "name" json
      <*> readProp "display_name" json
      <*> readProp "description" json
      <*> readProp "source" json
      <*> (unNullOrUndefined <$> readProp "author" json)
      <*> readProp "prerequisites" json
      <*> readProp "categories" json
      <*> readProp "visibility" json
      <*> readProp "counter" json
      <*> (unNullOrUndefined <$> readProp "version" json)
      <*> (unNullOrUndefined <$> readProp "urls" json)
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "tags" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


newtype ResourceResponses = ResourceResponses {
  resourceResponses :: (Array ResourceResponse)
}


type ResourceResponsesR = {
  resourceResponses :: (Array ResourceResponse)
}


_ResourceResponses :: Lens' ResourceResponses {
  resourceResponses :: (Array ResourceResponse)
}
_ResourceResponses f (ResourceResponses o) = ResourceResponses <$> f o


mkResourceResponses :: (Array ResourceResponse) -> ResourceResponses
mkResourceResponses resourceResponses =
  ResourceResponses{resourceResponses}


unwrapResourceResponses :: ResourceResponses -> {
  resourceResponses :: (Array ResourceResponse)
}
unwrapResourceResponses (ResourceResponses r) = r

instance resourceResponsesEncodeJson :: EncodeJson ResourceResponses where
  encodeJson (ResourceResponses o) =
       "tag" := "ResourceResponses"
    ~> "resource_responses" := o.resourceResponses
    ~> jsonEmptyObject


instance resourceResponsesDecodeJson :: DecodeJson ResourceResponses where
  decodeJson o = do
    obj <- decodeJson o
    resourceResponses <- obj .? "resource_responses"
    pure $ ResourceResponses {
      resourceResponses
    }


instance resourceResponsesRequestable :: Requestable ResourceResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance resourceResponsesRespondable :: Respondable ResourceResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkResourceResponses
      <$> readProp "resource_responses" json


instance resourceResponsesIsForeign :: IsForeign ResourceResponses where
  read json =
      mkResourceResponses
      <$> readProp "resource_responses" json


newtype ResourceStatResponse = ResourceStatResponse {
  resourceId :: Int,
  leurons :: Int,
  likes :: Int,
  neutral :: Int,
  dislikes :: Int,
  stars :: Int,
  views :: Int
}


type ResourceStatResponseR = {
  resourceId :: Int,
  leurons :: Int,
  likes :: Int,
  neutral :: Int,
  dislikes :: Int,
  stars :: Int,
  views :: Int
}


_ResourceStatResponse :: Lens' ResourceStatResponse {
  resourceId :: Int,
  leurons :: Int,
  likes :: Int,
  neutral :: Int,
  dislikes :: Int,
  stars :: Int,
  views :: Int
}
_ResourceStatResponse f (ResourceStatResponse o) = ResourceStatResponse <$> f o


mkResourceStatResponse :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> ResourceStatResponse
mkResourceStatResponse resourceId leurons likes neutral dislikes stars views =
  ResourceStatResponse{resourceId, leurons, likes, neutral, dislikes, stars, views}


unwrapResourceStatResponse :: ResourceStatResponse -> {
  resourceId :: Int,
  leurons :: Int,
  likes :: Int,
  neutral :: Int,
  dislikes :: Int,
  stars :: Int,
  views :: Int
}
unwrapResourceStatResponse (ResourceStatResponse r) = r

instance resourceStatResponseEncodeJson :: EncodeJson ResourceStatResponse where
  encodeJson (ResourceStatResponse o) =
       "tag" := "ResourceStatResponse"
    ~> "resource_id" := o.resourceId
    ~> "leurons" := o.leurons
    ~> "likes" := o.likes
    ~> "neutral" := o.neutral
    ~> "dislikes" := o.dislikes
    ~> "stars" := o.stars
    ~> "views" := o.views
    ~> jsonEmptyObject


instance resourceStatResponseDecodeJson :: DecodeJson ResourceStatResponse where
  decodeJson o = do
    obj <- decodeJson o
    resourceId <- obj .? "resource_id"
    leurons <- obj .? "leurons"
    likes <- obj .? "likes"
    neutral <- obj .? "neutral"
    dislikes <- obj .? "dislikes"
    stars <- obj .? "stars"
    views <- obj .? "views"
    pure $ ResourceStatResponse {
      resourceId,
      leurons,
      likes,
      neutral,
      dislikes,
      stars,
      views
    }


instance resourceStatResponseRequestable :: Requestable ResourceStatResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance resourceStatResponseRespondable :: Respondable ResourceStatResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkResourceStatResponse
      <$> readProp "resource_id" json
      <*> readProp "leurons" json
      <*> readProp "likes" json
      <*> readProp "neutral" json
      <*> readProp "dislikes" json
      <*> readProp "stars" json
      <*> readProp "views" json


instance resourceStatResponseIsForeign :: IsForeign ResourceStatResponse where
  read json =
      mkResourceStatResponse
      <$> readProp "resource_id" json
      <*> readProp "leurons" json
      <*> readProp "likes" json
      <*> readProp "neutral" json
      <*> readProp "dislikes" json
      <*> readProp "stars" json
      <*> readProp "views" json


newtype ResourceStatResponses = ResourceStatResponses {
  resourceStatResponses :: (Array ResourceStatResponse)
}


type ResourceStatResponsesR = {
  resourceStatResponses :: (Array ResourceStatResponse)
}


_ResourceStatResponses :: Lens' ResourceStatResponses {
  resourceStatResponses :: (Array ResourceStatResponse)
}
_ResourceStatResponses f (ResourceStatResponses o) = ResourceStatResponses <$> f o


mkResourceStatResponses :: (Array ResourceStatResponse) -> ResourceStatResponses
mkResourceStatResponses resourceStatResponses =
  ResourceStatResponses{resourceStatResponses}


unwrapResourceStatResponses :: ResourceStatResponses -> {
  resourceStatResponses :: (Array ResourceStatResponse)
}
unwrapResourceStatResponses (ResourceStatResponses r) = r

instance resourceStatResponsesEncodeJson :: EncodeJson ResourceStatResponses where
  encodeJson (ResourceStatResponses o) =
       "tag" := "ResourceStatResponses"
    ~> "resource_stat_responses" := o.resourceStatResponses
    ~> jsonEmptyObject


instance resourceStatResponsesDecodeJson :: DecodeJson ResourceStatResponses where
  decodeJson o = do
    obj <- decodeJson o
    resourceStatResponses <- obj .? "resource_stat_responses"
    pure $ ResourceStatResponses {
      resourceStatResponses
    }


instance resourceStatResponsesRequestable :: Requestable ResourceStatResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance resourceStatResponsesRespondable :: Respondable ResourceStatResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkResourceStatResponses
      <$> readProp "resource_stat_responses" json


instance resourceStatResponsesIsForeign :: IsForeign ResourceStatResponses where
  read json =
      mkResourceStatResponses
      <$> readProp "resource_stat_responses" json

-- footer