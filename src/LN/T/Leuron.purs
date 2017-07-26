module LN.T.Leuron where
import LN.T.DepList
import LN.T.Splits
import LN.T.Substitutions


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

newtype LeuronRequest = LeuronRequest {
  dataP :: LeuronData,
  title :: (Maybe String),
  description :: (Maybe String),
  section :: (Maybe String),
  page :: (Maybe String),
  examples :: (Maybe (Array String)),
  strengths :: (Maybe (Array String)),
  categories :: (DepList String),
  splits :: (Maybe (Array Splits)),
  substitutions :: (Maybe (Array Substitutions)),
  tags :: (Array String),
  style :: (Maybe (Array String)),
  guard :: Int
}


type LeuronRequestR = {
  dataP :: LeuronData,
  title :: (Maybe String),
  description :: (Maybe String),
  section :: (Maybe String),
  page :: (Maybe String),
  examples :: (Maybe (Array String)),
  strengths :: (Maybe (Array String)),
  categories :: (DepList String),
  splits :: (Maybe (Array Splits)),
  substitutions :: (Maybe (Array Substitutions)),
  tags :: (Array String),
  style :: (Maybe (Array String)),
  guard :: Int
}


_LeuronRequest :: Lens' LeuronRequest {
  dataP :: LeuronData,
  title :: (Maybe String),
  description :: (Maybe String),
  section :: (Maybe String),
  page :: (Maybe String),
  examples :: (Maybe (Array String)),
  strengths :: (Maybe (Array String)),
  categories :: (DepList String),
  splits :: (Maybe (Array Splits)),
  substitutions :: (Maybe (Array Substitutions)),
  tags :: (Array String),
  style :: (Maybe (Array String)),
  guard :: Int
}
_LeuronRequest f (LeuronRequest o) = LeuronRequest <$> f o


mkLeuronRequest :: LeuronData -> (Maybe String) -> (Maybe String) -> (Maybe String) -> (Maybe String) -> (Maybe (Array String)) -> (Maybe (Array String)) -> (DepList String) -> (Maybe (Array Splits)) -> (Maybe (Array Substitutions)) -> (Array String) -> (Maybe (Array String)) -> Int -> LeuronRequest
mkLeuronRequest dataP title description section page examples strengths categories splits substitutions tags style guard =
  LeuronRequest{dataP, title, description, section, page, examples, strengths, categories, splits, substitutions, tags, style, guard}


unwrapLeuronRequest :: LeuronRequest -> {
  dataP :: LeuronData,
  title :: (Maybe String),
  description :: (Maybe String),
  section :: (Maybe String),
  page :: (Maybe String),
  examples :: (Maybe (Array String)),
  strengths :: (Maybe (Array String)),
  categories :: (DepList String),
  splits :: (Maybe (Array Splits)),
  substitutions :: (Maybe (Array Substitutions)),
  tags :: (Array String),
  style :: (Maybe (Array String)),
  guard :: Int
}
unwrapLeuronRequest (LeuronRequest r) = r

instance leuronRequestEncodeJson :: EncodeJson LeuronRequest where
  encodeJson (LeuronRequest o) =
       "tag" := "LeuronRequest"
    ~> "data" := o.dataP
    ~> "title" := o.title
    ~> "description" := o.description
    ~> "section" := o.section
    ~> "page" := o.page
    ~> "examples" := o.examples
    ~> "strengths" := o.strengths
    ~> "categories" := o.categories
    ~> "splits" := o.splits
    ~> "substitutions" := o.substitutions
    ~> "tags" := o.tags
    ~> "style" := o.style
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance leuronRequestDecodeJson :: DecodeJson LeuronRequest where
  decodeJson o = do
    obj <- decodeJson o
    dataP <- obj .? "data"
    title <- obj .? "title"
    description <- obj .? "description"
    section <- obj .? "section"
    page <- obj .? "page"
    examples <- obj .? "examples"
    strengths <- obj .? "strengths"
    categories <- obj .? "categories"
    splits <- obj .? "splits"
    substitutions <- obj .? "substitutions"
    tags <- obj .? "tags"
    style <- obj .? "style"
    guard <- obj .? "guard"
    pure $ LeuronRequest {
      dataP,
      title,
      description,
      section,
      page,
      examples,
      strengths,
      categories,
      splits,
      substitutions,
      tags,
      style,
      guard
    }


instance leuronRequestRequestable :: Requestable LeuronRequest where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance leuronRequestRespondable :: Respondable LeuronRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype LeuronResponse = LeuronResponse {
  id :: Int,
  userId :: Int,
  resourceId :: Int,
  dataP :: LeuronData,
  title :: (Maybe String),
  description :: (Maybe String),
  section :: (Maybe String),
  page :: (Maybe String),
  examples :: (Maybe (Array String)),
  strengths :: (Maybe (Array String)),
  categories :: (DepList String),
  splits :: (Maybe (Array Splits)),
  substitutions :: (Maybe (Array Substitutions)),
  tags :: (Array String),
  style :: (Maybe (Array String)),
  checksum :: String,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


type LeuronResponseR = {
  id :: Int,
  userId :: Int,
  resourceId :: Int,
  dataP :: LeuronData,
  title :: (Maybe String),
  description :: (Maybe String),
  section :: (Maybe String),
  page :: (Maybe String),
  examples :: (Maybe (Array String)),
  strengths :: (Maybe (Array String)),
  categories :: (DepList String),
  splits :: (Maybe (Array Splits)),
  substitutions :: (Maybe (Array Substitutions)),
  tags :: (Array String),
  style :: (Maybe (Array String)),
  checksum :: String,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


_LeuronResponse :: Lens' LeuronResponse {
  id :: Int,
  userId :: Int,
  resourceId :: Int,
  dataP :: LeuronData,
  title :: (Maybe String),
  description :: (Maybe String),
  section :: (Maybe String),
  page :: (Maybe String),
  examples :: (Maybe (Array String)),
  strengths :: (Maybe (Array String)),
  categories :: (DepList String),
  splits :: (Maybe (Array Splits)),
  substitutions :: (Maybe (Array Substitutions)),
  tags :: (Array String),
  style :: (Maybe (Array String)),
  checksum :: String,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
_LeuronResponse f (LeuronResponse o) = LeuronResponse <$> f o


mkLeuronResponse :: Int -> Int -> Int -> LeuronData -> (Maybe String) -> (Maybe String) -> (Maybe String) -> (Maybe String) -> (Maybe (Array String)) -> (Maybe (Array String)) -> (DepList String) -> (Maybe (Array Splits)) -> (Maybe (Array Substitutions)) -> (Array String) -> (Maybe (Array String)) -> String -> Boolean -> Int -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> LeuronResponse
mkLeuronResponse id userId resourceId dataP title description section page examples strengths categories splits substitutions tags style checksum active guard createdAt modifiedAt activityAt =
  LeuronResponse{id, userId, resourceId, dataP, title, description, section, page, examples, strengths, categories, splits, substitutions, tags, style, checksum, active, guard, createdAt, modifiedAt, activityAt}


unwrapLeuronResponse :: LeuronResponse -> {
  id :: Int,
  userId :: Int,
  resourceId :: Int,
  dataP :: LeuronData,
  title :: (Maybe String),
  description :: (Maybe String),
  section :: (Maybe String),
  page :: (Maybe String),
  examples :: (Maybe (Array String)),
  strengths :: (Maybe (Array String)),
  categories :: (DepList String),
  splits :: (Maybe (Array Splits)),
  substitutions :: (Maybe (Array Substitutions)),
  tags :: (Array String),
  style :: (Maybe (Array String)),
  checksum :: String,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
unwrapLeuronResponse (LeuronResponse r) = r

instance leuronResponseEncodeJson :: EncodeJson LeuronResponse where
  encodeJson (LeuronResponse o) =
       "tag" := "LeuronResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "resource_id" := o.resourceId
    ~> "data" := o.dataP
    ~> "title" := o.title
    ~> "description" := o.description
    ~> "section" := o.section
    ~> "page" := o.page
    ~> "examples" := o.examples
    ~> "strengths" := o.strengths
    ~> "categories" := o.categories
    ~> "splits" := o.splits
    ~> "substitutions" := o.substitutions
    ~> "tags" := o.tags
    ~> "style" := o.style
    ~> "checksum" := o.checksum
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
    ~> "activity_at" := o.activityAt
    ~> jsonEmptyObject


instance leuronResponseDecodeJson :: DecodeJson LeuronResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    resourceId <- obj .? "resource_id"
    dataP <- obj .? "data"
    title <- obj .? "title"
    description <- obj .? "description"
    section <- obj .? "section"
    page <- obj .? "page"
    examples <- obj .? "examples"
    strengths <- obj .? "strengths"
    categories <- obj .? "categories"
    splits <- obj .? "splits"
    substitutions <- obj .? "substitutions"
    tags <- obj .? "tags"
    style <- obj .? "style"
    checksum <- obj .? "checksum"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    activityAt <- obj .? "activity_at"
    pure $ LeuronResponse {
      id,
      userId,
      resourceId,
      dataP,
      title,
      description,
      section,
      page,
      examples,
      strengths,
      categories,
      splits,
      substitutions,
      tags,
      style,
      checksum,
      active,
      guard,
      createdAt,
      modifiedAt,
      activityAt
    }


instance leuronResponseRequestable :: Requestable LeuronResponse where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance leuronResponseRespondable :: Respondable LeuronResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype LeuronResponses = LeuronResponses {
  leuronResponses :: (Array LeuronResponse)
}


type LeuronResponsesR = {
  leuronResponses :: (Array LeuronResponse)
}


_LeuronResponses :: Lens' LeuronResponses {
  leuronResponses :: (Array LeuronResponse)
}
_LeuronResponses f (LeuronResponses o) = LeuronResponses <$> f o


mkLeuronResponses :: (Array LeuronResponse) -> LeuronResponses
mkLeuronResponses leuronResponses =
  LeuronResponses{leuronResponses}


unwrapLeuronResponses :: LeuronResponses -> {
  leuronResponses :: (Array LeuronResponse)
}
unwrapLeuronResponses (LeuronResponses r) = r

instance leuronResponsesEncodeJson :: EncodeJson LeuronResponses where
  encodeJson (LeuronResponses o) =
       "tag" := "LeuronResponses"
    ~> "leuron_responses" := o.leuronResponses
    ~> jsonEmptyObject


instance leuronResponsesDecodeJson :: DecodeJson LeuronResponses where
  decodeJson o = do
    obj <- decodeJson o
    leuronResponses <- obj .? "leuron_responses"
    pure $ LeuronResponses {
      leuronResponses
    }


instance leuronResponsesRequestable :: Requestable LeuronResponses where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance leuronResponsesRespondable :: Respondable LeuronResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype LeuronStatResponse = LeuronStatResponse {
  leuronId :: Int,
  likes :: Int,
  neutral :: Int,
  dislikes :: Int,
  stars :: Int,
  views :: Int
}


type LeuronStatResponseR = {
  leuronId :: Int,
  likes :: Int,
  neutral :: Int,
  dislikes :: Int,
  stars :: Int,
  views :: Int
}


_LeuronStatResponse :: Lens' LeuronStatResponse {
  leuronId :: Int,
  likes :: Int,
  neutral :: Int,
  dislikes :: Int,
  stars :: Int,
  views :: Int
}
_LeuronStatResponse f (LeuronStatResponse o) = LeuronStatResponse <$> f o


mkLeuronStatResponse :: Int -> Int -> Int -> Int -> Int -> Int -> LeuronStatResponse
mkLeuronStatResponse leuronId likes neutral dislikes stars views =
  LeuronStatResponse{leuronId, likes, neutral, dislikes, stars, views}


unwrapLeuronStatResponse :: LeuronStatResponse -> {
  leuronId :: Int,
  likes :: Int,
  neutral :: Int,
  dislikes :: Int,
  stars :: Int,
  views :: Int
}
unwrapLeuronStatResponse (LeuronStatResponse r) = r

instance leuronStatResponseEncodeJson :: EncodeJson LeuronStatResponse where
  encodeJson (LeuronStatResponse o) =
       "tag" := "LeuronStatResponse"
    ~> "leuron_id" := o.leuronId
    ~> "likes" := o.likes
    ~> "neutral" := o.neutral
    ~> "dislikes" := o.dislikes
    ~> "stars" := o.stars
    ~> "views" := o.views
    ~> jsonEmptyObject


instance leuronStatResponseDecodeJson :: DecodeJson LeuronStatResponse where
  decodeJson o = do
    obj <- decodeJson o
    leuronId <- obj .? "leuron_id"
    likes <- obj .? "likes"
    neutral <- obj .? "neutral"
    dislikes <- obj .? "dislikes"
    stars <- obj .? "stars"
    views <- obj .? "views"
    pure $ LeuronStatResponse {
      leuronId,
      likes,
      neutral,
      dislikes,
      stars,
      views
    }


instance leuronStatResponseRequestable :: Requestable LeuronStatResponse where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance leuronStatResponseRespondable :: Respondable LeuronStatResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype LeuronStatResponses = LeuronStatResponses {
  leuronStatResponses :: (Array LeuronStatResponse)
}


type LeuronStatResponsesR = {
  leuronStatResponses :: (Array LeuronStatResponse)
}


_LeuronStatResponses :: Lens' LeuronStatResponses {
  leuronStatResponses :: (Array LeuronStatResponse)
}
_LeuronStatResponses f (LeuronStatResponses o) = LeuronStatResponses <$> f o


mkLeuronStatResponses :: (Array LeuronStatResponse) -> LeuronStatResponses
mkLeuronStatResponses leuronStatResponses =
  LeuronStatResponses{leuronStatResponses}


unwrapLeuronStatResponses :: LeuronStatResponses -> {
  leuronStatResponses :: (Array LeuronStatResponse)
}
unwrapLeuronStatResponses (LeuronStatResponses r) = r

instance leuronStatResponsesEncodeJson :: EncodeJson LeuronStatResponses where
  encodeJson (LeuronStatResponses o) =
       "tag" := "LeuronStatResponses"
    ~> "leuron_stat_responses" := o.leuronStatResponses
    ~> jsonEmptyObject


instance leuronStatResponsesDecodeJson :: DecodeJson LeuronStatResponses where
  decodeJson o = do
    obj <- decodeJson o
    leuronStatResponses <- obj .? "leuron_stat_responses"
    pure $ LeuronStatResponses {
      leuronStatResponses
    }


instance leuronStatResponsesRequestable :: Requestable LeuronStatResponses where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance leuronStatResponsesRespondable :: Respondable LeuronStatResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


data LeuronData
  = LnFact Fact
  | LnFactList FactList
  | LnCard Card
  | LnDCard DCard
  | LnDCardX DCardX
  | LnAcronym Acronym
  | LnSynonym Synonym
  | LnAntonym Antonym
  | LnTemplate Template
  | LnImageAssociation ImageAssociation
  | LnLinearDemo LinearDemo
  | LnTable Table
  | LnScript Script
  | LnQA QA
  | LnExamples 
  | LnEmpty 



instance leuronDataEncodeJson :: EncodeJson LeuronData where
  encodeJson (LnFact x0) =
       "tag" := "LnFact"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (LnFactList x0) =
       "tag" := "LnFactList"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (LnCard x0) =
       "tag" := "LnCard"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (LnDCard x0) =
       "tag" := "LnDCard"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (LnDCardX x0) =
       "tag" := "LnDCardX"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (LnAcronym x0) =
       "tag" := "LnAcronym"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (LnSynonym x0) =
       "tag" := "LnSynonym"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (LnAntonym x0) =
       "tag" := "LnAntonym"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (LnTemplate x0) =
       "tag" := "LnTemplate"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (LnImageAssociation x0) =
       "tag" := "LnImageAssociation"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (LnLinearDemo x0) =
       "tag" := "LnLinearDemo"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (LnTable x0) =
       "tag" := "LnTable"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (LnScript x0) =
       "tag" := "LnScript"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (LnQA x0) =
       "tag" := "LnQA"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (LnExamples ) =
       "tag" := "LnExamples"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (LnEmpty ) =
       "tag" := "LnEmpty"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance leuronDataDecodeJson :: DecodeJson LeuronData where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "LnFact" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> LnFact <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for LnFact"


      "LnFactList" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> LnFactList <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for LnFactList"


      "LnCard" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> LnCard <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for LnCard"


      "LnDCard" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> LnDCard <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for LnDCard"


      "LnDCardX" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> LnDCardX <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for LnDCardX"


      "LnAcronym" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> LnAcronym <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for LnAcronym"


      "LnSynonym" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> LnSynonym <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for LnSynonym"


      "LnAntonym" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> LnAntonym <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for LnAntonym"


      "LnTemplate" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> LnTemplate <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for LnTemplate"


      "LnImageAssociation" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> LnImageAssociation <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for LnImageAssociation"


      "LnLinearDemo" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> LnLinearDemo <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for LnLinearDemo"


      "LnTable" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> LnTable <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for LnTable"


      "LnScript" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> LnScript <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for LnScript"


      "LnQA" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> LnQA <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for LnQA"


      "LnExamples" -> do
        pure LnExamples

      "LnEmpty" -> do
        pure LnEmpty

      _ -> Left $ "DecodeJson TypeMismatch for LeuronData"



instance leuronDataRequestable :: Requestable LeuronData where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance leuronDataRespondable :: Respondable LeuronData where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "LnFact" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> LnFact <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "LnFact" "Respondable"


      "LnFactList" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> LnFactList <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "LnFactList" "Respondable"


      "LnCard" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> LnCard <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "LnCard" "Respondable"


      "LnDCard" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> LnDCard <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "LnDCard" "Respondable"


      "LnDCardX" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> LnDCardX <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "LnDCardX" "Respondable"


      "LnAcronym" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> LnAcronym <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "LnAcronym" "Respondable"


      "LnSynonym" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> LnSynonym <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "LnSynonym" "Respondable"


      "LnAntonym" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> LnAntonym <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "LnAntonym" "Respondable"


      "LnTemplate" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> LnTemplate <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "LnTemplate" "Respondable"


      "LnImageAssociation" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> LnImageAssociation <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "LnImageAssociation" "Respondable"


      "LnLinearDemo" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> LnLinearDemo <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "LnLinearDemo" "Respondable"


      "LnTable" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> LnTable <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "LnTable" "Respondable"


      "LnScript" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> LnScript <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "LnScript" "Respondable"


      "LnQA" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> LnQA <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "LnQA" "Respondable"


      "LnExamples" -> do
        pure LnExamples

      "LnEmpty" -> do
        pure LnEmpty

      _ -> fail $ TypeMismatch "LeuronData" "Respondable"



data TyLeuron
  = TyLnFact 
  | TyLnFactList 
  | TyLnCard 
  | TyLnDCard 
  | TyLnDCardX 
  | TyLnAcronym 
  | TyLnSynonym 
  | TyLnAntonym 
  | TyLnTemplate 
  | TyLnImageAssociation 
  | TyLnLinearDemo 
  | TyLnTable 
  | TyLnScript 
  | TyLnQA 
  | TyLnExamples 
  | TyLnEmpty 



instance tyLeuronEncodeJson :: EncodeJson TyLeuron where
  encodeJson (TyLnFact ) =
       "tag" := "TyLnFact"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyLnFactList ) =
       "tag" := "TyLnFactList"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyLnCard ) =
       "tag" := "TyLnCard"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyLnDCard ) =
       "tag" := "TyLnDCard"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyLnDCardX ) =
       "tag" := "TyLnDCardX"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyLnAcronym ) =
       "tag" := "TyLnAcronym"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyLnSynonym ) =
       "tag" := "TyLnSynonym"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyLnAntonym ) =
       "tag" := "TyLnAntonym"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyLnTemplate ) =
       "tag" := "TyLnTemplate"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyLnImageAssociation ) =
       "tag" := "TyLnImageAssociation"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyLnLinearDemo ) =
       "tag" := "TyLnLinearDemo"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyLnTable ) =
       "tag" := "TyLnTable"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyLnScript ) =
       "tag" := "TyLnScript"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyLnQA ) =
       "tag" := "TyLnQA"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyLnExamples ) =
       "tag" := "TyLnExamples"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TyLnEmpty ) =
       "tag" := "TyLnEmpty"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance tyLeuronDecodeJson :: DecodeJson TyLeuron where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "TyLnFact" -> do
        pure TyLnFact

      "TyLnFactList" -> do
        pure TyLnFactList

      "TyLnCard" -> do
        pure TyLnCard

      "TyLnDCard" -> do
        pure TyLnDCard

      "TyLnDCardX" -> do
        pure TyLnDCardX

      "TyLnAcronym" -> do
        pure TyLnAcronym

      "TyLnSynonym" -> do
        pure TyLnSynonym

      "TyLnAntonym" -> do
        pure TyLnAntonym

      "TyLnTemplate" -> do
        pure TyLnTemplate

      "TyLnImageAssociation" -> do
        pure TyLnImageAssociation

      "TyLnLinearDemo" -> do
        pure TyLnLinearDemo

      "TyLnTable" -> do
        pure TyLnTable

      "TyLnScript" -> do
        pure TyLnScript

      "TyLnQA" -> do
        pure TyLnQA

      "TyLnExamples" -> do
        pure TyLnExamples

      "TyLnEmpty" -> do
        pure TyLnEmpty

      _ -> Left $ "DecodeJson TypeMismatch for TyLeuron"



instance tyLeuronRequestable :: Requestable TyLeuron where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance tyLeuronRespondable :: Respondable TyLeuron where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "TyLnFact" -> do
        pure TyLnFact

      "TyLnFactList" -> do
        pure TyLnFactList

      "TyLnCard" -> do
        pure TyLnCard

      "TyLnDCard" -> do
        pure TyLnDCard

      "TyLnDCardX" -> do
        pure TyLnDCardX

      "TyLnAcronym" -> do
        pure TyLnAcronym

      "TyLnSynonym" -> do
        pure TyLnSynonym

      "TyLnAntonym" -> do
        pure TyLnAntonym

      "TyLnTemplate" -> do
        pure TyLnTemplate

      "TyLnImageAssociation" -> do
        pure TyLnImageAssociation

      "TyLnLinearDemo" -> do
        pure TyLnLinearDemo

      "TyLnTable" -> do
        pure TyLnTable

      "TyLnScript" -> do
        pure TyLnScript

      "TyLnQA" -> do
        pure TyLnQA

      "TyLnExamples" -> do
        pure TyLnExamples

      "TyLnEmpty" -> do
        pure TyLnEmpty

      _ -> fail $ TypeMismatch "TyLeuron" "Respondable"



instance tyLeuronEq :: Eq TyLeuron where
  eq TyLnFact TyLnFact = true
  eq TyLnFactList TyLnFactList = true
  eq TyLnCard TyLnCard = true
  eq TyLnDCard TyLnDCard = true
  eq TyLnDCardX TyLnDCardX = true
  eq TyLnAcronym TyLnAcronym = true
  eq TyLnSynonym TyLnSynonym = true
  eq TyLnAntonym TyLnAntonym = true
  eq TyLnTemplate TyLnTemplate = true
  eq TyLnImageAssociation TyLnImageAssociation = true
  eq TyLnLinearDemo TyLnLinearDemo = true
  eq TyLnTable TyLnTable = true
  eq TyLnScript TyLnScript = true
  eq TyLnQA TyLnQA = true
  eq TyLnExamples TyLnExamples = true
  eq TyLnEmpty TyLnEmpty = true
  eq _ _ = false

instance tyLeuronShow :: Show TyLeuron where
  show TyLnFact = "ty_ln_fact"
  show TyLnFactList = "ty_ln_fact_list"
  show TyLnCard = "ty_ln_card"
  show TyLnDCard = "ty_ln_dcard"
  show TyLnDCardX = "ty_ln_dcard_x"
  show TyLnAcronym = "ty_ln_acronym"
  show TyLnSynonym = "ty_ln_synonym"
  show TyLnAntonym = "ty_ln_antonym"
  show TyLnTemplate = "ty_ln_template"
  show TyLnImageAssociation = "ty_ln_image_association"
  show TyLnLinearDemo = "ty_ln_linear_demo"
  show TyLnTable = "ty_ln_table"
  show TyLnScript = "ty_ln_script"
  show TyLnQA = "ty_ln_qa"
  show TyLnExamples = "ty_ln_examples"
  show TyLnEmpty = "ty_ln_empty"


data LeuronStatus
  = LeuronKnow 
  | LeuronDontKnow 
  | LeuronDontCare 
  | LeuronProtest 



instance leuronStatusEncodeJson :: EncodeJson LeuronStatus where
  encodeJson (LeuronKnow ) =
       "tag" := "LeuronKnow"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (LeuronDontKnow ) =
       "tag" := "LeuronDontKnow"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (LeuronDontCare ) =
       "tag" := "LeuronDontCare"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (LeuronProtest ) =
       "tag" := "LeuronProtest"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance leuronStatusDecodeJson :: DecodeJson LeuronStatus where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "LeuronKnow" -> do
        pure LeuronKnow

      "LeuronDontKnow" -> do
        pure LeuronDontKnow

      "LeuronDontCare" -> do
        pure LeuronDontCare

      "LeuronProtest" -> do
        pure LeuronProtest

      _ -> Left $ "DecodeJson TypeMismatch for LeuronStatus"



instance leuronStatusRequestable :: Requestable LeuronStatus where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance leuronStatusRespondable :: Respondable LeuronStatus where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "LeuronKnow" -> do
        pure LeuronKnow

      "LeuronDontKnow" -> do
        pure LeuronDontKnow

      "LeuronDontCare" -> do
        pure LeuronDontCare

      "LeuronProtest" -> do
        pure LeuronProtest

      _ -> fail $ TypeMismatch "LeuronStatus" "Respondable"



instance leuronStatusEq :: Eq LeuronStatus where
  eq LeuronKnow LeuronKnow = true
  eq LeuronDontKnow LeuronDontKnow = true
  eq LeuronDontCare LeuronDontCare = true
  eq LeuronProtest LeuronProtest = true
  eq _ _ = false

instance leuronStatusShow :: Show LeuronStatus where
  show LeuronKnow = "leuron_know"
  show LeuronDontKnow = "leuron_dont_know"
  show LeuronDontCare = "leuron_dont_care"
  show LeuronProtest = "leuron_protest"


newtype Fact = Fact {
  text :: String
}


type FactR = {
  text :: String
}


_Fact :: Lens' Fact {
  text :: String
}
_Fact f (Fact o) = Fact <$> f o


mkFact :: String -> Fact
mkFact text =
  Fact{text}


unwrapFact :: Fact -> {
  text :: String
}
unwrapFact (Fact r) = r

instance factEncodeJson :: EncodeJson Fact where
  encodeJson (Fact o) =
       "tag" := "Fact"
    ~> "text" := o.text
    ~> jsonEmptyObject


instance factDecodeJson :: DecodeJson Fact where
  decodeJson o = do
    obj <- decodeJson o
    text <- obj .? "text"
    pure $ Fact {
      text
    }


instance factRequestable :: Requestable Fact where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance factRespondable :: Respondable Fact where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype FactList = FactList {
  fact :: String,
  list :: (Array String)
}


type FactListR = {
  fact :: String,
  list :: (Array String)
}


_FactList :: Lens' FactList {
  fact :: String,
  list :: (Array String)
}
_FactList f (FactList o) = FactList <$> f o


mkFactList :: String -> (Array String) -> FactList
mkFactList fact list =
  FactList{fact, list}


unwrapFactList :: FactList -> {
  fact :: String,
  list :: (Array String)
}
unwrapFactList (FactList r) = r

instance factListEncodeJson :: EncodeJson FactList where
  encodeJson (FactList o) =
       "tag" := "FactList"
    ~> "fact" := o.fact
    ~> "list" := o.list
    ~> jsonEmptyObject


instance factListDecodeJson :: DecodeJson FactList where
  decodeJson o = do
    obj <- decodeJson o
    fact <- obj .? "fact"
    list <- obj .? "list"
    pure $ FactList {
      fact,
      list
    }


instance factListRequestable :: Requestable FactList where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance factListRespondable :: Respondable FactList where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype Card = Card {
  front :: String,
  back :: String
}


type CardR = {
  front :: String,
  back :: String
}


_Card :: Lens' Card {
  front :: String,
  back :: String
}
_Card f (Card o) = Card <$> f o


mkCard :: String -> String -> Card
mkCard front back =
  Card{front, back}


unwrapCard :: Card -> {
  front :: String,
  back :: String
}
unwrapCard (Card r) = r

instance cardEncodeJson :: EncodeJson Card where
  encodeJson (Card o) =
       "tag" := "Card"
    ~> "front" := o.front
    ~> "back" := o.back
    ~> jsonEmptyObject


instance cardDecodeJson :: DecodeJson Card where
  decodeJson o = do
    obj <- decodeJson o
    front <- obj .? "front"
    back <- obj .? "back"
    pure $ Card {
      front,
      back
    }


instance cardRequestable :: Requestable Card where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance cardRespondable :: Respondable Card where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype DCard = DCard {
  front :: String,
  back :: String
}


type DCardR = {
  front :: String,
  back :: String
}


_DCard :: Lens' DCard {
  front :: String,
  back :: String
}
_DCard f (DCard o) = DCard <$> f o


mkDCard :: String -> String -> DCard
mkDCard front back =
  DCard{front, back}


unwrapDCard :: DCard -> {
  front :: String,
  back :: String
}
unwrapDCard (DCard r) = r

instance dCardEncodeJson :: EncodeJson DCard where
  encodeJson (DCard o) =
       "tag" := "DCard"
    ~> "front" := o.front
    ~> "back" := o.back
    ~> jsonEmptyObject


instance dCardDecodeJson :: DecodeJson DCard where
  decodeJson o = do
    obj <- decodeJson o
    front <- obj .? "front"
    back <- obj .? "back"
    pure $ DCard {
      front,
      back
    }


instance dCardRequestable :: Requestable DCard where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance dCardRespondable :: Respondable DCard where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype DCardX = DCardX {
  front :: (Array String),
  back :: (Array String)
}


type DCardXR = {
  front :: (Array String),
  back :: (Array String)
}


_DCardX :: Lens' DCardX {
  front :: (Array String),
  back :: (Array String)
}
_DCardX f (DCardX o) = DCardX <$> f o


mkDCardX :: (Array String) -> (Array String) -> DCardX
mkDCardX front back =
  DCardX{front, back}


unwrapDCardX :: DCardX -> {
  front :: (Array String),
  back :: (Array String)
}
unwrapDCardX (DCardX r) = r

instance dCardXEncodeJson :: EncodeJson DCardX where
  encodeJson (DCardX o) =
       "tag" := "DCardX"
    ~> "front" := o.front
    ~> "back" := o.back
    ~> jsonEmptyObject


instance dCardXDecodeJson :: DecodeJson DCardX where
  decodeJson o = do
    obj <- decodeJson o
    front <- obj .? "front"
    back <- obj .? "back"
    pure $ DCardX {
      front,
      back
    }


instance dCardXRequestable :: Requestable DCardX where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance dCardXRespondable :: Respondable DCardX where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype Acronym = Acronym {
  abbreviation :: String,
  meaning :: String
}


type AcronymR = {
  abbreviation :: String,
  meaning :: String
}


_Acronym :: Lens' Acronym {
  abbreviation :: String,
  meaning :: String
}
_Acronym f (Acronym o) = Acronym <$> f o


mkAcronym :: String -> String -> Acronym
mkAcronym abbreviation meaning =
  Acronym{abbreviation, meaning}


unwrapAcronym :: Acronym -> {
  abbreviation :: String,
  meaning :: String
}
unwrapAcronym (Acronym r) = r

instance acronymEncodeJson :: EncodeJson Acronym where
  encodeJson (Acronym o) =
       "tag" := "Acronym"
    ~> "abbreviation" := o.abbreviation
    ~> "meaning" := o.meaning
    ~> jsonEmptyObject


instance acronymDecodeJson :: DecodeJson Acronym where
  decodeJson o = do
    obj <- decodeJson o
    abbreviation <- obj .? "abbreviation"
    meaning <- obj .? "meaning"
    pure $ Acronym {
      abbreviation,
      meaning
    }


instance acronymRequestable :: Requestable Acronym where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance acronymRespondable :: Respondable Acronym where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype Synonym = Synonym {
  a :: String,
  b :: String
}


type SynonymR = {
  a :: String,
  b :: String
}


_Synonym :: Lens' Synonym {
  a :: String,
  b :: String
}
_Synonym f (Synonym o) = Synonym <$> f o


mkSynonym :: String -> String -> Synonym
mkSynonym a b =
  Synonym{a, b}


unwrapSynonym :: Synonym -> {
  a :: String,
  b :: String
}
unwrapSynonym (Synonym r) = r

instance synonymEncodeJson :: EncodeJson Synonym where
  encodeJson (Synonym o) =
       "tag" := "Synonym"
    ~> "a" := o.a
    ~> "b" := o.b
    ~> jsonEmptyObject


instance synonymDecodeJson :: DecodeJson Synonym where
  decodeJson o = do
    obj <- decodeJson o
    a <- obj .? "a"
    b <- obj .? "b"
    pure $ Synonym {
      a,
      b
    }


instance synonymRequestable :: Requestable Synonym where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance synonymRespondable :: Respondable Synonym where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype Antonym = Antonym {
  a :: String,
  b :: String
}


type AntonymR = {
  a :: String,
  b :: String
}


_Antonym :: Lens' Antonym {
  a :: String,
  b :: String
}
_Antonym f (Antonym o) = Antonym <$> f o


mkAntonym :: String -> String -> Antonym
mkAntonym a b =
  Antonym{a, b}


unwrapAntonym :: Antonym -> {
  a :: String,
  b :: String
}
unwrapAntonym (Antonym r) = r

instance antonymEncodeJson :: EncodeJson Antonym where
  encodeJson (Antonym o) =
       "tag" := "Antonym"
    ~> "a" := o.a
    ~> "b" := o.b
    ~> jsonEmptyObject


instance antonymDecodeJson :: DecodeJson Antonym where
  decodeJson o = do
    obj <- decodeJson o
    a <- obj .? "a"
    b <- obj .? "b"
    pure $ Antonym {
      a,
      b
    }


instance antonymRequestable :: Requestable Antonym where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance antonymRespondable :: Respondable Antonym where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype Template = Template {
  template :: String,
  values :: (Array TemplateValue)
}


type TemplateR = {
  template :: String,
  values :: (Array TemplateValue)
}


_Template :: Lens' Template {
  template :: String,
  values :: (Array TemplateValue)
}
_Template f (Template o) = Template <$> f o


mkTemplate :: String -> (Array TemplateValue) -> Template
mkTemplate template values =
  Template{template, values}


unwrapTemplate :: Template -> {
  template :: String,
  values :: (Array TemplateValue)
}
unwrapTemplate (Template r) = r

instance templateEncodeJson :: EncodeJson Template where
  encodeJson (Template o) =
       "tag" := "Template"
    ~> "template" := o.template
    ~> "values" := o.values
    ~> jsonEmptyObject


instance templateDecodeJson :: DecodeJson Template where
  decodeJson o = do
    obj <- decodeJson o
    template <- obj .? "template"
    values <- obj .? "values"
    pure $ Template {
      template,
      values
    }


instance templateRequestable :: Requestable Template where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance templateRespondable :: Respondable Template where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


type TemplateValue  = ((Tuple String) (Array String))


newtype ImageAssociation = ImageAssociation {
  imageUrl :: (Array String),
  assocBy :: (Array String),
  assocResult :: (Array String)
}


type ImageAssociationR = {
  imageUrl :: (Array String),
  assocBy :: (Array String),
  assocResult :: (Array String)
}


_ImageAssociation :: Lens' ImageAssociation {
  imageUrl :: (Array String),
  assocBy :: (Array String),
  assocResult :: (Array String)
}
_ImageAssociation f (ImageAssociation o) = ImageAssociation <$> f o


mkImageAssociation :: (Array String) -> (Array String) -> (Array String) -> ImageAssociation
mkImageAssociation imageUrl assocBy assocResult =
  ImageAssociation{imageUrl, assocBy, assocResult}


unwrapImageAssociation :: ImageAssociation -> {
  imageUrl :: (Array String),
  assocBy :: (Array String),
  assocResult :: (Array String)
}
unwrapImageAssociation (ImageAssociation r) = r

instance imageAssociationEncodeJson :: EncodeJson ImageAssociation where
  encodeJson (ImageAssociation o) =
       "tag" := "ImageAssociation"
    ~> "image_url" := o.imageUrl
    ~> "assoc_by" := o.assocBy
    ~> "assoc_result" := o.assocResult
    ~> jsonEmptyObject


instance imageAssociationDecodeJson :: DecodeJson ImageAssociation where
  decodeJson o = do
    obj <- decodeJson o
    imageUrl <- obj .? "image_url"
    assocBy <- obj .? "assoc_by"
    assocResult <- obj .? "assoc_result"
    pure $ ImageAssociation {
      imageUrl,
      assocBy,
      assocResult
    }


instance imageAssociationRequestable :: Requestable ImageAssociation where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance imageAssociationRespondable :: Respondable ImageAssociation where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype Script = Script {
  title :: String,
  desc :: String,
  url :: String
}


type ScriptR = {
  title :: String,
  desc :: String,
  url :: String
}


_Script :: Lens' Script {
  title :: String,
  desc :: String,
  url :: String
}
_Script f (Script o) = Script <$> f o


mkScript :: String -> String -> String -> Script
mkScript title desc url =
  Script{title, desc, url}


unwrapScript :: Script -> {
  title :: String,
  desc :: String,
  url :: String
}
unwrapScript (Script r) = r

instance scriptEncodeJson :: EncodeJson Script where
  encodeJson (Script o) =
       "tag" := "Script"
    ~> "title" := o.title
    ~> "desc" := o.desc
    ~> "url" := o.url
    ~> jsonEmptyObject


instance scriptDecodeJson :: DecodeJson Script where
  decodeJson o = do
    obj <- decodeJson o
    title <- obj .? "title"
    desc <- obj .? "desc"
    url <- obj .? "url"
    pure $ Script {
      title,
      desc,
      url
    }


instance scriptRequestable :: Requestable Script where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance scriptRespondable :: Respondable Script where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


type LDContent  = String


type LDHint  = (Maybe String)


type LinearDemoNode  = ((Tuple LDContent) LDHint)


newtype LinearDemo = LinearDemo {
  label :: String,
  content :: (Array LinearDemoNode)
}


type LinearDemoR = {
  label :: String,
  content :: (Array LinearDemoNode)
}


_LinearDemo :: Lens' LinearDemo {
  label :: String,
  content :: (Array LinearDemoNode)
}
_LinearDemo f (LinearDemo o) = LinearDemo <$> f o


mkLinearDemo :: String -> (Array LinearDemoNode) -> LinearDemo
mkLinearDemo label content =
  LinearDemo{label, content}


unwrapLinearDemo :: LinearDemo -> {
  label :: String,
  content :: (Array LinearDemoNode)
}
unwrapLinearDemo (LinearDemo r) = r

instance linearDemoEncodeJson :: EncodeJson LinearDemo where
  encodeJson (LinearDemo o) =
       "tag" := "LinearDemo"
    ~> "label" := o.label
    ~> "content" := o.content
    ~> jsonEmptyObject


instance linearDemoDecodeJson :: DecodeJson LinearDemo where
  decodeJson o = do
    obj <- decodeJson o
    label <- obj .? "label"
    content <- obj .? "content"
    pure $ LinearDemo {
      label,
      content
    }


instance linearDemoRequestable :: Requestable LinearDemo where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance linearDemoRespondable :: Respondable LinearDemo where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype QA = QA {
  question :: String,
  answer :: String
}


type QAR = {
  question :: String,
  answer :: String
}


_QA :: Lens' QA {
  question :: String,
  answer :: String
}
_QA f (QA o) = QA <$> f o


mkQA :: String -> String -> QA
mkQA question answer =
  QA{question, answer}


unwrapQA :: QA -> {
  question :: String,
  answer :: String
}
unwrapQA (QA r) = r

instance qAEncodeJson :: EncodeJson QA where
  encodeJson (QA o) =
       "tag" := "QA"
    ~> "question" := o.question
    ~> "answer" := o.answer
    ~> jsonEmptyObject


instance qADecodeJson :: DecodeJson QA where
  decodeJson o = do
    obj <- decodeJson o
    question <- obj .? "question"
    answer <- obj .? "answer"
    pure $ QA {
      question,
      answer
    }


instance qARequestable :: Requestable QA where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance qARespondable :: Respondable QA where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype Table = Table {
  title :: String,
  columns :: (Array String),
  rows :: (Array (Array (Maybe String)))
}


type TableR = {
  title :: String,
  columns :: (Array String),
  rows :: (Array (Array (Maybe String)))
}


_Table :: Lens' Table {
  title :: String,
  columns :: (Array String),
  rows :: (Array (Array (Maybe String)))
}
_Table f (Table o) = Table <$> f o


mkTable :: String -> (Array String) -> (Array (Array (Maybe String))) -> Table
mkTable title columns rows =
  Table{title, columns, rows}


unwrapTable :: Table -> {
  title :: String,
  columns :: (Array String),
  rows :: (Array (Array (Maybe String)))
}
unwrapTable (Table r) = r

instance tableEncodeJson :: EncodeJson Table where
  encodeJson (Table o) =
       "tag" := "Table"
    ~> "title" := o.title
    ~> "columns" := o.columns
    ~> "rows" := o.rows
    ~> jsonEmptyObject


instance tableDecodeJson :: DecodeJson Table where
  decodeJson o = do
    obj <- decodeJson o
    title <- obj .? "title"
    columns <- obj .? "columns"
    rows <- obj .? "rows"
    pure $ Table {
      title,
      columns,
      rows
    }


instance tableRequestable :: Requestable Table where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance tableRespondable :: Respondable Table where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson

-- footer