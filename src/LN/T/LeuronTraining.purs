module LN.T.LeuronTraining where


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

import Purescript.Api.Helpers

data LeuronTrainingSummary
  = LTS_View 
  | LTS_Skip 
  | LTS_Know 
  | LTS_DontKnow 
  | LTS_DontUnderstand 
  | LTS_DontCare 
  | LTS_Protest 



instance leuronTrainingSummaryEncodeJson :: EncodeJson LeuronTrainingSummary where
  encodeJson (LTS_View ) =
       "tag" := "LTS_View"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (LTS_Skip ) =
       "tag" := "LTS_Skip"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (LTS_Know ) =
       "tag" := "LTS_Know"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (LTS_DontKnow ) =
       "tag" := "LTS_DontKnow"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (LTS_DontUnderstand ) =
       "tag" := "LTS_DontUnderstand"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (LTS_DontCare ) =
       "tag" := "LTS_DontCare"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (LTS_Protest ) =
       "tag" := "LTS_Protest"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance leuronTrainingSummaryDecodeJson :: DecodeJson LeuronTrainingSummary where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "LTS_View" -> do
        pure LTS_View

      "LTS_Skip" -> do
        pure LTS_Skip

      "LTS_Know" -> do
        pure LTS_Know

      "LTS_DontKnow" -> do
        pure LTS_DontKnow

      "LTS_DontUnderstand" -> do
        pure LTS_DontUnderstand

      "LTS_DontCare" -> do
        pure LTS_DontCare

      "LTS_Protest" -> do
        pure LTS_Protest

      _ -> Left $ "DecodeJson TypeMismatch for LeuronTrainingSummary"



instance leuronTrainingSummaryRequestable :: Requestable LeuronTrainingSummary where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance leuronTrainingSummaryRespondable :: Respondable LeuronTrainingSummary where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "LTS_View" -> do
        pure LTS_View

      "LTS_Skip" -> do
        pure LTS_Skip

      "LTS_Know" -> do
        pure LTS_Know

      "LTS_DontKnow" -> do
        pure LTS_DontKnow

      "LTS_DontUnderstand" -> do
        pure LTS_DontUnderstand

      "LTS_DontCare" -> do
        pure LTS_DontCare

      "LTS_Protest" -> do
        pure LTS_Protest

      _ -> Left $ TypeMismatch "LeuronTrainingSummary" "Respondable"



instance leuronTrainingSummaryIsForeign :: IsForeign LeuronTrainingSummary where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "LTS_View" -> do
        pure LTS_View

      "LTS_Skip" -> do
        pure LTS_Skip

      "LTS_Know" -> do
        pure LTS_Know

      "LTS_DontKnow" -> do
        pure LTS_DontKnow

      "LTS_DontUnderstand" -> do
        pure LTS_DontUnderstand

      "LTS_DontCare" -> do
        pure LTS_DontCare

      "LTS_Protest" -> do
        pure LTS_Protest

      _ -> Left $ TypeMismatch "LeuronTrainingSummary" "IsForeign"



instance leuronTrainingSummaryEq :: Eq LeuronTrainingSummary where
  eq LTS_View LTS_View = true
  eq LTS_Skip LTS_Skip = true
  eq LTS_Know LTS_Know = true
  eq LTS_DontKnow LTS_DontKnow = true
  eq LTS_DontUnderstand LTS_DontUnderstand = true
  eq LTS_DontCare LTS_DontCare = true
  eq LTS_Protest LTS_Protest = true
  eq _ _ = false

readLeuronTrainingSummary :: String -> Maybe LeuronTrainingSummary
readLeuronTrainingSummary "lts_view" = Just LTS_View
readLeuronTrainingSummary "lts_skip" = Just LTS_Skip
readLeuronTrainingSummary "lts_know" = Just LTS_Know
readLeuronTrainingSummary "lts_dont_know" = Just LTS_DontKnow
readLeuronTrainingSummary "lts_dont_understand" = Just LTS_DontUnderstand
readLeuronTrainingSummary "lts_dont_care" = Just LTS_DontCare
readLeuronTrainingSummary "lts_protest" = Just LTS_Protest
readLeuronTrainingSummary _ = Nothing

newtype LeuronTrainingRequest = LeuronTrainingRequest {
  summary :: LeuronTrainingSummary,
  guard :: Int
}


type LeuronTrainingRequestR = {
  summary :: LeuronTrainingSummary,
  guard :: Int
}


_LeuronTrainingRequest :: Lens' LeuronTrainingRequest {
  summary :: LeuronTrainingSummary,
  guard :: Int
}
_LeuronTrainingRequest f (LeuronTrainingRequest o) = LeuronTrainingRequest <$> f o


mkLeuronTrainingRequest :: LeuronTrainingSummary -> Int -> LeuronTrainingRequest
mkLeuronTrainingRequest summary guard =
  LeuronTrainingRequest{summary, guard}


unwrapLeuronTrainingRequest :: LeuronTrainingRequest -> {
  summary :: LeuronTrainingSummary,
  guard :: Int
}
unwrapLeuronTrainingRequest (LeuronTrainingRequest r) = r

instance leuronTrainingRequestEncodeJson :: EncodeJson LeuronTrainingRequest where
  encodeJson (LeuronTrainingRequest o) =
       "tag" := "LeuronTrainingRequest"
    ~> "summary" := o.summary
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance leuronTrainingRequestDecodeJson :: DecodeJson LeuronTrainingRequest where
  decodeJson o = do
    obj <- decodeJson o
    summary <- obj .? "summary"
    guard <- obj .? "guard"
    pure $ LeuronTrainingRequest {
      summary,
      guard
    }


instance leuronTrainingRequestRequestable :: Requestable LeuronTrainingRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance leuronTrainingRequestRespondable :: Respondable LeuronTrainingRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkLeuronTrainingRequest
      <$> readProp "summary" json
      <*> readProp "guard" json


instance leuronTrainingRequestIsForeign :: IsForeign LeuronTrainingRequest where
  read json =
      mkLeuronTrainingRequest
      <$> readProp "summary" json
      <*> readProp "guard" json


newtype LeuronTrainingResponse = LeuronTrainingResponse {
  id :: Int,
  userId :: Int,
  leuronId :: Int,
  summary :: LeuronTrainingSummary,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}


type LeuronTrainingResponseR = {
  id :: Int,
  userId :: Int,
  leuronId :: Int,
  summary :: LeuronTrainingSummary,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}


_LeuronTrainingResponse :: Lens' LeuronTrainingResponse {
  id :: Int,
  userId :: Int,
  leuronId :: Int,
  summary :: LeuronTrainingSummary,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}
_LeuronTrainingResponse f (LeuronTrainingResponse o) = LeuronTrainingResponse <$> f o


mkLeuronTrainingResponse :: Int -> Int -> Int -> LeuronTrainingSummary -> Int -> (Maybe Date) -> (Maybe Date) -> LeuronTrainingResponse
mkLeuronTrainingResponse id userId leuronId summary guard createdAt modifiedAt =
  LeuronTrainingResponse{id, userId, leuronId, summary, guard, createdAt, modifiedAt}


unwrapLeuronTrainingResponse :: LeuronTrainingResponse -> {
  id :: Int,
  userId :: Int,
  leuronId :: Int,
  summary :: LeuronTrainingSummary,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}
unwrapLeuronTrainingResponse (LeuronTrainingResponse r) = r

instance leuronTrainingResponseEncodeJson :: EncodeJson LeuronTrainingResponse where
  encodeJson (LeuronTrainingResponse o) =
       "tag" := "LeuronTrainingResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "leuron_id" := o.leuronId
    ~> "summary" := o.summary
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
    ~> jsonEmptyObject


instance leuronTrainingResponseDecodeJson :: DecodeJson LeuronTrainingResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    leuronId <- obj .? "leuron_id"
    summary <- obj .? "summary"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    pure $ LeuronTrainingResponse {
      id,
      userId,
      leuronId,
      summary,
      guard,
      createdAt,
      modifiedAt
    }


instance leuronTrainingResponseRequestable :: Requestable LeuronTrainingResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance leuronTrainingResponseRespondable :: Respondable LeuronTrainingResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkLeuronTrainingResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "leuron_id" json
      <*> readProp "summary" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)


instance leuronTrainingResponseIsForeign :: IsForeign LeuronTrainingResponse where
  read json =
      mkLeuronTrainingResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "leuron_id" json
      <*> readProp "summary" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)


newtype LeuronTrainingResponses = LeuronTrainingResponses {
  leuronTrainingResponses :: (Array LeuronTrainingResponse)
}


type LeuronTrainingResponsesR = {
  leuronTrainingResponses :: (Array LeuronTrainingResponse)
}


_LeuronTrainingResponses :: Lens' LeuronTrainingResponses {
  leuronTrainingResponses :: (Array LeuronTrainingResponse)
}
_LeuronTrainingResponses f (LeuronTrainingResponses o) = LeuronTrainingResponses <$> f o


mkLeuronTrainingResponses :: (Array LeuronTrainingResponse) -> LeuronTrainingResponses
mkLeuronTrainingResponses leuronTrainingResponses =
  LeuronTrainingResponses{leuronTrainingResponses}


unwrapLeuronTrainingResponses :: LeuronTrainingResponses -> {
  leuronTrainingResponses :: (Array LeuronTrainingResponse)
}
unwrapLeuronTrainingResponses (LeuronTrainingResponses r) = r

instance leuronTrainingResponsesEncodeJson :: EncodeJson LeuronTrainingResponses where
  encodeJson (LeuronTrainingResponses o) =
       "tag" := "LeuronTrainingResponses"
    ~> "leuron_training_responses" := o.leuronTrainingResponses
    ~> jsonEmptyObject


instance leuronTrainingResponsesDecodeJson :: DecodeJson LeuronTrainingResponses where
  decodeJson o = do
    obj <- decodeJson o
    leuronTrainingResponses <- obj .? "leuron_training_responses"
    pure $ LeuronTrainingResponses {
      leuronTrainingResponses
    }


instance leuronTrainingResponsesRequestable :: Requestable LeuronTrainingResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance leuronTrainingResponsesRespondable :: Respondable LeuronTrainingResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkLeuronTrainingResponses
      <$> readProp "leuron_training_responses" json


instance leuronTrainingResponsesIsForeign :: IsForeign LeuronTrainingResponses where
  read json =
      mkLeuronTrainingResponses
      <$> readProp "leuron_training_responses" json


newtype LeuronTrainingStatResponse = LeuronTrainingStatResponse {
  leuronTrainingId :: Int
}


type LeuronTrainingStatResponseR = {
  leuronTrainingId :: Int
}


_LeuronTrainingStatResponse :: Lens' LeuronTrainingStatResponse {
  leuronTrainingId :: Int
}
_LeuronTrainingStatResponse f (LeuronTrainingStatResponse o) = LeuronTrainingStatResponse <$> f o


mkLeuronTrainingStatResponse :: Int -> LeuronTrainingStatResponse
mkLeuronTrainingStatResponse leuronTrainingId =
  LeuronTrainingStatResponse{leuronTrainingId}


unwrapLeuronTrainingStatResponse :: LeuronTrainingStatResponse -> {
  leuronTrainingId :: Int
}
unwrapLeuronTrainingStatResponse (LeuronTrainingStatResponse r) = r

instance leuronTrainingStatResponseEncodeJson :: EncodeJson LeuronTrainingStatResponse where
  encodeJson (LeuronTrainingStatResponse o) =
       "tag" := "LeuronTrainingStatResponse"
    ~> "leuron_training_id" := o.leuronTrainingId
    ~> jsonEmptyObject


instance leuronTrainingStatResponseDecodeJson :: DecodeJson LeuronTrainingStatResponse where
  decodeJson o = do
    obj <- decodeJson o
    leuronTrainingId <- obj .? "leuron_training_id"
    pure $ LeuronTrainingStatResponse {
      leuronTrainingId
    }


instance leuronTrainingStatResponseRequestable :: Requestable LeuronTrainingStatResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance leuronTrainingStatResponseRespondable :: Respondable LeuronTrainingStatResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkLeuronTrainingStatResponse
      <$> readProp "leuron_training_id" json


instance leuronTrainingStatResponseIsForeign :: IsForeign LeuronTrainingStatResponse where
  read json =
      mkLeuronTrainingStatResponse
      <$> readProp "leuron_training_id" json


newtype LeuronTrainingStatResponses = LeuronTrainingStatResponses {
  leuronTrainingStatResponses :: (Array LeuronTrainingStatResponse)
}


type LeuronTrainingStatResponsesR = {
  leuronTrainingStatResponses :: (Array LeuronTrainingStatResponse)
}


_LeuronTrainingStatResponses :: Lens' LeuronTrainingStatResponses {
  leuronTrainingStatResponses :: (Array LeuronTrainingStatResponse)
}
_LeuronTrainingStatResponses f (LeuronTrainingStatResponses o) = LeuronTrainingStatResponses <$> f o


mkLeuronTrainingStatResponses :: (Array LeuronTrainingStatResponse) -> LeuronTrainingStatResponses
mkLeuronTrainingStatResponses leuronTrainingStatResponses =
  LeuronTrainingStatResponses{leuronTrainingStatResponses}


unwrapLeuronTrainingStatResponses :: LeuronTrainingStatResponses -> {
  leuronTrainingStatResponses :: (Array LeuronTrainingStatResponse)
}
unwrapLeuronTrainingStatResponses (LeuronTrainingStatResponses r) = r

instance leuronTrainingStatResponsesEncodeJson :: EncodeJson LeuronTrainingStatResponses where
  encodeJson (LeuronTrainingStatResponses o) =
       "tag" := "LeuronTrainingStatResponses"
    ~> "leuron_training_stat_responses" := o.leuronTrainingStatResponses
    ~> jsonEmptyObject


instance leuronTrainingStatResponsesDecodeJson :: DecodeJson LeuronTrainingStatResponses where
  decodeJson o = do
    obj <- decodeJson o
    leuronTrainingStatResponses <- obj .? "leuron_training_stat_responses"
    pure $ LeuronTrainingStatResponses {
      leuronTrainingStatResponses
    }


instance leuronTrainingStatResponsesRequestable :: Requestable LeuronTrainingStatResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance leuronTrainingStatResponsesRespondable :: Respondable LeuronTrainingStatResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkLeuronTrainingStatResponses
      <$> readProp "leuron_training_stat_responses" json


instance leuronTrainingStatResponsesIsForeign :: IsForeign LeuronTrainingStatResponses where
  read json =
      mkLeuronTrainingStatResponses
      <$> readProp "leuron_training_stat_responses" json

-- footer