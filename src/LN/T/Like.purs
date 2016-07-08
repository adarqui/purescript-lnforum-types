module LN.T.Like where


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

data LikeOpt
  = Like 
  | Neutral 
  | Dislike 



instance likeOptEncodeJson :: EncodeJson LikeOpt where
  encodeJson (Like ) =
       "tag" := "Like"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Neutral ) =
       "tag" := "Neutral"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Dislike ) =
       "tag" := "Dislike"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance likeOptDecodeJson :: DecodeJson LikeOpt where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Like" -> do
        pure Like

      "Neutral" -> do
        pure Neutral

      "Dislike" -> do
        pure Dislike

      _ -> Left $ "DecodeJson TypeMismatch for LikeOpt"



instance likeOptRequestable :: Requestable LikeOpt where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance likeOptRespondable :: Respondable LikeOpt where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "Like" -> do
        pure Like

      "Neutral" -> do
        pure Neutral

      "Dislike" -> do
        pure Dislike

      _ -> Left $ TypeMismatch "LikeOpt" "Respondable"



instance likeOptIsForeign :: IsForeign LikeOpt where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "Like" -> do
        pure Like

      "Neutral" -> do
        pure Neutral

      "Dislike" -> do
        pure Dislike

      _ -> Left $ TypeMismatch "LikeOpt" "IsForeign"



instance likeOptEq :: Eq LikeOpt where
  eq Like Like = true
  eq Neutral Neutral = true
  eq Dislike Dislike = true
  eq _ _ = false

readLikeOpt :: String -> Maybe LikeOpt
readLikeOpt "like" = Just Like
readLikeOpt "neutral" = Just Neutral
readLikeOpt "dislike" = Just Dislike
readLikeOpt _ = Nothing

newtype LikeRequest = LikeRequest {
  opt :: LikeOpt,
  reason :: (Maybe String),
  guard :: Int
}


type LikeRequestR = {
  opt :: LikeOpt,
  reason :: (Maybe String),
  guard :: Int
}


_LikeRequest :: Lens' LikeRequest {
  opt :: LikeOpt,
  reason :: (Maybe String),
  guard :: Int
}
_LikeRequest f (LikeRequest o) = LikeRequest <$> f o


mkLikeRequest :: LikeOpt -> (Maybe String) -> Int -> LikeRequest
mkLikeRequest opt reason guard =
  LikeRequest{opt, reason, guard}


unwrapLikeRequest :: LikeRequest -> {
  opt :: LikeOpt,
  reason :: (Maybe String),
  guard :: Int
}
unwrapLikeRequest (LikeRequest r) = r

instance likeRequestEncodeJson :: EncodeJson LikeRequest where
  encodeJson (LikeRequest o) =
       "tag" := "LikeRequest"
    ~> "opt" := o.opt
    ~> "reason" := o.reason
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance likeRequestDecodeJson :: DecodeJson LikeRequest where
  decodeJson o = do
    obj <- decodeJson o
    opt <- obj .? "opt"
    reason <- obj .? "reason"
    guard <- obj .? "guard"
    pure $ LikeRequest {
      opt,
      reason,
      guard
    }


instance likeRequestRequestable :: Requestable LikeRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance likeRequestRespondable :: Respondable LikeRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkLikeRequest
      <$> readProp "opt" json
      <*> (unNullOrUndefined <$> readProp "reason" json)
      <*> readProp "guard" json


instance likeRequestIsForeign :: IsForeign LikeRequest where
  read json =
      mkLikeRequest
      <$> readProp "opt" json
      <*> (unNullOrUndefined <$> readProp "reason" json)
      <*> readProp "guard" json


newtype LikeResponse = LikeResponse {
  id :: Int,
  ent :: Ent,
  entId :: Int,
  userId :: Int,
  opt :: LikeOpt,
  score :: Int,
  reason :: (Maybe String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}


type LikeResponseR = {
  id :: Int,
  ent :: Ent,
  entId :: Int,
  userId :: Int,
  opt :: LikeOpt,
  score :: Int,
  reason :: (Maybe String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}


_LikeResponse :: Lens' LikeResponse {
  id :: Int,
  ent :: Ent,
  entId :: Int,
  userId :: Int,
  opt :: LikeOpt,
  score :: Int,
  reason :: (Maybe String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}
_LikeResponse f (LikeResponse o) = LikeResponse <$> f o


mkLikeResponse :: Int -> Ent -> Int -> Int -> LikeOpt -> Int -> (Maybe String) -> Boolean -> Int -> (Maybe Date) -> (Maybe Date) -> LikeResponse
mkLikeResponse id ent entId userId opt score reason active guard createdAt modifiedAt =
  LikeResponse{id, ent, entId, userId, opt, score, reason, active, guard, createdAt, modifiedAt}


unwrapLikeResponse :: LikeResponse -> {
  id :: Int,
  ent :: Ent,
  entId :: Int,
  userId :: Int,
  opt :: LikeOpt,
  score :: Int,
  reason :: (Maybe String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}
unwrapLikeResponse (LikeResponse r) = r

instance likeResponseEncodeJson :: EncodeJson LikeResponse where
  encodeJson (LikeResponse o) =
       "tag" := "LikeResponse"
    ~> "id" := o.id
    ~> "ent" := o.ent
    ~> "ent_id" := o.entId
    ~> "user_id" := o.userId
    ~> "opt" := o.opt
    ~> "score" := o.score
    ~> "reason" := o.reason
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
    ~> jsonEmptyObject


instance likeResponseDecodeJson :: DecodeJson LikeResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    ent <- obj .? "ent"
    entId <- obj .? "ent_id"
    userId <- obj .? "user_id"
    opt <- obj .? "opt"
    score <- obj .? "score"
    reason <- obj .? "reason"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    pure $ LikeResponse {
      id,
      ent,
      entId,
      userId,
      opt,
      score,
      reason,
      active,
      guard,
      createdAt,
      modifiedAt
    }


instance likeResponseRequestable :: Requestable LikeResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance likeResponseRespondable :: Respondable LikeResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkLikeResponse
      <$> readProp "id" json
      <*> readProp "ent" json
      <*> readProp "ent_id" json
      <*> readProp "user_id" json
      <*> readProp "opt" json
      <*> readProp "score" json
      <*> (unNullOrUndefined <$> readProp "reason" json)
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)


instance likeResponseIsForeign :: IsForeign LikeResponse where
  read json =
      mkLikeResponse
      <$> readProp "id" json
      <*> readProp "ent" json
      <*> readProp "ent_id" json
      <*> readProp "user_id" json
      <*> readProp "opt" json
      <*> readProp "score" json
      <*> (unNullOrUndefined <$> readProp "reason" json)
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)


newtype LikeResponses = LikeResponses {
  likeResponses :: (Array LikeResponse)
}


type LikeResponsesR = {
  likeResponses :: (Array LikeResponse)
}


_LikeResponses :: Lens' LikeResponses {
  likeResponses :: (Array LikeResponse)
}
_LikeResponses f (LikeResponses o) = LikeResponses <$> f o


mkLikeResponses :: (Array LikeResponse) -> LikeResponses
mkLikeResponses likeResponses =
  LikeResponses{likeResponses}


unwrapLikeResponses :: LikeResponses -> {
  likeResponses :: (Array LikeResponse)
}
unwrapLikeResponses (LikeResponses r) = r

instance likeResponsesEncodeJson :: EncodeJson LikeResponses where
  encodeJson (LikeResponses o) =
       "tag" := "LikeResponses"
    ~> "like_responses" := o.likeResponses
    ~> jsonEmptyObject


instance likeResponsesDecodeJson :: DecodeJson LikeResponses where
  decodeJson o = do
    obj <- decodeJson o
    likeResponses <- obj .? "like_responses"
    pure $ LikeResponses {
      likeResponses
    }


instance likeResponsesRequestable :: Requestable LikeResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance likeResponsesRespondable :: Respondable LikeResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkLikeResponses
      <$> readProp "like_responses" json


instance likeResponsesIsForeign :: IsForeign LikeResponses where
  read json =
      mkLikeResponses
      <$> readProp "like_responses" json


newtype LikeStatResponse = LikeStatResponse {
  ent :: Ent,
  entId :: Int,
  score :: Int,
  like :: Int,
  neutral :: Int,
  dislike :: Int
}


type LikeStatResponseR = {
  ent :: Ent,
  entId :: Int,
  score :: Int,
  like :: Int,
  neutral :: Int,
  dislike :: Int
}


_LikeStatResponse :: Lens' LikeStatResponse {
  ent :: Ent,
  entId :: Int,
  score :: Int,
  like :: Int,
  neutral :: Int,
  dislike :: Int
}
_LikeStatResponse f (LikeStatResponse o) = LikeStatResponse <$> f o


mkLikeStatResponse :: Ent -> Int -> Int -> Int -> Int -> Int -> LikeStatResponse
mkLikeStatResponse ent entId score like neutral dislike =
  LikeStatResponse{ent, entId, score, like, neutral, dislike}


unwrapLikeStatResponse :: LikeStatResponse -> {
  ent :: Ent,
  entId :: Int,
  score :: Int,
  like :: Int,
  neutral :: Int,
  dislike :: Int
}
unwrapLikeStatResponse (LikeStatResponse r) = r

instance likeStatResponseEncodeJson :: EncodeJson LikeStatResponse where
  encodeJson (LikeStatResponse o) =
       "tag" := "LikeStatResponse"
    ~> "ent" := o.ent
    ~> "ent_id" := o.entId
    ~> "score" := o.score
    ~> "like" := o.like
    ~> "neutral" := o.neutral
    ~> "dislike" := o.dislike
    ~> jsonEmptyObject


instance likeStatResponseDecodeJson :: DecodeJson LikeStatResponse where
  decodeJson o = do
    obj <- decodeJson o
    ent <- obj .? "ent"
    entId <- obj .? "ent_id"
    score <- obj .? "score"
    like <- obj .? "like"
    neutral <- obj .? "neutral"
    dislike <- obj .? "dislike"
    pure $ LikeStatResponse {
      ent,
      entId,
      score,
      like,
      neutral,
      dislike
    }


instance likeStatResponseRequestable :: Requestable LikeStatResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance likeStatResponseRespondable :: Respondable LikeStatResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkLikeStatResponse
      <$> readProp "ent" json
      <*> readProp "ent_id" json
      <*> readProp "score" json
      <*> readProp "like" json
      <*> readProp "neutral" json
      <*> readProp "dislike" json


instance likeStatResponseIsForeign :: IsForeign LikeStatResponse where
  read json =
      mkLikeStatResponse
      <$> readProp "ent" json
      <*> readProp "ent_id" json
      <*> readProp "score" json
      <*> readProp "like" json
      <*> readProp "neutral" json
      <*> readProp "dislike" json


newtype LikeStatResponses = LikeStatResponses {
  likeStatResponses :: (Array LikeStatResponse)
}


type LikeStatResponsesR = {
  likeStatResponses :: (Array LikeStatResponse)
}


_LikeStatResponses :: Lens' LikeStatResponses {
  likeStatResponses :: (Array LikeStatResponse)
}
_LikeStatResponses f (LikeStatResponses o) = LikeStatResponses <$> f o


mkLikeStatResponses :: (Array LikeStatResponse) -> LikeStatResponses
mkLikeStatResponses likeStatResponses =
  LikeStatResponses{likeStatResponses}


unwrapLikeStatResponses :: LikeStatResponses -> {
  likeStatResponses :: (Array LikeStatResponse)
}
unwrapLikeStatResponses (LikeStatResponses r) = r

instance likeStatResponsesEncodeJson :: EncodeJson LikeStatResponses where
  encodeJson (LikeStatResponses o) =
       "tag" := "LikeStatResponses"
    ~> "like_stat_responses" := o.likeStatResponses
    ~> jsonEmptyObject


instance likeStatResponsesDecodeJson :: DecodeJson LikeStatResponses where
  decodeJson o = do
    obj <- decodeJson o
    likeStatResponses <- obj .? "like_stat_responses"
    pure $ LikeStatResponses {
      likeStatResponses
    }


instance likeStatResponsesRequestable :: Requestable LikeStatResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance likeStatResponsesRespondable :: Respondable LikeStatResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkLikeStatResponses
      <$> readProp "like_stat_responses" json


instance likeStatResponsesIsForeign :: IsForeign LikeStatResponses where
  read json =
      mkLikeStatResponses
      <$> readProp "like_stat_responses" json

-- footer