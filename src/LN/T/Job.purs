module LN.T.Job where
import LN.T


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

data Job
  = Job_Nop Unit Unit
  | Job_Ping Unit Date
  | Job_CreateUserProfile ((Tuple Int) ProfileRequest) ProfileResponse



instance jobEncodeJson :: EncodeJson Job where
  encodeJson (Job_Nop x0 x1) =
       "tag" := "Job_Nop"
    ~> "contents" := [encodeJson x0, encodeJson x1]
    ~> jsonEmptyObject
  encodeJson (Job_Ping x0 x1) =
       "tag" := "Job_Ping"
    ~> "contents" := [encodeJson x0, encodeJson x1]
    ~> jsonEmptyObject
  encodeJson (Job_CreateUserProfile x0 x1) =
       "tag" := "Job_CreateUserProfile"
    ~> "contents" := [encodeJson x0, encodeJson x1]
    ~> jsonEmptyObject


instance jobDecodeJson :: DecodeJson Job where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Job_Nop" -> do
        r <- obj .? "contents"
        case r of
          [x0, x1] -> Job_Nop <$> decodeJson x0 <*> decodeJson x1
          _ -> Left $ "DecodeJson TypeMismatch for Job_Nop"


      "Job_Ping" -> do
        r <- obj .? "contents"
        case r of
          [x0, x1] -> Job_Ping <$> decodeJson x0 <*> decodeJson x1
          _ -> Left $ "DecodeJson TypeMismatch for Job_Ping"


      "Job_CreateUserProfile" -> do
        r <- obj .? "contents"
        case r of
          [x0, x1] -> Job_CreateUserProfile <$> decodeJson x0 <*> decodeJson x1
          _ -> Left $ "DecodeJson TypeMismatch for Job_CreateUserProfile"


      _ -> Left $ "DecodeJson TypeMismatch for Job"



instance jobRequestable :: Requestable Job where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance jobRespondable :: Respondable Job where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "Job_Nop" -> do
        r <- readProp "contents" json
        case r of
          [x0, x1] -> Job_Nop <$> read x0 <*> read x1
          _ -> fail $ TypeMismatch "Job_Nop" "Respondable"


      "Job_Ping" -> do
        r <- readProp "contents" json
        case r of
          [x0, x1] -> Job_Ping <$> read x0 <*> read x1
          _ -> fail $ TypeMismatch "Job_Ping" "Respondable"


      "Job_CreateUserProfile" -> do
        r <- readProp "contents" json
        case r of
          [x0, x1] -> Job_CreateUserProfile <$> read x0 <*> read x1
          _ -> fail $ TypeMismatch "Job_CreateUserProfile" "Respondable"


      _ -> fail $ TypeMismatch "Job" "Respondable"



instance jobIsForeign :: IsForeign Job where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "Job_Nop" -> do
        r <- readProp "contents" json
        case r of
          [x0, x1] -> Job_Nop <$> read x0 <*> read x1
          _ -> fail $ TypeMismatch "Job_Nop" "IsForeign"


      "Job_Ping" -> do
        r <- readProp "contents" json
        case r of
          [x0, x1] -> Job_Ping <$> read x0 <*> read x1
          _ -> fail $ TypeMismatch "Job_Ping" "IsForeign"


      "Job_CreateUserProfile" -> do
        r <- readProp "contents" json
        case r of
          [x0, x1] -> Job_CreateUserProfile <$> read x0 <*> read x1
          _ -> fail $ TypeMismatch "Job_CreateUserProfile" "IsForeign"


      _ -> fail $ TypeMismatch "Job" "IsForeign"



instance jobEq :: Eq Job where
  eq (Job_Nop x0a x1a) (Job_Nop x0b x1b) = x0a == x0b && x1a == x1b
  eq (Job_Ping x0a x1a) (Job_Ping x0b x1b) = x0a == x0b && x1a == x1b
  eq (Job_CreateUserProfile x0a x1a) (Job_CreateUserProfile x0b x1b) = x0a == x0b && x1a == x1b
  eq _ _ = false

data Queue
  = QNop 
  | QPing 
  | QCreateUserProfile 
  | QCreateUserApi 
  | QAddThreadPostToSet 
  | QRemoveThreadPostFromSet 
  | QFixUserProfiles 
  | QFixThreadPostSets 



instance queueEncodeJson :: EncodeJson Queue where
  encodeJson (QNop ) =
       "tag" := "QNop"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (QPing ) =
       "tag" := "QPing"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (QCreateUserProfile ) =
       "tag" := "QCreateUserProfile"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (QCreateUserApi ) =
       "tag" := "QCreateUserApi"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (QAddThreadPostToSet ) =
       "tag" := "QAddThreadPostToSet"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (QRemoveThreadPostFromSet ) =
       "tag" := "QRemoveThreadPostFromSet"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (QFixUserProfiles ) =
       "tag" := "QFixUserProfiles"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (QFixThreadPostSets ) =
       "tag" := "QFixThreadPostSets"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance queueDecodeJson :: DecodeJson Queue where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "QNop" -> do
        pure QNop

      "QPing" -> do
        pure QPing

      "QCreateUserProfile" -> do
        pure QCreateUserProfile

      "QCreateUserApi" -> do
        pure QCreateUserApi

      "QAddThreadPostToSet" -> do
        pure QAddThreadPostToSet

      "QRemoveThreadPostFromSet" -> do
        pure QRemoveThreadPostFromSet

      "QFixUserProfiles" -> do
        pure QFixUserProfiles

      "QFixThreadPostSets" -> do
        pure QFixThreadPostSets

      _ -> Left $ "DecodeJson TypeMismatch for Queue"



instance queueRequestable :: Requestable Queue where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance queueRespondable :: Respondable Queue where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "QNop" -> do
        pure QNop

      "QPing" -> do
        pure QPing

      "QCreateUserProfile" -> do
        pure QCreateUserProfile

      "QCreateUserApi" -> do
        pure QCreateUserApi

      "QAddThreadPostToSet" -> do
        pure QAddThreadPostToSet

      "QRemoveThreadPostFromSet" -> do
        pure QRemoveThreadPostFromSet

      "QFixUserProfiles" -> do
        pure QFixUserProfiles

      "QFixThreadPostSets" -> do
        pure QFixThreadPostSets

      _ -> fail $ TypeMismatch "Queue" "Respondable"



instance queueIsForeign :: IsForeign Queue where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "QNop" -> do
        pure QNop

      "QPing" -> do
        pure QPing

      "QCreateUserProfile" -> do
        pure QCreateUserProfile

      "QCreateUserApi" -> do
        pure QCreateUserApi

      "QAddThreadPostToSet" -> do
        pure QAddThreadPostToSet

      "QRemoveThreadPostFromSet" -> do
        pure QRemoveThreadPostFromSet

      "QFixUserProfiles" -> do
        pure QFixUserProfiles

      "QFixThreadPostSets" -> do
        pure QFixThreadPostSets

      _ -> fail $ TypeMismatch "Queue" "IsForeign"



instance queueEq :: Eq Queue where
  eq QNop QNop = true
  eq QPing QPing = true
  eq QCreateUserProfile QCreateUserProfile = true
  eq QCreateUserApi QCreateUserApi = true
  eq QAddThreadPostToSet QAddThreadPostToSet = true
  eq QRemoveThreadPostFromSet QRemoveThreadPostFromSet = true
  eq QFixUserProfiles QFixUserProfiles = true
  eq QFixThreadPostSets QFixThreadPostSets = true
  eq _ _ = false
-- footer