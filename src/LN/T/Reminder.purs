module LN.T.Reminder where


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

newtype ReminderRequest = ReminderRequest {
  dataP :: String,
  guard :: Int
}


type ReminderRequestR = {
  dataP :: String,
  guard :: Int
}


_ReminderRequest :: Lens' ReminderRequest {
  dataP :: String,
  guard :: Int
}
_ReminderRequest f (ReminderRequest o) = ReminderRequest <$> f o


mkReminderRequest :: String -> Int -> ReminderRequest
mkReminderRequest dataP guard =
  ReminderRequest{dataP, guard}


unwrapReminderRequest :: ReminderRequest -> {
  dataP :: String,
  guard :: Int
}
unwrapReminderRequest (ReminderRequest r) = r

instance reminderRequestEncodeJson :: EncodeJson ReminderRequest where
  encodeJson (ReminderRequest o) =
       "tag" := "ReminderRequest"
    ~> "data" := o.dataP
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance reminderRequestDecodeJson :: DecodeJson ReminderRequest where
  decodeJson o = do
    obj <- decodeJson o
    dataP <- obj .? "data"
    guard <- obj .? "guard"
    pure $ ReminderRequest {
      dataP,
      guard
    }


instance reminderRequestRequestable :: Requestable ReminderRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance reminderRequestRespondable :: Respondable ReminderRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkReminderRequest
      <$> readProp "data" json
      <*> readProp "guard" json


instance reminderRequestIsForeign :: IsForeign ReminderRequest where
  read json =
      mkReminderRequest
      <$> readProp "data" json
      <*> readProp "guard" json


newtype ReminderResponse = ReminderResponse {
  id :: Int,
  userId :: Int,
  parentFolderId :: Int,
  dataP :: String,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


type ReminderResponseR = {
  id :: Int,
  userId :: Int,
  parentFolderId :: Int,
  dataP :: String,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


_ReminderResponse :: Lens' ReminderResponse {
  id :: Int,
  userId :: Int,
  parentFolderId :: Int,
  dataP :: String,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
_ReminderResponse f (ReminderResponse o) = ReminderResponse <$> f o


mkReminderResponse :: Int -> Int -> Int -> String -> Boolean -> Int -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> ReminderResponse
mkReminderResponse id userId parentFolderId dataP active guard createdAt modifiedAt activityAt =
  ReminderResponse{id, userId, parentFolderId, dataP, active, guard, createdAt, modifiedAt, activityAt}


unwrapReminderResponse :: ReminderResponse -> {
  id :: Int,
  userId :: Int,
  parentFolderId :: Int,
  dataP :: String,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
unwrapReminderResponse (ReminderResponse r) = r

instance reminderResponseEncodeJson :: EncodeJson ReminderResponse where
  encodeJson (ReminderResponse o) =
       "tag" := "ReminderResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "parent_folder_id" := o.parentFolderId
    ~> "data" := o.dataP
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
    ~> "activity_at" := o.activityAt
    ~> jsonEmptyObject


instance reminderResponseDecodeJson :: DecodeJson ReminderResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    parentFolderId <- obj .? "parent_folder_id"
    dataP <- obj .? "data"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    activityAt <- obj .? "activity_at"
    pure $ ReminderResponse {
      id,
      userId,
      parentFolderId,
      dataP,
      active,
      guard,
      createdAt,
      modifiedAt,
      activityAt
    }


instance reminderResponseRequestable :: Requestable ReminderResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance reminderResponseRespondable :: Respondable ReminderResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkReminderResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "parent_folder_id" json
      <*> readProp "data" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


instance reminderResponseIsForeign :: IsForeign ReminderResponse where
  read json =
      mkReminderResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "parent_folder_id" json
      <*> readProp "data" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


newtype ReminderResponses = ReminderResponses {
  reminderResponses :: (Array ReminderResponse)
}


type ReminderResponsesR = {
  reminderResponses :: (Array ReminderResponse)
}


_ReminderResponses :: Lens' ReminderResponses {
  reminderResponses :: (Array ReminderResponse)
}
_ReminderResponses f (ReminderResponses o) = ReminderResponses <$> f o


mkReminderResponses :: (Array ReminderResponse) -> ReminderResponses
mkReminderResponses reminderResponses =
  ReminderResponses{reminderResponses}


unwrapReminderResponses :: ReminderResponses -> {
  reminderResponses :: (Array ReminderResponse)
}
unwrapReminderResponses (ReminderResponses r) = r

instance reminderResponsesEncodeJson :: EncodeJson ReminderResponses where
  encodeJson (ReminderResponses o) =
       "tag" := "ReminderResponses"
    ~> "reminder_responses" := o.reminderResponses
    ~> jsonEmptyObject


instance reminderResponsesDecodeJson :: DecodeJson ReminderResponses where
  decodeJson o = do
    obj <- decodeJson o
    reminderResponses <- obj .? "reminder_responses"
    pure $ ReminderResponses {
      reminderResponses
    }


instance reminderResponsesRequestable :: Requestable ReminderResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance reminderResponsesRespondable :: Respondable ReminderResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkReminderResponses
      <$> readProp "reminder_responses" json


instance reminderResponsesIsForeign :: IsForeign ReminderResponses where
  read json =
      mkReminderResponses
      <$> readProp "reminder_responses" json


newtype ReminderFolderRequest = ReminderFolderRequest {
  displayName :: String,
  description :: (Maybe String),
  visibility :: Visibility,
  guard :: Int
}


type ReminderFolderRequestR = {
  displayName :: String,
  description :: (Maybe String),
  visibility :: Visibility,
  guard :: Int
}


_ReminderFolderRequest :: Lens' ReminderFolderRequest {
  displayName :: String,
  description :: (Maybe String),
  visibility :: Visibility,
  guard :: Int
}
_ReminderFolderRequest f (ReminderFolderRequest o) = ReminderFolderRequest <$> f o


mkReminderFolderRequest :: String -> (Maybe String) -> Visibility -> Int -> ReminderFolderRequest
mkReminderFolderRequest displayName description visibility guard =
  ReminderFolderRequest{displayName, description, visibility, guard}


unwrapReminderFolderRequest :: ReminderFolderRequest -> {
  displayName :: String,
  description :: (Maybe String),
  visibility :: Visibility,
  guard :: Int
}
unwrapReminderFolderRequest (ReminderFolderRequest r) = r

instance reminderFolderRequestEncodeJson :: EncodeJson ReminderFolderRequest where
  encodeJson (ReminderFolderRequest o) =
       "tag" := "ReminderFolderRequest"
    ~> "display_name" := o.displayName
    ~> "description" := o.description
    ~> "visibility" := o.visibility
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance reminderFolderRequestDecodeJson :: DecodeJson ReminderFolderRequest where
  decodeJson o = do
    obj <- decodeJson o
    displayName <- obj .? "display_name"
    description <- obj .? "description"
    visibility <- obj .? "visibility"
    guard <- obj .? "guard"
    pure $ ReminderFolderRequest {
      displayName,
      description,
      visibility,
      guard
    }


instance reminderFolderRequestRequestable :: Requestable ReminderFolderRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance reminderFolderRequestRespondable :: Respondable ReminderFolderRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkReminderFolderRequest
      <$> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "visibility" json
      <*> readProp "guard" json


instance reminderFolderRequestIsForeign :: IsForeign ReminderFolderRequest where
  read json =
      mkReminderFolderRequest
      <$> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "visibility" json
      <*> readProp "guard" json


newtype ReminderFolderResponse = ReminderFolderResponse {
  id :: Int,
  userId :: Int,
  parentFolderId :: (Maybe Int),
  name :: String,
  displayName :: String,
  visibility :: Visibility,
  description :: (Maybe String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


type ReminderFolderResponseR = {
  id :: Int,
  userId :: Int,
  parentFolderId :: (Maybe Int),
  name :: String,
  displayName :: String,
  visibility :: Visibility,
  description :: (Maybe String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


_ReminderFolderResponse :: Lens' ReminderFolderResponse {
  id :: Int,
  userId :: Int,
  parentFolderId :: (Maybe Int),
  name :: String,
  displayName :: String,
  visibility :: Visibility,
  description :: (Maybe String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
_ReminderFolderResponse f (ReminderFolderResponse o) = ReminderFolderResponse <$> f o


mkReminderFolderResponse :: Int -> Int -> (Maybe Int) -> String -> String -> Visibility -> (Maybe String) -> Boolean -> Int -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> ReminderFolderResponse
mkReminderFolderResponse id userId parentFolderId name displayName visibility description active guard createdAt modifiedAt activityAt =
  ReminderFolderResponse{id, userId, parentFolderId, name, displayName, visibility, description, active, guard, createdAt, modifiedAt, activityAt}


unwrapReminderFolderResponse :: ReminderFolderResponse -> {
  id :: Int,
  userId :: Int,
  parentFolderId :: (Maybe Int),
  name :: String,
  displayName :: String,
  visibility :: Visibility,
  description :: (Maybe String),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
unwrapReminderFolderResponse (ReminderFolderResponse r) = r

instance reminderFolderResponseEncodeJson :: EncodeJson ReminderFolderResponse where
  encodeJson (ReminderFolderResponse o) =
       "tag" := "ReminderFolderResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "parent_folder_id" := o.parentFolderId
    ~> "name" := o.name
    ~> "display_name" := o.displayName
    ~> "visibility" := o.visibility
    ~> "description" := o.description
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
    ~> "activity_at" := o.activityAt
    ~> jsonEmptyObject


instance reminderFolderResponseDecodeJson :: DecodeJson ReminderFolderResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    parentFolderId <- obj .? "parent_folder_id"
    name <- obj .? "name"
    displayName <- obj .? "display_name"
    visibility <- obj .? "visibility"
    description <- obj .? "description"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    activityAt <- obj .? "activity_at"
    pure $ ReminderFolderResponse {
      id,
      userId,
      parentFolderId,
      name,
      displayName,
      visibility,
      description,
      active,
      guard,
      createdAt,
      modifiedAt,
      activityAt
    }


instance reminderFolderResponseRequestable :: Requestable ReminderFolderResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance reminderFolderResponseRespondable :: Respondable ReminderFolderResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkReminderFolderResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> (unNullOrUndefined <$> readProp "parent_folder_id" json)
      <*> readProp "name" json
      <*> readProp "display_name" json
      <*> readProp "visibility" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


instance reminderFolderResponseIsForeign :: IsForeign ReminderFolderResponse where
  read json =
      mkReminderFolderResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> (unNullOrUndefined <$> readProp "parent_folder_id" json)
      <*> readProp "name" json
      <*> readProp "display_name" json
      <*> readProp "visibility" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


newtype ReminderFolderResponses = ReminderFolderResponses {
  reminderFolderResponses :: (Array ReminderFolderResponse)
}


type ReminderFolderResponsesR = {
  reminderFolderResponses :: (Array ReminderFolderResponse)
}


_ReminderFolderResponses :: Lens' ReminderFolderResponses {
  reminderFolderResponses :: (Array ReminderFolderResponse)
}
_ReminderFolderResponses f (ReminderFolderResponses o) = ReminderFolderResponses <$> f o


mkReminderFolderResponses :: (Array ReminderFolderResponse) -> ReminderFolderResponses
mkReminderFolderResponses reminderFolderResponses =
  ReminderFolderResponses{reminderFolderResponses}


unwrapReminderFolderResponses :: ReminderFolderResponses -> {
  reminderFolderResponses :: (Array ReminderFolderResponse)
}
unwrapReminderFolderResponses (ReminderFolderResponses r) = r

instance reminderFolderResponsesEncodeJson :: EncodeJson ReminderFolderResponses where
  encodeJson (ReminderFolderResponses o) =
       "tag" := "ReminderFolderResponses"
    ~> "reminder_folder_responses" := o.reminderFolderResponses
    ~> jsonEmptyObject


instance reminderFolderResponsesDecodeJson :: DecodeJson ReminderFolderResponses where
  decodeJson o = do
    obj <- decodeJson o
    reminderFolderResponses <- obj .? "reminder_folder_responses"
    pure $ ReminderFolderResponses {
      reminderFolderResponses
    }


instance reminderFolderResponsesRequestable :: Requestable ReminderFolderResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance reminderFolderResponsesRespondable :: Respondable ReminderFolderResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkReminderFolderResponses
      <$> readProp "reminder_folder_responses" json


instance reminderFolderResponsesIsForeign :: IsForeign ReminderFolderResponses where
  read json =
      mkReminderFolderResponses
      <$> readProp "reminder_folder_responses" json

-- footer