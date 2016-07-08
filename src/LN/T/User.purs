module LN.T.User where



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

newtype UserRequest = UserRequest {
  displayName :: String,
  fullName :: String,
  email :: String,
  plugin :: String,
  ident :: String,
  acceptTOS :: (Maybe Date)
}


type UserRequestR = {
  displayName :: String,
  fullName :: String,
  email :: String,
  plugin :: String,
  ident :: String,
  acceptTOS :: (Maybe Date)
}


_UserRequest :: Lens' UserRequest {
  displayName :: String,
  fullName :: String,
  email :: String,
  plugin :: String,
  ident :: String,
  acceptTOS :: (Maybe Date)
}
_UserRequest f (UserRequest o) = UserRequest <$> f o


mkUserRequest :: String -> String -> String -> String -> String -> (Maybe Date) -> UserRequest
mkUserRequest displayName fullName email plugin ident acceptTOS =
  UserRequest{displayName, fullName, email, plugin, ident, acceptTOS}


unwrapUserRequest :: UserRequest -> {
  displayName :: String,
  fullName :: String,
  email :: String,
  plugin :: String,
  ident :: String,
  acceptTOS :: (Maybe Date)
}
unwrapUserRequest (UserRequest r) = r

instance userRequestEncodeJson :: EncodeJson UserRequest where
  encodeJson (UserRequest o) =
       "tag" := "UserRequest"
    ~> "display_name" := o.displayName
    ~> "full_name" := o.fullName
    ~> "email" := o.email
    ~> "plugin" := o.plugin
    ~> "ident" := o.ident
    ~> "accept_tos" := o.acceptTOS
    ~> jsonEmptyObject


instance userRequestDecodeJson :: DecodeJson UserRequest where
  decodeJson o = do
    obj <- decodeJson o
    displayName <- obj .? "display_name"
    fullName <- obj .? "full_name"
    email <- obj .? "email"
    plugin <- obj .? "plugin"
    ident <- obj .? "ident"
    acceptTOS <- obj .? "accept_tos"
    pure $ UserRequest {
      displayName,
      fullName,
      email,
      plugin,
      ident,
      acceptTOS
    }


instance userRequestRequestable :: Requestable UserRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance userRequestRespondable :: Respondable UserRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkUserRequest
      <$> readProp "display_name" json
      <*> readProp "full_name" json
      <*> readProp "email" json
      <*> readProp "plugin" json
      <*> readProp "ident" json
      <*> (unNullOrUndefined <$> readProp "accept_tos" json)


instance userRequestIsForeign :: IsForeign UserRequest where
  read json =
      mkUserRequest
      <$> readProp "display_name" json
      <*> readProp "full_name" json
      <*> readProp "email" json
      <*> readProp "plugin" json
      <*> readProp "ident" json
      <*> (unNullOrUndefined <$> readProp "accept_tos" json)


newtype UserResponse = UserResponse {
  id :: Int,
  name :: String,
  displayName :: String,
  fullName :: String,
  email :: String,
  emailMD5 :: String,
  plugin :: String,
  ident :: String,
  acceptTOS :: (Maybe Date),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  deactivatedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


type UserResponseR = {
  id :: Int,
  name :: String,
  displayName :: String,
  fullName :: String,
  email :: String,
  emailMD5 :: String,
  plugin :: String,
  ident :: String,
  acceptTOS :: (Maybe Date),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  deactivatedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


_UserResponse :: Lens' UserResponse {
  id :: Int,
  name :: String,
  displayName :: String,
  fullName :: String,
  email :: String,
  emailMD5 :: String,
  plugin :: String,
  ident :: String,
  acceptTOS :: (Maybe Date),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  deactivatedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
_UserResponse f (UserResponse o) = UserResponse <$> f o


mkUserResponse :: Int -> String -> String -> String -> String -> String -> String -> String -> (Maybe Date) -> Boolean -> Int -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> UserResponse
mkUserResponse id name displayName fullName email emailMD5 plugin ident acceptTOS active guard createdAt modifiedAt deactivatedAt activityAt =
  UserResponse{id, name, displayName, fullName, email, emailMD5, plugin, ident, acceptTOS, active, guard, createdAt, modifiedAt, deactivatedAt, activityAt}


unwrapUserResponse :: UserResponse -> {
  id :: Int,
  name :: String,
  displayName :: String,
  fullName :: String,
  email :: String,
  emailMD5 :: String,
  plugin :: String,
  ident :: String,
  acceptTOS :: (Maybe Date),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  deactivatedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
unwrapUserResponse (UserResponse r) = r

instance userResponseEncodeJson :: EncodeJson UserResponse where
  encodeJson (UserResponse o) =
       "tag" := "UserResponse"
    ~> "id" := o.id
    ~> "name" := o.name
    ~> "display_name" := o.displayName
    ~> "full_name" := o.fullName
    ~> "email" := o.email
    ~> "email_md5" := o.emailMD5
    ~> "plugin" := o.plugin
    ~> "ident" := o.ident
    ~> "accept_tos" := o.acceptTOS
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
    ~> "deactivated_at" := o.deactivatedAt
    ~> "activity_at" := o.activityAt
    ~> jsonEmptyObject


instance userResponseDecodeJson :: DecodeJson UserResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    name <- obj .? "name"
    displayName <- obj .? "display_name"
    fullName <- obj .? "full_name"
    email <- obj .? "email"
    emailMD5 <- obj .? "email_md5"
    plugin <- obj .? "plugin"
    ident <- obj .? "ident"
    acceptTOS <- obj .? "accept_tos"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    deactivatedAt <- obj .? "deactivated_at"
    activityAt <- obj .? "activity_at"
    pure $ UserResponse {
      id,
      name,
      displayName,
      fullName,
      email,
      emailMD5,
      plugin,
      ident,
      acceptTOS,
      active,
      guard,
      createdAt,
      modifiedAt,
      deactivatedAt,
      activityAt
    }


instance userResponseRequestable :: Requestable UserResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance userResponseRespondable :: Respondable UserResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkUserResponse
      <$> readProp "id" json
      <*> readProp "name" json
      <*> readProp "display_name" json
      <*> readProp "full_name" json
      <*> readProp "email" json
      <*> readProp "email_md5" json
      <*> readProp "plugin" json
      <*> readProp "ident" json
      <*> (unNullOrUndefined <$> readProp "accept_tos" json)
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "deactivated_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


instance userResponseIsForeign :: IsForeign UserResponse where
  read json =
      mkUserResponse
      <$> readProp "id" json
      <*> readProp "name" json
      <*> readProp "display_name" json
      <*> readProp "full_name" json
      <*> readProp "email" json
      <*> readProp "email_md5" json
      <*> readProp "plugin" json
      <*> readProp "ident" json
      <*> (unNullOrUndefined <$> readProp "accept_tos" json)
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "deactivated_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


newtype UserResponses = UserResponses {
  userResponses :: (Array UserResponse)
}


type UserResponsesR = {
  userResponses :: (Array UserResponse)
}


_UserResponses :: Lens' UserResponses {
  userResponses :: (Array UserResponse)
}
_UserResponses f (UserResponses o) = UserResponses <$> f o


mkUserResponses :: (Array UserResponse) -> UserResponses
mkUserResponses userResponses =
  UserResponses{userResponses}


unwrapUserResponses :: UserResponses -> {
  userResponses :: (Array UserResponse)
}
unwrapUserResponses (UserResponses r) = r

instance userResponsesEncodeJson :: EncodeJson UserResponses where
  encodeJson (UserResponses o) =
       "tag" := "UserResponses"
    ~> "user_responses" := o.userResponses
    ~> jsonEmptyObject


instance userResponsesDecodeJson :: DecodeJson UserResponses where
  decodeJson o = do
    obj <- decodeJson o
    userResponses <- obj .? "user_responses"
    pure $ UserResponses {
      userResponses
    }


instance userResponsesRequestable :: Requestable UserResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance userResponsesRespondable :: Respondable UserResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkUserResponses
      <$> readProp "user_responses" json


instance userResponsesIsForeign :: IsForeign UserResponses where
  read json =
      mkUserResponses
      <$> readProp "user_responses" json


newtype UserSanitizedResponse = UserSanitizedResponse {
  id :: Int,
  name :: String,
  displayName :: String,
  emailMD5 :: String,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


type UserSanitizedResponseR = {
  id :: Int,
  name :: String,
  displayName :: String,
  emailMD5 :: String,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


_UserSanitizedResponse :: Lens' UserSanitizedResponse {
  id :: Int,
  name :: String,
  displayName :: String,
  emailMD5 :: String,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
_UserSanitizedResponse f (UserSanitizedResponse o) = UserSanitizedResponse <$> f o


mkUserSanitizedResponse :: Int -> String -> String -> String -> Boolean -> Int -> (Maybe Date) -> (Maybe Date) -> UserSanitizedResponse
mkUserSanitizedResponse id name displayName emailMD5 active guard createdAt activityAt =
  UserSanitizedResponse{id, name, displayName, emailMD5, active, guard, createdAt, activityAt}


unwrapUserSanitizedResponse :: UserSanitizedResponse -> {
  id :: Int,
  name :: String,
  displayName :: String,
  emailMD5 :: String,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
unwrapUserSanitizedResponse (UserSanitizedResponse r) = r

instance userSanitizedResponseEncodeJson :: EncodeJson UserSanitizedResponse where
  encodeJson (UserSanitizedResponse o) =
       "tag" := "UserSanitizedResponse"
    ~> "id" := o.id
    ~> "name" := o.name
    ~> "display_name" := o.displayName
    ~> "email_md5" := o.emailMD5
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "activity_at" := o.activityAt
    ~> jsonEmptyObject


instance userSanitizedResponseDecodeJson :: DecodeJson UserSanitizedResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    name <- obj .? "name"
    displayName <- obj .? "display_name"
    emailMD5 <- obj .? "email_md5"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    activityAt <- obj .? "activity_at"
    pure $ UserSanitizedResponse {
      id,
      name,
      displayName,
      emailMD5,
      active,
      guard,
      createdAt,
      activityAt
    }


instance userSanitizedResponseRequestable :: Requestable UserSanitizedResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance userSanitizedResponseRespondable :: Respondable UserSanitizedResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkUserSanitizedResponse
      <$> readProp "id" json
      <*> readProp "name" json
      <*> readProp "display_name" json
      <*> readProp "email_md5" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


instance userSanitizedResponseIsForeign :: IsForeign UserSanitizedResponse where
  read json =
      mkUserSanitizedResponse
      <$> readProp "id" json
      <*> readProp "name" json
      <*> readProp "display_name" json
      <*> readProp "email_md5" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


newtype UserSanitizedResponses = UserSanitizedResponses {
  userSanitizedResponses :: (Array UserSanitizedResponse)
}


type UserSanitizedResponsesR = {
  userSanitizedResponses :: (Array UserSanitizedResponse)
}


_UserSanitizedResponses :: Lens' UserSanitizedResponses {
  userSanitizedResponses :: (Array UserSanitizedResponse)
}
_UserSanitizedResponses f (UserSanitizedResponses o) = UserSanitizedResponses <$> f o


mkUserSanitizedResponses :: (Array UserSanitizedResponse) -> UserSanitizedResponses
mkUserSanitizedResponses userSanitizedResponses =
  UserSanitizedResponses{userSanitizedResponses}


unwrapUserSanitizedResponses :: UserSanitizedResponses -> {
  userSanitizedResponses :: (Array UserSanitizedResponse)
}
unwrapUserSanitizedResponses (UserSanitizedResponses r) = r

instance userSanitizedResponsesEncodeJson :: EncodeJson UserSanitizedResponses where
  encodeJson (UserSanitizedResponses o) =
       "tag" := "UserSanitizedResponses"
    ~> "user_sanitized_responses" := o.userSanitizedResponses
    ~> jsonEmptyObject


instance userSanitizedResponsesDecodeJson :: DecodeJson UserSanitizedResponses where
  decodeJson o = do
    obj <- decodeJson o
    userSanitizedResponses <- obj .? "user_sanitized_responses"
    pure $ UserSanitizedResponses {
      userSanitizedResponses
    }


instance userSanitizedResponsesRequestable :: Requestable UserSanitizedResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance userSanitizedResponsesRespondable :: Respondable UserSanitizedResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkUserSanitizedResponses
      <$> readProp "user_sanitized_responses" json


instance userSanitizedResponsesIsForeign :: IsForeign UserSanitizedResponses where
  read json =
      mkUserSanitizedResponses
      <$> readProp "user_sanitized_responses" json


newtype UserSanitizedStatResponse = UserSanitizedStatResponse {
  userId :: Int,
  threads :: Int,
  threadPosts :: Int,
  respect :: Int,
  resources :: Int,
  leurons :: Int,
  workouts :: Int
}


type UserSanitizedStatResponseR = {
  userId :: Int,
  threads :: Int,
  threadPosts :: Int,
  respect :: Int,
  resources :: Int,
  leurons :: Int,
  workouts :: Int
}


_UserSanitizedStatResponse :: Lens' UserSanitizedStatResponse {
  userId :: Int,
  threads :: Int,
  threadPosts :: Int,
  respect :: Int,
  resources :: Int,
  leurons :: Int,
  workouts :: Int
}
_UserSanitizedStatResponse f (UserSanitizedStatResponse o) = UserSanitizedStatResponse <$> f o


mkUserSanitizedStatResponse :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> UserSanitizedStatResponse
mkUserSanitizedStatResponse userId threads threadPosts respect resources leurons workouts =
  UserSanitizedStatResponse{userId, threads, threadPosts, respect, resources, leurons, workouts}


unwrapUserSanitizedStatResponse :: UserSanitizedStatResponse -> {
  userId :: Int,
  threads :: Int,
  threadPosts :: Int,
  respect :: Int,
  resources :: Int,
  leurons :: Int,
  workouts :: Int
}
unwrapUserSanitizedStatResponse (UserSanitizedStatResponse r) = r

instance userSanitizedStatResponseEncodeJson :: EncodeJson UserSanitizedStatResponse where
  encodeJson (UserSanitizedStatResponse o) =
       "tag" := "UserSanitizedStatResponse"
    ~> "user_id" := o.userId
    ~> "threads" := o.threads
    ~> "thread_posts" := o.threadPosts
    ~> "respect" := o.respect
    ~> "resources" := o.resources
    ~> "leurons" := o.leurons
    ~> "workouts" := o.workouts
    ~> jsonEmptyObject


instance userSanitizedStatResponseDecodeJson :: DecodeJson UserSanitizedStatResponse where
  decodeJson o = do
    obj <- decodeJson o
    userId <- obj .? "user_id"
    threads <- obj .? "threads"
    threadPosts <- obj .? "thread_posts"
    respect <- obj .? "respect"
    resources <- obj .? "resources"
    leurons <- obj .? "leurons"
    workouts <- obj .? "workouts"
    pure $ UserSanitizedStatResponse {
      userId,
      threads,
      threadPosts,
      respect,
      resources,
      leurons,
      workouts
    }


instance userSanitizedStatResponseRequestable :: Requestable UserSanitizedStatResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance userSanitizedStatResponseRespondable :: Respondable UserSanitizedStatResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkUserSanitizedStatResponse
      <$> readProp "user_id" json
      <*> readProp "threads" json
      <*> readProp "thread_posts" json
      <*> readProp "respect" json
      <*> readProp "resources" json
      <*> readProp "leurons" json
      <*> readProp "workouts" json


instance userSanitizedStatResponseIsForeign :: IsForeign UserSanitizedStatResponse where
  read json =
      mkUserSanitizedStatResponse
      <$> readProp "user_id" json
      <*> readProp "threads" json
      <*> readProp "thread_posts" json
      <*> readProp "respect" json
      <*> readProp "resources" json
      <*> readProp "leurons" json
      <*> readProp "workouts" json


newtype UserSanitizedStatResponses = UserSanitizedStatResponses {
  userSanitizedStatResponses :: (Array UserSanitizedStatResponse)
}


type UserSanitizedStatResponsesR = {
  userSanitizedStatResponses :: (Array UserSanitizedStatResponse)
}


_UserSanitizedStatResponses :: Lens' UserSanitizedStatResponses {
  userSanitizedStatResponses :: (Array UserSanitizedStatResponse)
}
_UserSanitizedStatResponses f (UserSanitizedStatResponses o) = UserSanitizedStatResponses <$> f o


mkUserSanitizedStatResponses :: (Array UserSanitizedStatResponse) -> UserSanitizedStatResponses
mkUserSanitizedStatResponses userSanitizedStatResponses =
  UserSanitizedStatResponses{userSanitizedStatResponses}


unwrapUserSanitizedStatResponses :: UserSanitizedStatResponses -> {
  userSanitizedStatResponses :: (Array UserSanitizedStatResponse)
}
unwrapUserSanitizedStatResponses (UserSanitizedStatResponses r) = r

instance userSanitizedStatResponsesEncodeJson :: EncodeJson UserSanitizedStatResponses where
  encodeJson (UserSanitizedStatResponses o) =
       "tag" := "UserSanitizedStatResponses"
    ~> "user_sanitized_stat_responses" := o.userSanitizedStatResponses
    ~> jsonEmptyObject


instance userSanitizedStatResponsesDecodeJson :: DecodeJson UserSanitizedStatResponses where
  decodeJson o = do
    obj <- decodeJson o
    userSanitizedStatResponses <- obj .? "user_sanitized_stat_responses"
    pure $ UserSanitizedStatResponses {
      userSanitizedStatResponses
    }


instance userSanitizedStatResponsesRequestable :: Requestable UserSanitizedStatResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance userSanitizedStatResponsesRespondable :: Respondable UserSanitizedStatResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkUserSanitizedStatResponses
      <$> readProp "user_sanitized_stat_responses" json


instance userSanitizedStatResponsesIsForeign :: IsForeign UserSanitizedStatResponses where
  read json =
      mkUserSanitizedStatResponses
      <$> readProp "user_sanitized_stat_responses" json

-- footer