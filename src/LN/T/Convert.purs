module LN.T.Convert where


import Data.Date.Helpers                (Date)
import Data.Maybe                       (Maybe)

import LN.T

apiRequestToApiResponse :: Int -> Int -> String -> (Maybe Date) -> (Maybe Date) -> ApiRequest -> ApiResponse
apiRequestToApiResponse id userId key createdAt modifiedAt (ApiRequest o) =
  ApiResponse {
    id: id,
    userId: userId,
    key: key,
    comment: o.comment,
    guard: o.guard,
    createdAt: createdAt,
    modifiedAt: modifiedAt
  }


apiResponseToApiRequest :: ApiResponse -> ApiRequest
apiResponseToApiRequest  (ApiResponse o) =
  ApiRequest {
    comment: o.comment,
    guard: o.guard
  }


idRequestToIdResponse :: Int -> Int -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> IdRequest -> IdResponse
idRequestToIdResponse id userId createdAt modifiedAt activityAt (IdRequest o) =
  IdResponse {
    id: id,
    userId: userId,
    targetId: o.targetId,
    guard: o.guard,
    createdAt: createdAt,
    modifiedAt: modifiedAt,
    activityAt: activityAt
  }


idResponseToIdRequest :: IdResponse -> IdRequest
idResponseToIdRequest  (IdResponse o) =
  IdRequest {
    targetId: o.targetId,
    guard: o.guard
  }


profileRequestToProfileResponse :: Int -> Ent -> Int -> Int -> Int -> (Maybe Date) -> (Maybe Date) -> ProfileRequest -> ProfileResponse
profileRequestToProfileResponse id ent entId karmaGood karmaBad createdAt modifiedAt (ProfileRequest o) =
  ProfileResponse {
    id: id,
    ent: ent,
    entId: entId,
    gender: o.gender,
    birthdate: o.birthdate,
    website: o.website,
    location: o.location,
    signature: o.signature,
    debug: o.debug,
    karmaGood: karmaGood,
    karmaBad: karmaBad,
    guard: o.guard,
    createdAt: createdAt,
    modifiedAt: modifiedAt
  }


profileResponseToProfileRequest :: (Array String) -> (Maybe String) -> ProfileResponse -> ProfileRequest
profileResponseToProfileRequest websites stateWebsites (ProfileResponse o) =
  ProfileRequest {
    gender: o.gender,
    birthdate: o.birthdate,
    website: o.website,
    websites: websites,
    location: o.location,
    signature: o.signature,
    debug: o.debug,
    guard: o.guard,
    stateWebsites: stateWebsites
  }


boardRequestToBoardResponse :: Int -> Int -> String -> Boolean -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> BoardRequest -> BoardResponse
boardRequestToBoardResponse id userId name active createdAt modifiedAt activityAt (BoardRequest o) =
  BoardResponse {
    id: id,
    userId: userId,
    name: name,
    displayName: o.displayName,
    description: o.description,
    source: o.source,
    author: o.author,
    prerequisites: o.prerequisites,
    categories: o.categories,
    visibility: o.visibility,
    counter: o.counter,
    version: o.version,
    urls: o.urls,
    icon: o.icon,
    tags: o.tags,
    active: active,
    guard: o.guard,
    createdAt: createdAt,
    modifiedAt: modifiedAt,
    activityAt: activityAt
  }


boardResponseToBoardRequest :: BoardResponse -> BoardRequest
boardResponseToBoardRequest  (BoardResponse o) =
  BoardRequest {
    displayName: o.displayName,
    description: o.description,
    source: o.source,
    author: o.author,
    prerequisites: o.prerequisites,
    categories: o.categories,
    visibility: o.visibility,
    counter: o.counter,
    version: o.version,
    urls: o.urls,
    icon: o.icon,
    tags: o.tags,
    guard: o.guard
  }


userRequestToUserResponse :: Int -> String -> String -> (Maybe String) -> (Maybe Date) -> (Maybe String) -> (Maybe Date) -> Boolean -> Int -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> UserRequest -> UserResponse
userRequestToUserResponse id name emailMD5 githubIdent githubCreatedAt googleIdent googleCreatedAt active guard createdAt modifiedAt deactivatedAt activityAt (UserRequest o) =
  UserResponse {
    id: id,
    name: name,
    displayName: o.displayName,
    fullName: o.fullName,
    email: o.email,
    emailMD5: emailMD5,
    plugin: o.plugin,
    githubIdent: githubIdent,
    githubCreatedAt: githubCreatedAt,
    googleIdent: googleIdent,
    googleCreatedAt: googleCreatedAt,
    acceptTOS: o.acceptTOS,
    active: active,
    guard: guard,
    createdAt: createdAt,
    modifiedAt: modifiedAt,
    deactivatedAt: deactivatedAt,
    activityAt: activityAt
  }


userResponseToUserRequest :: UserResponse -> UserRequest
userResponseToUserRequest  (UserResponse o) =
  UserRequest {
    displayName: o.displayName,
    fullName: o.fullName,
    email: o.email,
    plugin: o.plugin,
    acceptTOS: o.acceptTOS
  }


userRequestToUserSanitizedResponse :: Int -> String -> String -> Boolean -> Int -> (Maybe Date) -> (Maybe Date) -> UserRequest -> UserSanitizedResponse
userRequestToUserSanitizedResponse id name emailMD5 active guard createdAt activityAt (UserRequest o) =
  UserSanitizedResponse {
    id: id,
    name: name,
    displayName: o.displayName,
    emailMD5: emailMD5,
    active: active,
    guard: guard,
    createdAt: createdAt,
    activityAt: activityAt
  }


userSanitizedResponseToUserRequest :: String -> String -> String -> (Maybe Date) -> UserSanitizedResponse -> UserRequest
userSanitizedResponseToUserRequest fullName email plugin acceptTOS (UserSanitizedResponse o) =
  UserRequest {
    displayName: o.displayName,
    fullName: fullName,
    email: email,
    plugin: plugin,
    acceptTOS: acceptTOS
  }



-- footer