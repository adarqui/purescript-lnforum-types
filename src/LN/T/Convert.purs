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


forumRequestToForumResponse :: Int -> Int -> String -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> ForumRequest -> ForumResponse
forumRequestToForumResponse id userId name createdAt modifiedBy modifiedAt activityAt (ForumRequest o) =
  ForumResponse {
    id: id,
    userId: userId,
    name: name,
    displayName: o.displayName,
    description: o.description,
    threadsPerBoard: o.threadsPerBoard,
    threadPostsPerThread: o.threadPostsPerThread,
    recentThreadsLimit: o.recentThreadsLimit,
    recentPostsLimit: o.recentPostsLimit,
    motwLimit: o.motwLimit,
    icon: o.icon,
    visibility: o.visibility,
    tags: o.tags,
    guard: o.guard,
    createdAt: createdAt,
    modifiedBy: modifiedBy,
    modifiedAt: modifiedAt,
    activityAt: activityAt
  }


forumResponseToForumRequest :: ForumResponse -> ForumRequest
forumResponseToForumRequest  (ForumResponse o) =
  ForumRequest {
    displayName: o.displayName,
    description: o.description,
    threadsPerBoard: o.threadsPerBoard,
    threadPostsPerThread: o.threadPostsPerThread,
    recentThreadsLimit: o.recentThreadsLimit,
    recentPostsLimit: o.recentPostsLimit,
    motwLimit: o.motwLimit,
    icon: o.icon,
    tags: o.tags,
    visibility: o.visibility,
    guard: o.guard
  }


boardRequestToBoardResponse :: Int -> Int -> String -> (Maybe Date) -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> BoardRequest -> BoardResponse
boardRequestToBoardResponse id userId name createdAt modifiedAt modifiedBy activityAt (BoardRequest o) =
  BoardResponse {
    id: id,
    userId: userId,
    name: name,
    displayName: o.displayName,
    description: o.description,
    boardType: o.boardType,
    active: o.active,
    isAnonymous: o.isAnonymous,
    canCreateBoards: o.canCreateBoards,
    canCreateThreads: o.canCreateThreads,
    visibility: o.visibility,
    icon: o.icon,
    tags: o.tags,
    guard: o.guard,
    createdAt: createdAt,
    modifiedAt: modifiedAt,
    modifiedBy: modifiedBy,
    activityAt: activityAt
  }


boardResponseToBoardRequest :: BoardResponse -> BoardRequest
boardResponseToBoardRequest  (BoardResponse o) =
  BoardRequest {
    displayName: o.displayName,
    description: o.description,
    boardType: o.boardType,
    active: o.active,
    isAnonymous: o.isAnonymous,
    canCreateBoards: o.canCreateBoards,
    canCreateThreads: o.canCreateThreads,
    visibility: o.visibility,
    icon: o.icon,
    tags: o.tags,
    guard: o.guard
  }


threadRequestToThreadResponse :: Int -> Int -> Int -> String -> Boolean -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> ThreadRequest -> ThreadResponse
threadRequestToThreadResponse id userId boardId name active createdAt modifiedBy modifiedAt activityAt (ThreadRequest o) =
  ThreadResponse {
    id: id,
    userId: userId,
    boardId: boardId,
    name: name,
    displayName: o.displayName,
    description: o.description,
    sticky: o.sticky,
    locked: o.locked,
    poll: o.poll,
    icon: o.icon,
    tags: o.tags,
    active: active,
    guard: o.guard,
    createdAt: createdAt,
    modifiedBy: modifiedBy,
    modifiedAt: modifiedAt,
    activityAt: activityAt
  }


threadResponseToThreadRequest :: (Maybe String) -> ThreadResponse -> ThreadRequest
threadResponseToThreadRequest stateTag (ThreadResponse o) =
  ThreadRequest {
    displayName: o.displayName,
    description: o.description,
    sticky: o.sticky,
    locked: o.locked,
    poll: o.poll,
    icon: o.icon,
    tags: o.tags,
    guard: o.guard,
    stateTag: stateTag
  }


threadPostRequestToThreadPostResponse :: Int -> Int -> Int -> Int -> (Maybe Int) -> Boolean -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> ThreadPostRequest -> ThreadPostResponse
threadPostRequestToThreadPostResponse id userId boardId threadId parentId active createdAt modifiedBy modifiedAt activityAt (ThreadPostRequest o) =
  ThreadPostResponse {
    id: id,
    userId: userId,
    boardId: boardId,
    threadId: threadId,
    parentId: parentId,
    title: o.title,
    body: o.body,
    tags: o.tags,
    privateTags: o.privateTags,
    active: active,
    guard: o.guard,
    createdAt: createdAt,
    modifiedBy: modifiedBy,
    modifiedAt: modifiedAt,
    activityAt: activityAt
  }


threadPostResponseToThreadPostRequest :: (Maybe String) -> (Maybe String) -> ThreadPostResponse -> ThreadPostRequest
threadPostResponseToThreadPostRequest stateTag statePrivateTag (ThreadPostResponse o) =
  ThreadPostRequest {
    title: o.title,
    body: o.body,
    tags: o.tags,
    privateTags: o.privateTags,
    guard: o.guard,
    stateTag: stateTag,
    statePrivateTag: statePrivateTag
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