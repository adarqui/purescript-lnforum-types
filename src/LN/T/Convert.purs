module LN.T.Convert where


import Data.Date.Helpers                (Date)
import Data.Maybe                       (Maybe)

import LN.T.Internal.Types

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


boardRequestToBoardResponse :: Int -> Int -> Int -> Int -> (Maybe Int) -> String -> Boolean -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> BoardRequest -> BoardResponse
boardRequestToBoardResponse id userId orgId forumId parentId name active createdAt modifiedBy modifiedAt activityAt (BoardRequest o) =
  BoardResponse {
    id: id,
    userId: userId,
    orgId: orgId,
    forumId: forumId,
    parentId: parentId,
    name: name,
    displayName: o.displayName,
    description: o.description,
    isAnonymous: o.isAnonymous,
    canCreateSubBoards: o.canCreateSubBoards,
    canCreateThreads: o.canCreateThreads,
    suggestedTags: o.suggestedTags,
    icon: o.icon,
    tags: o.tags,
    active: active,
    guard: o.guard,
    createdAt: createdAt,
    modifiedBy: modifiedBy,
    modifiedAt: modifiedAt,
    activityAt: activityAt
  }


boardResponseToBoardRequest :: BoardResponse -> BoardRequest
boardResponseToBoardRequest  (BoardResponse o) =
  BoardRequest {
    displayName: o.displayName,
    description: o.description,
    isAnonymous: o.isAnonymous,
    canCreateSubBoards: o.canCreateSubBoards,
    canCreateThreads: o.canCreateThreads,
    suggestedTags: o.suggestedTags,
    icon: o.icon,
    tags: o.tags,
    guard: o.guard
  }


bucketRequestToBucketResponse :: Int -> Int -> String -> Boolean -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> BucketRequest -> BucketResponse
bucketRequestToBucketResponse id userId name active createdAt modifiedAt activityAt (BucketRequest o) =
  BucketResponse {
    id: id,
    userId: userId,
    name: name,
    displayName: o.displayName,
    description: o.description,
    scoreLo: o.scoreLo,
    scoreHi: o.scoreHi,
    leurons: o.leurons,
    resources: o.resources,
    categories: o.categories,
    filters: o.filters,
    active: active,
    guard: o.guard,
    createdAt: createdAt,
    modifiedAt: modifiedAt,
    activityAt: activityAt
  }


bucketResponseToBucketRequest :: BucketResponse -> BucketRequest
bucketResponseToBucketRequest  (BucketResponse o) =
  BucketRequest {
    displayName: o.displayName,
    description: o.description,
    scoreLo: o.scoreLo,
    scoreHi: o.scoreHi,
    leurons: o.leurons,
    resources: o.resources,
    categories: o.categories,
    filters: o.filters,
    guard: o.guard
  }


emptyRequestToEmptyResponse :: Int -> Int -> (Maybe Date) -> (Maybe Date) -> EmptyRequest -> EmptyResponse
emptyRequestToEmptyResponse id userId createdAt modifiedAt (EmptyRequest o) =
  EmptyResponse {
    id: id,
    userId: userId,
    value: o.value,
    createdAt: createdAt,
    modifiedAt: modifiedAt
  }


emptyResponseToEmptyRequest :: EmptyResponse -> EmptyRequest
emptyResponseToEmptyRequest  (EmptyResponse o) =
  EmptyRequest {
    value: o.value
  }


forumRequestToForumResponse :: Int -> Int -> Int -> String -> Boolean -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> ForumRequest -> ForumResponse
forumRequestToForumResponse id userId orgId name active createdAt modifiedBy modifiedAt activityAt (ForumRequest o) =
  ForumResponse {
    id: id,
    userId: userId,
    orgId: orgId,
    name: name,
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
    active: active,
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


leuronRequestToLeuronResponse :: Int -> Int -> Int -> Boolean -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> LeuronRequest -> LeuronResponse
leuronRequestToLeuronResponse id userId resourceId active createdAt modifiedAt activityAt (LeuronRequest o) =
  LeuronResponse {
    id: id,
    userId: userId,
    resourceId: resourceId,
    dataP: o.dataP,
    title: o.title,
    description: o.description,
    section: o.section,
    page: o.page,
    examples: o.examples,
    strengths: o.strengths,
    categories: o.categories,
    splits: o.splits,
    substitutions: o.substitutions,
    tags: o.tags,
    style: o.style,
    active: active,
    guard: o.guard,
    createdAt: createdAt,
    modifiedAt: modifiedAt,
    activityAt: activityAt
  }


leuronResponseToLeuronRequest :: LeuronResponse -> LeuronRequest
leuronResponseToLeuronRequest  (LeuronResponse o) =
  LeuronRequest {
    dataP: o.dataP,
    title: o.title,
    description: o.description,
    section: o.section,
    page: o.page,
    examples: o.examples,
    strengths: o.strengths,
    categories: o.categories,
    splits: o.splits,
    substitutions: o.substitutions,
    tags: o.tags,
    style: o.style,
    guard: o.guard
  }


leuronTrainingRequestToLeuronTrainingResponse :: Int -> Int -> Int -> (Maybe Date) -> (Maybe Date) -> LeuronTrainingRequest -> LeuronTrainingResponse
leuronTrainingRequestToLeuronTrainingResponse id userId leuronId createdAt modifiedAt (LeuronTrainingRequest o) =
  LeuronTrainingResponse {
    id: id,
    userId: userId,
    leuronId: leuronId,
    summary: o.summary,
    guard: o.guard,
    createdAt: createdAt,
    modifiedAt: modifiedAt
  }


leuronTrainingResponseToLeuronTrainingRequest :: LeuronTrainingResponse -> LeuronTrainingRequest
leuronTrainingResponseToLeuronTrainingRequest  (LeuronTrainingResponse o) =
  LeuronTrainingRequest {
    summary: o.summary,
    guard: o.guard
  }


likeRequestToLikeResponse :: Int -> Ent -> Int -> Int -> Int -> Boolean -> (Maybe Date) -> (Maybe Date) -> LikeRequest -> LikeResponse
likeRequestToLikeResponse id ent entId userId score active createdAt modifiedAt (LikeRequest o) =
  LikeResponse {
    id: id,
    ent: ent,
    entId: entId,
    userId: userId,
    opt: o.opt,
    score: score,
    reason: o.reason,
    active: active,
    guard: o.guard,
    createdAt: createdAt,
    modifiedAt: modifiedAt
  }


likeResponseToLikeRequest :: LikeResponse -> LikeRequest
likeResponseToLikeRequest  (LikeResponse o) =
  LikeRequest {
    opt: o.opt,
    reason: o.reason,
    guard: o.guard
  }


organizationRequestToOrganizationResponse :: Int -> Int -> String -> String -> Boolean -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> OrganizationRequest -> OrganizationResponse
organizationRequestToOrganizationResponse id userId name emailMD5 active createdAt modifiedBy modifiedAt activityAt (OrganizationRequest o) =
  OrganizationResponse {
    id: id,
    userId: userId,
    name: name,
    displayName: o.displayName,
    description: o.description,
    company: o.company,
    location: o.location,
    email: o.email,
    emailMD5: emailMD5,
    membership: o.membership,
    icon: o.icon,
    tags: o.tags,
    visibility: o.visibility,
    active: active,
    guard: o.guard,
    createdAt: createdAt,
    modifiedBy: modifiedBy,
    modifiedAt: modifiedAt,
    activityAt: activityAt
  }


organizationResponseToOrganizationRequest :: OrganizationResponse -> OrganizationRequest
organizationResponseToOrganizationRequest  (OrganizationResponse o) =
  OrganizationRequest {
    displayName: o.displayName,
    description: o.description,
    company: o.company,
    location: o.location,
    email: o.email,
    membership: o.membership,
    tags: o.tags,
    icon: o.icon,
    visibility: o.visibility,
    guard: o.guard
  }


pmRequestToPmResponse :: Int -> Int -> Int -> Boolean -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> PmRequest -> PmResponse
pmRequestToPmResponse id userId toUserId active createdAt modifiedAt activityAt (PmRequest o) =
  PmResponse {
    id: id,
    userId: userId,
    toUserId: toUserId,
    subject: o.subject,
    body: o.body,
    active: active,
    guard: o.guard,
    createdAt: createdAt,
    modifiedAt: modifiedAt,
    activityAt: activityAt
  }


pmResponseToPmRequest :: PmResponse -> PmRequest
pmResponseToPmRequest  (PmResponse o) =
  PmRequest {
    subject: o.subject,
    body: o.body,
    guard: o.guard
  }


pmInRequestToPmInResponse :: Int -> Int -> Int -> Boolean -> Boolean -> Boolean -> (Maybe Date) -> (Maybe Date) -> PmInRequest -> PmInResponse
pmInRequestToPmInResponse id pmId userId isNew isSaved active createdAt modifiedAt (PmInRequest o) =
  PmInResponse {
    id: id,
    pmId: pmId,
    userId: userId,
    label: o.label,
    isRead: o.isRead,
    isStarred: o.isStarred,
    isNew: isNew,
    isSaved: isSaved,
    active: active,
    guard: o.guard,
    createdAt: createdAt,
    modifiedAt: modifiedAt
  }


pmInResponseToPmInRequest :: PmInResponse -> PmInRequest
pmInResponseToPmInRequest  (PmInResponse o) =
  PmInRequest {
    label: o.label,
    isRead: o.isRead,
    isStarred: o.isStarred,
    guard: o.guard
  }


pmOutRequestToPmOutResponse :: Int -> Int -> Int -> Boolean -> Boolean -> (Maybe Date) -> (Maybe Date) -> PmOutRequest -> PmOutResponse
pmOutRequestToPmOutResponse id pmId userId isSaved active createdAt modifiedAt (PmOutRequest o) =
  PmOutResponse {
    id: id,
    pmId: pmId,
    userId: userId,
    label: o.label,
    isSaved: isSaved,
    active: active,
    guard: o.guard,
    createdAt: createdAt,
    modifiedAt: modifiedAt
  }


pmOutResponseToPmOutRequest :: PmOutResponse -> PmOutRequest
pmOutResponseToPmOutRequest  (PmOutResponse o) =
  PmOutRequest {
    label: o.label,
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


profileResponseToProfileRequest :: ProfileResponse -> ProfileRequest
profileResponseToProfileRequest  (ProfileResponse o) =
  ProfileRequest {
    gender: o.gender,
    birthdate: o.birthdate,
    website: o.website,
    location: o.location,
    signature: o.signature,
    debug: o.debug,
    guard: o.guard
  }


reminderRequestToReminderResponse :: Int -> Int -> Int -> Boolean -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> ReminderRequest -> ReminderResponse
reminderRequestToReminderResponse id userId parentFolderId active createdAt modifiedAt activityAt (ReminderRequest o) =
  ReminderResponse {
    id: id,
    userId: userId,
    parentFolderId: parentFolderId,
    dataP: o.dataP,
    active: active,
    guard: o.guard,
    createdAt: createdAt,
    modifiedAt: modifiedAt,
    activityAt: activityAt
  }


reminderResponseToReminderRequest :: ReminderResponse -> ReminderRequest
reminderResponseToReminderRequest  (ReminderResponse o) =
  ReminderRequest {
    dataP: o.dataP,
    guard: o.guard
  }


reminderFolderRequestToReminderFolderResponse :: Int -> Int -> (Maybe Int) -> String -> Boolean -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> ReminderFolderRequest -> ReminderFolderResponse
reminderFolderRequestToReminderFolderResponse id userId parentFolderId name active createdAt modifiedAt activityAt (ReminderFolderRequest o) =
  ReminderFolderResponse {
    id: id,
    userId: userId,
    parentFolderId: parentFolderId,
    name: name,
    displayName: o.displayName,
    visibility: o.visibility,
    description: o.description,
    active: active,
    guard: o.guard,
    createdAt: createdAt,
    modifiedAt: modifiedAt,
    activityAt: activityAt
  }


reminderFolderResponseToReminderFolderRequest :: ReminderFolderResponse -> ReminderFolderRequest
reminderFolderResponseToReminderFolderRequest  (ReminderFolderResponse o) =
  ReminderFolderRequest {
    displayName: o.displayName,
    description: o.description,
    visibility: o.visibility,
    guard: o.guard
  }


resourceRequestToResourceResponse :: Int -> Int -> String -> Boolean -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> ResourceRequest -> ResourceResponse
resourceRequestToResourceResponse id userId name active createdAt modifiedAt activityAt (ResourceRequest o) =
  ResourceResponse {
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


resourceResponseToResourceRequest :: ResourceResponse -> ResourceRequest
resourceResponseToResourceRequest  (ResourceResponse o) =
  ResourceRequest {
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


starRequestToStarResponse :: Int -> Ent -> Int -> Int -> Boolean -> (Maybe Date) -> (Maybe Date) -> StarRequest -> StarResponse
starRequestToStarResponse id ent entId userId active createdAt modifiedAt (StarRequest o) =
  StarResponse {
    id: id,
    ent: ent,
    entId: entId,
    userId: userId,
    reason: o.reason,
    active: active,
    guard: o.guard,
    createdAt: createdAt,
    modifiedAt: modifiedAt
  }


starResponseToStarRequest :: StarResponse -> StarRequest
starResponseToStarRequest  (StarResponse o) =
  StarRequest {
    reason: o.reason,
    guard: o.guard
  }


teamRequestToTeamResponse :: Int -> Int -> Int -> SystemTeam -> Boolean -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> TeamRequest -> TeamResponse
teamRequestToTeamResponse id userId orgId system active createdAt modifiedBy modifiedAt activityAt (TeamRequest o) =
  TeamResponse {
    id: id,
    userId: userId,
    orgId: orgId,
    system: system,
    membership: o.membership,
    icon: o.icon,
    tags: o.tags,
    visibility: o.visibility,
    active: active,
    guard: o.guard,
    createdAt: createdAt,
    modifiedBy: modifiedBy,
    modifiedAt: modifiedAt,
    activityAt: activityAt
  }


teamResponseToTeamRequest :: TeamResponse -> TeamRequest
teamResponseToTeamRequest  (TeamResponse o) =
  TeamRequest {
    membership: o.membership,
    icon: o.icon,
    tags: o.tags,
    visibility: o.visibility,
    guard: o.guard
  }


threadRequestToThreadResponse :: Int -> Int -> Int -> Int -> Int -> String -> Boolean -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> ThreadRequest -> ThreadResponse
threadRequestToThreadResponse id userId orgId forumId boardId name active createdAt modifiedBy modifiedAt activityAt (ThreadRequest o) =
  ThreadResponse {
    id: id,
    userId: userId,
    orgId: orgId,
    forumId: forumId,
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


threadResponseToThreadRequest :: ThreadResponse -> ThreadRequest
threadResponseToThreadRequest  (ThreadResponse o) =
  ThreadRequest {
    displayName: o.displayName,
    description: o.description,
    sticky: o.sticky,
    locked: o.locked,
    poll: o.poll,
    icon: o.icon,
    tags: o.tags,
    guard: o.guard
  }


threadPostRequestToThreadPostResponse :: Int -> Int -> Int -> Int -> Int -> Int -> (Maybe Int) -> Boolean -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> ThreadPostRequest -> ThreadPostResponse
threadPostRequestToThreadPostResponse id userId orgId forumId boardId threadId parentId active createdAt modifiedBy modifiedAt activityAt (ThreadPostRequest o) =
  ThreadPostResponse {
    id: id,
    userId: userId,
    orgId: orgId,
    forumId: forumId,
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


threadPostResponseToThreadPostRequest :: ThreadPostResponse -> ThreadPostRequest
threadPostResponseToThreadPostRequest  (ThreadPostResponse o) =
  ThreadPostRequest {
    title: o.title,
    body: o.body,
    tags: o.tags,
    privateTags: o.privateTags,
    guard: o.guard
  }


userRequestToUserResponse :: Int -> String -> String -> Boolean -> Int -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> UserRequest -> UserResponse
userRequestToUserResponse id name emailMD5 active guard createdAt modifiedAt deactivatedAt activityAt (UserRequest o) =
  UserResponse {
    id: id,
    name: name,
    displayName: o.displayName,
    fullName: o.fullName,
    email: o.email,
    emailMD5: emailMD5,
    plugin: o.plugin,
    ident: o.ident,
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
    ident: o.ident,
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


userSanitizedResponseToUserRequest :: String -> String -> String -> String -> (Maybe Date) -> UserSanitizedResponse -> UserRequest
userSanitizedResponseToUserRequest fullName email plugin ident acceptTOS (UserSanitizedResponse o) =
  UserRequest {
    displayName: o.displayName,
    fullName: fullName,
    email: email,
    plugin: plugin,
    ident: ident,
    acceptTOS: acceptTOS
  }



-- footer