module LN.T.Param where



import Data.Argonaut.Core               (jsonEmptyObject, stringify)
import Data.Argonaut.Decode             (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode             (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Date.Helpers                (Date)
import Data.Either                      (Either(..))
import Data.Foreign                     (ForeignError(..), fail, unsafeFromForeign)
import Data.Foreign.NullOrUndefined     (unNullOrUndefined)
import Data.Foreign.Class               (class Decode, decode)
import Data.Foreign.Helpers             (readPropUnsafe)
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

data Param
  = Limit Int
  | Offset Int
  | SortOrder SortOrderBy
  | Order OrderBy
  | ByOrganizationId Int
  | ByOrganizationsIds (Array Int)
  | ByOrganizationName String
  | ByTeamId Int
  | ByTeamsIds (Array Int)
  | ByTeamName String
  | ByTeamMemberId Int
  | ByTeamMembersIds (Array Int)
  | ByUserId Int
  | ByUsersIds (Array Int)
  | ByUserName String
  | ByUsersNames (Array String)
  | ByGlobalGroupId Int
  | ByGlobalGroupsIds (Array Int)
  | ByGroupId Int
  | ByGroupsIds (Array Int)
  | ByGroupMemberId Int
  | ByGroupMembersIds (Array Int)
  | ByForumId Int
  | ByForumsIds (Array Int)
  | ByForumName String
  | ByBoardId Int
  | ByBoardsIds (Array Int)
  | ByBoardName String
  | ByThreadId Int
  | ByThreadsIds (Array Int)
  | ByThreadName String
  | ByThreadPostId Int
  | ByThreadPostsIds (Array Int)
  | ByThreadPostName String
  | ByThreadPostLikeId Int
  | ByThreadPostLikesIds (Array Int)
  | ByThreadPostStarId Int
  | ByThreadPostStarsIds (Array Int)
  | ByBucketId Int
  | ByBucketRoundId Int
  | ByResourceId Int
  | ByResourcesIds (Array Int)
  | ByResourceName String
  | ByLeuronId Int
  | ByLeuronsIds (Array Int)
  | ByPmId Int
  | ByPmsIds (Array Int)
  | ByReminderId Int
  | ByReminderFolderId Int
  | ByParentId Int
  | ByParentsIds (Array Int)
  | ByParentName String
  | ByEmail String
  | BySelf Boolean
  | View Boolean
  | Timestamp Date
  | UnixTimestamp Int
  | CreatedAtTimestamp Date
  | CreatedAtUnixTimestamp Int
  | RealIP String
  | IP String
  | WithOrganization Boolean
  | WithForum Boolean
  | WithBoard Boolean
  | WithThread Boolean
  | WithThreadPosts Boolean
  | WithResource Boolean



instance paramEncodeJson :: EncodeJson Param where
  encodeJson (Limit x0) =
       "tag" := "Limit"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (Offset x0) =
       "tag" := "Offset"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (SortOrder x0) =
       "tag" := "SortOrder"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (Order x0) =
       "tag" := "Order"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByOrganizationId x0) =
       "tag" := "ByOrganizationId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByOrganizationsIds x0) =
       "tag" := "ByOrganizationsIds"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByOrganizationName x0) =
       "tag" := "ByOrganizationName"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByTeamId x0) =
       "tag" := "ByTeamId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByTeamsIds x0) =
       "tag" := "ByTeamsIds"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByTeamName x0) =
       "tag" := "ByTeamName"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByTeamMemberId x0) =
       "tag" := "ByTeamMemberId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByTeamMembersIds x0) =
       "tag" := "ByTeamMembersIds"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByUserId x0) =
       "tag" := "ByUserId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByUsersIds x0) =
       "tag" := "ByUsersIds"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByUserName x0) =
       "tag" := "ByUserName"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByUsersNames x0) =
       "tag" := "ByUsersNames"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByGlobalGroupId x0) =
       "tag" := "ByGlobalGroupId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByGlobalGroupsIds x0) =
       "tag" := "ByGlobalGroupsIds"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByGroupId x0) =
       "tag" := "ByGroupId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByGroupsIds x0) =
       "tag" := "ByGroupsIds"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByGroupMemberId x0) =
       "tag" := "ByGroupMemberId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByGroupMembersIds x0) =
       "tag" := "ByGroupMembersIds"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByForumId x0) =
       "tag" := "ByForumId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByForumsIds x0) =
       "tag" := "ByForumsIds"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByForumName x0) =
       "tag" := "ByForumName"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByBoardId x0) =
       "tag" := "ByBoardId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByBoardsIds x0) =
       "tag" := "ByBoardsIds"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByBoardName x0) =
       "tag" := "ByBoardName"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByThreadId x0) =
       "tag" := "ByThreadId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByThreadsIds x0) =
       "tag" := "ByThreadsIds"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByThreadName x0) =
       "tag" := "ByThreadName"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByThreadPostId x0) =
       "tag" := "ByThreadPostId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByThreadPostsIds x0) =
       "tag" := "ByThreadPostsIds"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByThreadPostName x0) =
       "tag" := "ByThreadPostName"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByThreadPostLikeId x0) =
       "tag" := "ByThreadPostLikeId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByThreadPostLikesIds x0) =
       "tag" := "ByThreadPostLikesIds"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByThreadPostStarId x0) =
       "tag" := "ByThreadPostStarId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByThreadPostStarsIds x0) =
       "tag" := "ByThreadPostStarsIds"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByBucketId x0) =
       "tag" := "ByBucketId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByBucketRoundId x0) =
       "tag" := "ByBucketRoundId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByResourceId x0) =
       "tag" := "ByResourceId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByResourcesIds x0) =
       "tag" := "ByResourcesIds"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByResourceName x0) =
       "tag" := "ByResourceName"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByLeuronId x0) =
       "tag" := "ByLeuronId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByLeuronsIds x0) =
       "tag" := "ByLeuronsIds"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByPmId x0) =
       "tag" := "ByPmId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByPmsIds x0) =
       "tag" := "ByPmsIds"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByReminderId x0) =
       "tag" := "ByReminderId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByReminderFolderId x0) =
       "tag" := "ByReminderFolderId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByParentId x0) =
       "tag" := "ByParentId"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByParentsIds x0) =
       "tag" := "ByParentsIds"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByParentName x0) =
       "tag" := "ByParentName"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ByEmail x0) =
       "tag" := "ByEmail"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (BySelf x0) =
       "tag" := "BySelf"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (View x0) =
       "tag" := "View"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (Timestamp x0) =
       "tag" := "Timestamp"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (UnixTimestamp x0) =
       "tag" := "UnixTimestamp"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (CreatedAtTimestamp x0) =
       "tag" := "CreatedAtTimestamp"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (CreatedAtUnixTimestamp x0) =
       "tag" := "CreatedAtUnixTimestamp"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (RealIP x0) =
       "tag" := "RealIP"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (IP x0) =
       "tag" := "IP"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (WithOrganization x0) =
       "tag" := "WithOrganization"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (WithForum x0) =
       "tag" := "WithForum"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (WithBoard x0) =
       "tag" := "WithBoard"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (WithThread x0) =
       "tag" := "WithThread"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (WithThreadPosts x0) =
       "tag" := "WithThreadPosts"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (WithResource x0) =
       "tag" := "WithResource"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject


instance paramDecodeJson :: DecodeJson Param where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Limit" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> Limit <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for Limit"


      "Offset" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> Offset <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for Offset"


      "SortOrder" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> SortOrder <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for SortOrder"


      "Order" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> Order <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for Order"


      "ByOrganizationId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByOrganizationId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByOrganizationId"


      "ByOrganizationsIds" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByOrganizationsIds <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByOrganizationsIds"


      "ByOrganizationName" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByOrganizationName <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByOrganizationName"


      "ByTeamId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByTeamId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByTeamId"


      "ByTeamsIds" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByTeamsIds <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByTeamsIds"


      "ByTeamName" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByTeamName <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByTeamName"


      "ByTeamMemberId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByTeamMemberId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByTeamMemberId"


      "ByTeamMembersIds" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByTeamMembersIds <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByTeamMembersIds"


      "ByUserId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByUserId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByUserId"


      "ByUsersIds" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByUsersIds <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByUsersIds"


      "ByUserName" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByUserName <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByUserName"


      "ByUsersNames" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByUsersNames <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByUsersNames"


      "ByGlobalGroupId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByGlobalGroupId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByGlobalGroupId"


      "ByGlobalGroupsIds" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByGlobalGroupsIds <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByGlobalGroupsIds"


      "ByGroupId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByGroupId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByGroupId"


      "ByGroupsIds" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByGroupsIds <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByGroupsIds"


      "ByGroupMemberId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByGroupMemberId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByGroupMemberId"


      "ByGroupMembersIds" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByGroupMembersIds <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByGroupMembersIds"


      "ByForumId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByForumId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByForumId"


      "ByForumsIds" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByForumsIds <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByForumsIds"


      "ByForumName" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByForumName <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByForumName"


      "ByBoardId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByBoardId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByBoardId"


      "ByBoardsIds" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByBoardsIds <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByBoardsIds"


      "ByBoardName" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByBoardName <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByBoardName"


      "ByThreadId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByThreadId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByThreadId"


      "ByThreadsIds" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByThreadsIds <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByThreadsIds"


      "ByThreadName" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByThreadName <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByThreadName"


      "ByThreadPostId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByThreadPostId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByThreadPostId"


      "ByThreadPostsIds" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByThreadPostsIds <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByThreadPostsIds"


      "ByThreadPostName" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByThreadPostName <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByThreadPostName"


      "ByThreadPostLikeId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByThreadPostLikeId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByThreadPostLikeId"


      "ByThreadPostLikesIds" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByThreadPostLikesIds <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByThreadPostLikesIds"


      "ByThreadPostStarId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByThreadPostStarId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByThreadPostStarId"


      "ByThreadPostStarsIds" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByThreadPostStarsIds <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByThreadPostStarsIds"


      "ByBucketId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByBucketId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByBucketId"


      "ByBucketRoundId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByBucketRoundId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByBucketRoundId"


      "ByResourceId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByResourceId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByResourceId"


      "ByResourcesIds" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByResourcesIds <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByResourcesIds"


      "ByResourceName" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByResourceName <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByResourceName"


      "ByLeuronId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByLeuronId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByLeuronId"


      "ByLeuronsIds" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByLeuronsIds <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByLeuronsIds"


      "ByPmId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByPmId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByPmId"


      "ByPmsIds" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByPmsIds <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByPmsIds"


      "ByReminderId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByReminderId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByReminderId"


      "ByReminderFolderId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByReminderFolderId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByReminderFolderId"


      "ByParentId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByParentId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByParentId"


      "ByParentsIds" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByParentsIds <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByParentsIds"


      "ByParentName" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByParentName <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByParentName"


      "ByEmail" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByEmail <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByEmail"


      "BySelf" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> BySelf <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for BySelf"


      "View" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> View <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for View"


      "Timestamp" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> Timestamp <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for Timestamp"


      "UnixTimestamp" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> UnixTimestamp <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for UnixTimestamp"


      "CreatedAtTimestamp" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> CreatedAtTimestamp <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for CreatedAtTimestamp"


      "CreatedAtUnixTimestamp" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> CreatedAtUnixTimestamp <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for CreatedAtUnixTimestamp"


      "RealIP" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> RealIP <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for RealIP"


      "IP" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> IP <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for IP"


      "WithOrganization" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> WithOrganization <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for WithOrganization"


      "WithForum" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> WithForum <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for WithForum"


      "WithBoard" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> WithBoard <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for WithBoard"


      "WithThread" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> WithThread <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for WithThread"


      "WithThreadPosts" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> WithThreadPosts <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for WithThreadPosts"


      "WithResource" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> WithResource <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for WithResource"


      _ -> Left $ "DecodeJson TypeMismatch for Param"



instance paramRequestable :: Requestable Param where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance paramRespondable :: Respondable Param where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "Limit" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> Limit <$> decode x0
          _ -> fail $ TypeMismatch "Limit" "Respondable"


      "Offset" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> Offset <$> decode x0
          _ -> fail $ TypeMismatch "Offset" "Respondable"


      "SortOrder" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> SortOrder <$> decode x0
          _ -> fail $ TypeMismatch "SortOrder" "Respondable"


      "Order" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> Order <$> decode x0
          _ -> fail $ TypeMismatch "Order" "Respondable"


      "ByOrganizationId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByOrganizationId <$> decode x0
          _ -> fail $ TypeMismatch "ByOrganizationId" "Respondable"


      "ByOrganizationsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByOrganizationsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByOrganizationsIds" "Respondable"


      "ByOrganizationName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByOrganizationName <$> decode x0
          _ -> fail $ TypeMismatch "ByOrganizationName" "Respondable"


      "ByTeamId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByTeamId <$> decode x0
          _ -> fail $ TypeMismatch "ByTeamId" "Respondable"


      "ByTeamsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByTeamsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByTeamsIds" "Respondable"


      "ByTeamName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByTeamName <$> decode x0
          _ -> fail $ TypeMismatch "ByTeamName" "Respondable"


      "ByTeamMemberId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByTeamMemberId <$> decode x0
          _ -> fail $ TypeMismatch "ByTeamMemberId" "Respondable"


      "ByTeamMembersIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByTeamMembersIds <$> decode x0
          _ -> fail $ TypeMismatch "ByTeamMembersIds" "Respondable"


      "ByUserId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByUserId <$> decode x0
          _ -> fail $ TypeMismatch "ByUserId" "Respondable"


      "ByUsersIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByUsersIds <$> decode x0
          _ -> fail $ TypeMismatch "ByUsersIds" "Respondable"


      "ByUserName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByUserName <$> decode x0
          _ -> fail $ TypeMismatch "ByUserName" "Respondable"


      "ByUsersNames" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByUsersNames <$> decode x0
          _ -> fail $ TypeMismatch "ByUsersNames" "Respondable"


      "ByGlobalGroupId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByGlobalGroupId <$> decode x0
          _ -> fail $ TypeMismatch "ByGlobalGroupId" "Respondable"


      "ByGlobalGroupsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByGlobalGroupsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByGlobalGroupsIds" "Respondable"


      "ByGroupId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByGroupId <$> decode x0
          _ -> fail $ TypeMismatch "ByGroupId" "Respondable"


      "ByGroupsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByGroupsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByGroupsIds" "Respondable"


      "ByGroupMemberId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByGroupMemberId <$> decode x0
          _ -> fail $ TypeMismatch "ByGroupMemberId" "Respondable"


      "ByGroupMembersIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByGroupMembersIds <$> decode x0
          _ -> fail $ TypeMismatch "ByGroupMembersIds" "Respondable"


      "ByForumId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByForumId <$> decode x0
          _ -> fail $ TypeMismatch "ByForumId" "Respondable"


      "ByForumsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByForumsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByForumsIds" "Respondable"


      "ByForumName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByForumName <$> decode x0
          _ -> fail $ TypeMismatch "ByForumName" "Respondable"


      "ByBoardId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByBoardId <$> decode x0
          _ -> fail $ TypeMismatch "ByBoardId" "Respondable"


      "ByBoardsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByBoardsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByBoardsIds" "Respondable"


      "ByBoardName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByBoardName <$> decode x0
          _ -> fail $ TypeMismatch "ByBoardName" "Respondable"


      "ByThreadId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadId <$> decode x0
          _ -> fail $ TypeMismatch "ByThreadId" "Respondable"


      "ByThreadsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByThreadsIds" "Respondable"


      "ByThreadName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadName <$> decode x0
          _ -> fail $ TypeMismatch "ByThreadName" "Respondable"


      "ByThreadPostId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadPostId <$> decode x0
          _ -> fail $ TypeMismatch "ByThreadPostId" "Respondable"


      "ByThreadPostsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadPostsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByThreadPostsIds" "Respondable"


      "ByThreadPostName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadPostName <$> decode x0
          _ -> fail $ TypeMismatch "ByThreadPostName" "Respondable"


      "ByThreadPostLikeId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadPostLikeId <$> decode x0
          _ -> fail $ TypeMismatch "ByThreadPostLikeId" "Respondable"


      "ByThreadPostLikesIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadPostLikesIds <$> decode x0
          _ -> fail $ TypeMismatch "ByThreadPostLikesIds" "Respondable"


      "ByThreadPostStarId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadPostStarId <$> decode x0
          _ -> fail $ TypeMismatch "ByThreadPostStarId" "Respondable"


      "ByThreadPostStarsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadPostStarsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByThreadPostStarsIds" "Respondable"


      "ByBucketId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByBucketId <$> decode x0
          _ -> fail $ TypeMismatch "ByBucketId" "Respondable"


      "ByBucketRoundId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByBucketRoundId <$> decode x0
          _ -> fail $ TypeMismatch "ByBucketRoundId" "Respondable"


      "ByResourceId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByResourceId <$> decode x0
          _ -> fail $ TypeMismatch "ByResourceId" "Respondable"


      "ByResourcesIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByResourcesIds <$> decode x0
          _ -> fail $ TypeMismatch "ByResourcesIds" "Respondable"


      "ByResourceName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByResourceName <$> decode x0
          _ -> fail $ TypeMismatch "ByResourceName" "Respondable"


      "ByLeuronId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByLeuronId <$> decode x0
          _ -> fail $ TypeMismatch "ByLeuronId" "Respondable"


      "ByLeuronsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByLeuronsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByLeuronsIds" "Respondable"


      "ByPmId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByPmId <$> decode x0
          _ -> fail $ TypeMismatch "ByPmId" "Respondable"


      "ByPmsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByPmsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByPmsIds" "Respondable"


      "ByReminderId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByReminderId <$> decode x0
          _ -> fail $ TypeMismatch "ByReminderId" "Respondable"


      "ByReminderFolderId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByReminderFolderId <$> decode x0
          _ -> fail $ TypeMismatch "ByReminderFolderId" "Respondable"


      "ByParentId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByParentId <$> decode x0
          _ -> fail $ TypeMismatch "ByParentId" "Respondable"


      "ByParentsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByParentsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByParentsIds" "Respondable"


      "ByParentName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByParentName <$> decode x0
          _ -> fail $ TypeMismatch "ByParentName" "Respondable"


      "ByEmail" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByEmail <$> decode x0
          _ -> fail $ TypeMismatch "ByEmail" "Respondable"


      "BySelf" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> BySelf <$> decode x0
          _ -> fail $ TypeMismatch "BySelf" "Respondable"


      "View" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> View <$> decode x0
          _ -> fail $ TypeMismatch "View" "Respondable"


      "Timestamp" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> Timestamp <$> decode x0
          _ -> fail $ TypeMismatch "Timestamp" "Respondable"


      "UnixTimestamp" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> UnixTimestamp <$> decode x0
          _ -> fail $ TypeMismatch "UnixTimestamp" "Respondable"


      "CreatedAtTimestamp" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> CreatedAtTimestamp <$> decode x0
          _ -> fail $ TypeMismatch "CreatedAtTimestamp" "Respondable"


      "CreatedAtUnixTimestamp" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> CreatedAtUnixTimestamp <$> decode x0
          _ -> fail $ TypeMismatch "CreatedAtUnixTimestamp" "Respondable"


      "RealIP" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> RealIP <$> decode x0
          _ -> fail $ TypeMismatch "RealIP" "Respondable"


      "IP" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> IP <$> decode x0
          _ -> fail $ TypeMismatch "IP" "Respondable"


      "WithOrganization" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> WithOrganization <$> decode x0
          _ -> fail $ TypeMismatch "WithOrganization" "Respondable"


      "WithForum" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> WithForum <$> decode x0
          _ -> fail $ TypeMismatch "WithForum" "Respondable"


      "WithBoard" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> WithBoard <$> decode x0
          _ -> fail $ TypeMismatch "WithBoard" "Respondable"


      "WithThread" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> WithThread <$> decode x0
          _ -> fail $ TypeMismatch "WithThread" "Respondable"


      "WithThreadPosts" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> WithThreadPosts <$> decode x0
          _ -> fail $ TypeMismatch "WithThreadPosts" "Respondable"


      "WithResource" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> WithResource <$> decode x0
          _ -> fail $ TypeMismatch "WithResource" "Respondable"


      _ -> fail $ TypeMismatch "Param" "Respondable"



instance paramDecode :: Decode Param where
  decode json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "Limit" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> Limit <$> decode x0
          _ -> fail $ TypeMismatch "Limit" "Decode"


      "Offset" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> Offset <$> decode x0
          _ -> fail $ TypeMismatch "Offset" "Decode"


      "SortOrder" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> SortOrder <$> decode x0
          _ -> fail $ TypeMismatch "SortOrder" "Decode"


      "Order" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> Order <$> decode x0
          _ -> fail $ TypeMismatch "Order" "Decode"


      "ByOrganizationId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByOrganizationId <$> decode x0
          _ -> fail $ TypeMismatch "ByOrganizationId" "Decode"


      "ByOrganizationsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByOrganizationsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByOrganizationsIds" "Decode"


      "ByOrganizationName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByOrganizationName <$> decode x0
          _ -> fail $ TypeMismatch "ByOrganizationName" "Decode"


      "ByTeamId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByTeamId <$> decode x0
          _ -> fail $ TypeMismatch "ByTeamId" "Decode"


      "ByTeamsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByTeamsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByTeamsIds" "Decode"


      "ByTeamName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByTeamName <$> decode x0
          _ -> fail $ TypeMismatch "ByTeamName" "Decode"


      "ByTeamMemberId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByTeamMemberId <$> decode x0
          _ -> fail $ TypeMismatch "ByTeamMemberId" "Decode"


      "ByTeamMembersIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByTeamMembersIds <$> decode x0
          _ -> fail $ TypeMismatch "ByTeamMembersIds" "Decode"


      "ByUserId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByUserId <$> decode x0
          _ -> fail $ TypeMismatch "ByUserId" "Decode"


      "ByUsersIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByUsersIds <$> decode x0
          _ -> fail $ TypeMismatch "ByUsersIds" "Decode"


      "ByUserName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByUserName <$> decode x0
          _ -> fail $ TypeMismatch "ByUserName" "Decode"


      "ByUsersNames" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByUsersNames <$> decode x0
          _ -> fail $ TypeMismatch "ByUsersNames" "Decode"


      "ByGlobalGroupId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByGlobalGroupId <$> decode x0
          _ -> fail $ TypeMismatch "ByGlobalGroupId" "Decode"


      "ByGlobalGroupsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByGlobalGroupsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByGlobalGroupsIds" "Decode"


      "ByGroupId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByGroupId <$> decode x0
          _ -> fail $ TypeMismatch "ByGroupId" "Decode"


      "ByGroupsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByGroupsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByGroupsIds" "Decode"


      "ByGroupMemberId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByGroupMemberId <$> decode x0
          _ -> fail $ TypeMismatch "ByGroupMemberId" "Decode"


      "ByGroupMembersIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByGroupMembersIds <$> decode x0
          _ -> fail $ TypeMismatch "ByGroupMembersIds" "Decode"


      "ByForumId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByForumId <$> decode x0
          _ -> fail $ TypeMismatch "ByForumId" "Decode"


      "ByForumsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByForumsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByForumsIds" "Decode"


      "ByForumName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByForumName <$> decode x0
          _ -> fail $ TypeMismatch "ByForumName" "Decode"


      "ByBoardId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByBoardId <$> decode x0
          _ -> fail $ TypeMismatch "ByBoardId" "Decode"


      "ByBoardsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByBoardsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByBoardsIds" "Decode"


      "ByBoardName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByBoardName <$> decode x0
          _ -> fail $ TypeMismatch "ByBoardName" "Decode"


      "ByThreadId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadId <$> decode x0
          _ -> fail $ TypeMismatch "ByThreadId" "Decode"


      "ByThreadsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByThreadsIds" "Decode"


      "ByThreadName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadName <$> decode x0
          _ -> fail $ TypeMismatch "ByThreadName" "Decode"


      "ByThreadPostId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadPostId <$> decode x0
          _ -> fail $ TypeMismatch "ByThreadPostId" "Decode"


      "ByThreadPostsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadPostsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByThreadPostsIds" "Decode"


      "ByThreadPostName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadPostName <$> decode x0
          _ -> fail $ TypeMismatch "ByThreadPostName" "Decode"


      "ByThreadPostLikeId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadPostLikeId <$> decode x0
          _ -> fail $ TypeMismatch "ByThreadPostLikeId" "Decode"


      "ByThreadPostLikesIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadPostLikesIds <$> decode x0
          _ -> fail $ TypeMismatch "ByThreadPostLikesIds" "Decode"


      "ByThreadPostStarId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadPostStarId <$> decode x0
          _ -> fail $ TypeMismatch "ByThreadPostStarId" "Decode"


      "ByThreadPostStarsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadPostStarsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByThreadPostStarsIds" "Decode"


      "ByBucketId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByBucketId <$> decode x0
          _ -> fail $ TypeMismatch "ByBucketId" "Decode"


      "ByBucketRoundId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByBucketRoundId <$> decode x0
          _ -> fail $ TypeMismatch "ByBucketRoundId" "Decode"


      "ByResourceId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByResourceId <$> decode x0
          _ -> fail $ TypeMismatch "ByResourceId" "Decode"


      "ByResourcesIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByResourcesIds <$> decode x0
          _ -> fail $ TypeMismatch "ByResourcesIds" "Decode"


      "ByResourceName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByResourceName <$> decode x0
          _ -> fail $ TypeMismatch "ByResourceName" "Decode"


      "ByLeuronId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByLeuronId <$> decode x0
          _ -> fail $ TypeMismatch "ByLeuronId" "Decode"


      "ByLeuronsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByLeuronsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByLeuronsIds" "Decode"


      "ByPmId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByPmId <$> decode x0
          _ -> fail $ TypeMismatch "ByPmId" "Decode"


      "ByPmsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByPmsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByPmsIds" "Decode"


      "ByReminderId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByReminderId <$> decode x0
          _ -> fail $ TypeMismatch "ByReminderId" "Decode"


      "ByReminderFolderId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByReminderFolderId <$> decode x0
          _ -> fail $ TypeMismatch "ByReminderFolderId" "Decode"


      "ByParentId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByParentId <$> decode x0
          _ -> fail $ TypeMismatch "ByParentId" "Decode"


      "ByParentsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByParentsIds <$> decode x0
          _ -> fail $ TypeMismatch "ByParentsIds" "Decode"


      "ByParentName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByParentName <$> decode x0
          _ -> fail $ TypeMismatch "ByParentName" "Decode"


      "ByEmail" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByEmail <$> decode x0
          _ -> fail $ TypeMismatch "ByEmail" "Decode"


      "BySelf" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> BySelf <$> decode x0
          _ -> fail $ TypeMismatch "BySelf" "Decode"


      "View" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> View <$> decode x0
          _ -> fail $ TypeMismatch "View" "Decode"


      "Timestamp" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> Timestamp <$> decode x0
          _ -> fail $ TypeMismatch "Timestamp" "Decode"


      "UnixTimestamp" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> UnixTimestamp <$> decode x0
          _ -> fail $ TypeMismatch "UnixTimestamp" "Decode"


      "CreatedAtTimestamp" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> CreatedAtTimestamp <$> decode x0
          _ -> fail $ TypeMismatch "CreatedAtTimestamp" "Decode"


      "CreatedAtUnixTimestamp" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> CreatedAtUnixTimestamp <$> decode x0
          _ -> fail $ TypeMismatch "CreatedAtUnixTimestamp" "Decode"


      "RealIP" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> RealIP <$> decode x0
          _ -> fail $ TypeMismatch "RealIP" "Decode"


      "IP" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> IP <$> decode x0
          _ -> fail $ TypeMismatch "IP" "Decode"


      "WithOrganization" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> WithOrganization <$> decode x0
          _ -> fail $ TypeMismatch "WithOrganization" "Decode"


      "WithForum" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> WithForum <$> decode x0
          _ -> fail $ TypeMismatch "WithForum" "Decode"


      "WithBoard" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> WithBoard <$> decode x0
          _ -> fail $ TypeMismatch "WithBoard" "Decode"


      "WithThread" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> WithThread <$> decode x0
          _ -> fail $ TypeMismatch "WithThread" "Decode"


      "WithThreadPosts" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> WithThreadPosts <$> decode x0
          _ -> fail $ TypeMismatch "WithThreadPosts" "Decode"


      "WithResource" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> WithResource <$> decode x0
          _ -> fail $ TypeMismatch "WithResource" "Decode"


      _ -> fail $ TypeMismatch "Param" "Decode"



instance paramEq :: Eq Param where
  eq (Limit x0a) (Limit x0b) = x0a == x0b
  eq (Offset x0a) (Offset x0b) = x0a == x0b
  eq (SortOrder x0a) (SortOrder x0b) = x0a == x0b
  eq (Order x0a) (Order x0b) = x0a == x0b
  eq (ByOrganizationId x0a) (ByOrganizationId x0b) = x0a == x0b
  eq (ByOrganizationsIds x0a) (ByOrganizationsIds x0b) = x0a == x0b
  eq (ByOrganizationName x0a) (ByOrganizationName x0b) = x0a == x0b
  eq (ByTeamId x0a) (ByTeamId x0b) = x0a == x0b
  eq (ByTeamsIds x0a) (ByTeamsIds x0b) = x0a == x0b
  eq (ByTeamName x0a) (ByTeamName x0b) = x0a == x0b
  eq (ByTeamMemberId x0a) (ByTeamMemberId x0b) = x0a == x0b
  eq (ByTeamMembersIds x0a) (ByTeamMembersIds x0b) = x0a == x0b
  eq (ByUserId x0a) (ByUserId x0b) = x0a == x0b
  eq (ByUsersIds x0a) (ByUsersIds x0b) = x0a == x0b
  eq (ByUserName x0a) (ByUserName x0b) = x0a == x0b
  eq (ByUsersNames x0a) (ByUsersNames x0b) = x0a == x0b
  eq (ByGlobalGroupId x0a) (ByGlobalGroupId x0b) = x0a == x0b
  eq (ByGlobalGroupsIds x0a) (ByGlobalGroupsIds x0b) = x0a == x0b
  eq (ByGroupId x0a) (ByGroupId x0b) = x0a == x0b
  eq (ByGroupsIds x0a) (ByGroupsIds x0b) = x0a == x0b
  eq (ByGroupMemberId x0a) (ByGroupMemberId x0b) = x0a == x0b
  eq (ByGroupMembersIds x0a) (ByGroupMembersIds x0b) = x0a == x0b
  eq (ByForumId x0a) (ByForumId x0b) = x0a == x0b
  eq (ByForumsIds x0a) (ByForumsIds x0b) = x0a == x0b
  eq (ByForumName x0a) (ByForumName x0b) = x0a == x0b
  eq (ByBoardId x0a) (ByBoardId x0b) = x0a == x0b
  eq (ByBoardsIds x0a) (ByBoardsIds x0b) = x0a == x0b
  eq (ByBoardName x0a) (ByBoardName x0b) = x0a == x0b
  eq (ByThreadId x0a) (ByThreadId x0b) = x0a == x0b
  eq (ByThreadsIds x0a) (ByThreadsIds x0b) = x0a == x0b
  eq (ByThreadName x0a) (ByThreadName x0b) = x0a == x0b
  eq (ByThreadPostId x0a) (ByThreadPostId x0b) = x0a == x0b
  eq (ByThreadPostsIds x0a) (ByThreadPostsIds x0b) = x0a == x0b
  eq (ByThreadPostName x0a) (ByThreadPostName x0b) = x0a == x0b
  eq (ByThreadPostLikeId x0a) (ByThreadPostLikeId x0b) = x0a == x0b
  eq (ByThreadPostLikesIds x0a) (ByThreadPostLikesIds x0b) = x0a == x0b
  eq (ByThreadPostStarId x0a) (ByThreadPostStarId x0b) = x0a == x0b
  eq (ByThreadPostStarsIds x0a) (ByThreadPostStarsIds x0b) = x0a == x0b
  eq (ByBucketId x0a) (ByBucketId x0b) = x0a == x0b
  eq (ByBucketRoundId x0a) (ByBucketRoundId x0b) = x0a == x0b
  eq (ByResourceId x0a) (ByResourceId x0b) = x0a == x0b
  eq (ByResourcesIds x0a) (ByResourcesIds x0b) = x0a == x0b
  eq (ByResourceName x0a) (ByResourceName x0b) = x0a == x0b
  eq (ByLeuronId x0a) (ByLeuronId x0b) = x0a == x0b
  eq (ByLeuronsIds x0a) (ByLeuronsIds x0b) = x0a == x0b
  eq (ByPmId x0a) (ByPmId x0b) = x0a == x0b
  eq (ByPmsIds x0a) (ByPmsIds x0b) = x0a == x0b
  eq (ByReminderId x0a) (ByReminderId x0b) = x0a == x0b
  eq (ByReminderFolderId x0a) (ByReminderFolderId x0b) = x0a == x0b
  eq (ByParentId x0a) (ByParentId x0b) = x0a == x0b
  eq (ByParentsIds x0a) (ByParentsIds x0b) = x0a == x0b
  eq (ByParentName x0a) (ByParentName x0b) = x0a == x0b
  eq (ByEmail x0a) (ByEmail x0b) = x0a == x0b
  eq (BySelf x0a) (BySelf x0b) = x0a == x0b
  eq (View x0a) (View x0b) = x0a == x0b
  eq (Timestamp x0a) (Timestamp x0b) = x0a == x0b
  eq (UnixTimestamp x0a) (UnixTimestamp x0b) = x0a == x0b
  eq (CreatedAtTimestamp x0a) (CreatedAtTimestamp x0b) = x0a == x0b
  eq (CreatedAtUnixTimestamp x0a) (CreatedAtUnixTimestamp x0b) = x0a == x0b
  eq (RealIP x0a) (RealIP x0b) = x0a == x0b
  eq (IP x0a) (IP x0b) = x0a == x0b
  eq (WithOrganization x0a) (WithOrganization x0b) = x0a == x0b
  eq (WithForum x0a) (WithForum x0b) = x0a == x0b
  eq (WithBoard x0a) (WithBoard x0b) = x0a == x0b
  eq (WithThread x0a) (WithThread x0b) = x0a == x0b
  eq (WithThreadPosts x0a) (WithThreadPosts x0b) = x0a == x0b
  eq (WithResource x0a) (WithResource x0b) = x0a == x0b
  eq _ _ = false

instance paramShow :: Show Param where
  show (Limit x0) = "Limit: " <> show x0
  show (Offset x0) = "Offset: " <> show x0
  show (SortOrder x0) = "SortOrder: " <> show x0
  show (Order x0) = "Order: " <> show x0
  show (ByOrganizationId x0) = "ByOrganizationId: " <> show x0
  show (ByOrganizationsIds x0) = "ByOrganizationsIds: " <> show x0
  show (ByOrganizationName x0) = "ByOrganizationName: " <> show x0
  show (ByTeamId x0) = "ByTeamId: " <> show x0
  show (ByTeamsIds x0) = "ByTeamsIds: " <> show x0
  show (ByTeamName x0) = "ByTeamName: " <> show x0
  show (ByTeamMemberId x0) = "ByTeamMemberId: " <> show x0
  show (ByTeamMembersIds x0) = "ByTeamMembersIds: " <> show x0
  show (ByUserId x0) = "ByUserId: " <> show x0
  show (ByUsersIds x0) = "ByUsersIds: " <> show x0
  show (ByUserName x0) = "ByUserName: " <> show x0
  show (ByUsersNames x0) = "ByUsersNames: " <> show x0
  show (ByGlobalGroupId x0) = "ByGlobalGroupId: " <> show x0
  show (ByGlobalGroupsIds x0) = "ByGlobalGroupsIds: " <> show x0
  show (ByGroupId x0) = "ByGroupId: " <> show x0
  show (ByGroupsIds x0) = "ByGroupsIds: " <> show x0
  show (ByGroupMemberId x0) = "ByGroupMemberId: " <> show x0
  show (ByGroupMembersIds x0) = "ByGroupMembersIds: " <> show x0
  show (ByForumId x0) = "ByForumId: " <> show x0
  show (ByForumsIds x0) = "ByForumsIds: " <> show x0
  show (ByForumName x0) = "ByForumName: " <> show x0
  show (ByBoardId x0) = "ByBoardId: " <> show x0
  show (ByBoardsIds x0) = "ByBoardsIds: " <> show x0
  show (ByBoardName x0) = "ByBoardName: " <> show x0
  show (ByThreadId x0) = "ByThreadId: " <> show x0
  show (ByThreadsIds x0) = "ByThreadsIds: " <> show x0
  show (ByThreadName x0) = "ByThreadName: " <> show x0
  show (ByThreadPostId x0) = "ByThreadPostId: " <> show x0
  show (ByThreadPostsIds x0) = "ByThreadPostsIds: " <> show x0
  show (ByThreadPostName x0) = "ByThreadPostName: " <> show x0
  show (ByThreadPostLikeId x0) = "ByThreadPostLikeId: " <> show x0
  show (ByThreadPostLikesIds x0) = "ByThreadPostLikesIds: " <> show x0
  show (ByThreadPostStarId x0) = "ByThreadPostStarId: " <> show x0
  show (ByThreadPostStarsIds x0) = "ByThreadPostStarsIds: " <> show x0
  show (ByBucketId x0) = "ByBucketId: " <> show x0
  show (ByBucketRoundId x0) = "ByBucketRoundId: " <> show x0
  show (ByResourceId x0) = "ByResourceId: " <> show x0
  show (ByResourcesIds x0) = "ByResourcesIds: " <> show x0
  show (ByResourceName x0) = "ByResourceName: " <> show x0
  show (ByLeuronId x0) = "ByLeuronId: " <> show x0
  show (ByLeuronsIds x0) = "ByLeuronsIds: " <> show x0
  show (ByPmId x0) = "ByPmId: " <> show x0
  show (ByPmsIds x0) = "ByPmsIds: " <> show x0
  show (ByReminderId x0) = "ByReminderId: " <> show x0
  show (ByReminderFolderId x0) = "ByReminderFolderId: " <> show x0
  show (ByParentId x0) = "ByParentId: " <> show x0
  show (ByParentsIds x0) = "ByParentsIds: " <> show x0
  show (ByParentName x0) = "ByParentName: " <> show x0
  show (ByEmail x0) = "ByEmail: " <> show x0
  show (BySelf x0) = "BySelf: " <> show x0
  show (View x0) = "View: " <> show x0
  show (Timestamp x0) = "Timestamp: " <> show x0
  show (UnixTimestamp x0) = "UnixTimestamp: " <> show x0
  show (CreatedAtTimestamp x0) = "CreatedAtTimestamp: " <> show x0
  show (CreatedAtUnixTimestamp x0) = "CreatedAtUnixTimestamp: " <> show x0
  show (RealIP x0) = "RealIP: " <> show x0
  show (IP x0) = "IP: " <> show x0
  show (WithOrganization x0) = "WithOrganization: " <> show x0
  show (WithForum x0) = "WithForum: " <> show x0
  show (WithBoard x0) = "WithBoard: " <> show x0
  show (WithThread x0) = "WithThread: " <> show x0
  show (WithThreadPosts x0) = "WithThreadPosts: " <> show x0
  show (WithResource x0) = "WithResource: " <> show x0


instance paramQueryParam :: QueryParam Param where
  qp (Limit x0) = Tuple "limit" (show x0)
  qp (Offset x0) = Tuple "offset" (show x0)
  qp (SortOrder x0) = Tuple "sort_order" (show x0)
  qp (Order x0) = Tuple "order" (show x0)
  qp (ByOrganizationId x0) = Tuple "by_organization_id" (show x0)
  qp (ByOrganizationsIds x0) = Tuple "by_organizations_ids" (show x0)
  qp (ByOrganizationName x0) = Tuple "by_organization_name" x0
  qp (ByTeamId x0) = Tuple "by_team_id" (show x0)
  qp (ByTeamsIds x0) = Tuple "by_teams_ids" (show x0)
  qp (ByTeamName x0) = Tuple "by_team_name" x0
  qp (ByTeamMemberId x0) = Tuple "by_team_member_id" (show x0)
  qp (ByTeamMembersIds x0) = Tuple "by_team_members_ids" (show x0)
  qp (ByUserId x0) = Tuple "by_user_id" (show x0)
  qp (ByUsersIds x0) = Tuple "by_users_ids" (show x0)
  qp (ByUserName x0) = Tuple "by_user_name" x0
  qp (ByUsersNames x0) = Tuple "by_users_names" (show x0)
  qp (ByGlobalGroupId x0) = Tuple "by_global_group_id" (show x0)
  qp (ByGlobalGroupsIds x0) = Tuple "by_global_groups_ids" (show x0)
  qp (ByGroupId x0) = Tuple "by_group_id" (show x0)
  qp (ByGroupsIds x0) = Tuple "by_groups_ids" (show x0)
  qp (ByGroupMemberId x0) = Tuple "by_group_member_id" (show x0)
  qp (ByGroupMembersIds x0) = Tuple "by_group_members_ids" (show x0)
  qp (ByForumId x0) = Tuple "by_forum_id" (show x0)
  qp (ByForumsIds x0) = Tuple "by_forums_ids" (show x0)
  qp (ByForumName x0) = Tuple "by_forum_name" x0
  qp (ByBoardId x0) = Tuple "by_board_id" (show x0)
  qp (ByBoardsIds x0) = Tuple "by_boards_ids" (show x0)
  qp (ByBoardName x0) = Tuple "by_board_name" x0
  qp (ByThreadId x0) = Tuple "by_thread_id" (show x0)
  qp (ByThreadsIds x0) = Tuple "by_threads_ids" (show x0)
  qp (ByThreadName x0) = Tuple "by_thread_name" x0
  qp (ByThreadPostId x0) = Tuple "by_thread_post_id" (show x0)
  qp (ByThreadPostsIds x0) = Tuple "by_thread_posts_ids" (show x0)
  qp (ByThreadPostName x0) = Tuple "by_thread_post_name" x0
  qp (ByThreadPostLikeId x0) = Tuple "by_thread_post_like_id" (show x0)
  qp (ByThreadPostLikesIds x0) = Tuple "by_thread_post_likes_ids" (show x0)
  qp (ByThreadPostStarId x0) = Tuple "by_thread_post_star_id" (show x0)
  qp (ByThreadPostStarsIds x0) = Tuple "by_thread_post_stars_ids" (show x0)
  qp (ByBucketId x0) = Tuple "by_bucket_id" (show x0)
  qp (ByBucketRoundId x0) = Tuple "by_bucket_round_id" (show x0)
  qp (ByResourceId x0) = Tuple "by_resource_id" (show x0)
  qp (ByResourcesIds x0) = Tuple "by_resources_ids" (show x0)
  qp (ByResourceName x0) = Tuple "by_resource_name" x0
  qp (ByLeuronId x0) = Tuple "by_leuron_id" (show x0)
  qp (ByLeuronsIds x0) = Tuple "by_leurons_ids" (show x0)
  qp (ByPmId x0) = Tuple "by_pm_id" (show x0)
  qp (ByPmsIds x0) = Tuple "by_pms_ids" (show x0)
  qp (ByReminderId x0) = Tuple "by_reminder_id" (show x0)
  qp (ByReminderFolderId x0) = Tuple "by_reminder_folder_id" (show x0)
  qp (ByParentId x0) = Tuple "by_parent_id" (show x0)
  qp (ByParentsIds x0) = Tuple "by_parents_ids" (show x0)
  qp (ByParentName x0) = Tuple "by_parent_name" x0
  qp (ByEmail x0) = Tuple "by_email" x0
  qp (BySelf x0) = Tuple "by_self" (show x0)
  qp (View x0) = Tuple "view" (show x0)
  qp (Timestamp x0) = Tuple "timestamp" (show x0)
  qp (UnixTimestamp x0) = Tuple "unix_timestamp" (show x0)
  qp (CreatedAtTimestamp x0) = Tuple "created_at_timestamp" (show x0)
  qp (CreatedAtUnixTimestamp x0) = Tuple "created_at_unix_timestamp" (show x0)
  qp (RealIP x0) = Tuple "real_ip" x0
  qp (IP x0) = Tuple "ip" x0
  qp (WithOrganization x0) = Tuple "with_organization" (show x0)
  qp (WithForum x0) = Tuple "with_forum" (show x0)
  qp (WithBoard x0) = Tuple "with_board" (show x0)
  qp (WithThread x0) = Tuple "with_thread" (show x0)
  qp (WithThreadPosts x0) = Tuple "with_thread_posts" (show x0)
  qp (WithResource x0) = Tuple "with_resource" (show x0)


data ParamTag
  = ParamTag_Limit 
  | ParamTag_Offset 
  | ParamTag_SortOrder 
  | ParamTag_Order 
  | ParamTag_ByOrganizationId 
  | ParamTag_ByOrganizationsIds 
  | ParamTag_ByOrganizationName 
  | ParamTag_ByTeamId 
  | ParamTag_ByTeamsIds 
  | ParamTag_ByTeamName 
  | ParamTag_ByTeamMemberId 
  | ParamTag_ByTeamMembersIds 
  | ParamTag_ByUserId 
  | ParamTag_ByUsersIds 
  | ParamTag_ByUserName 
  | ParamTag_ByUsersNames 
  | ParamTag_ByGlobalGroupId 
  | ParamTag_ByGlobalGroupsIds 
  | ParamTag_ByGroupId 
  | ParamTag_ByGroupsIds 
  | ParamTag_ByGroupMemberId 
  | ParamTag_ByGroupMembersIds 
  | ParamTag_ByForumId 
  | ParamTag_ByForumsIds 
  | ParamTag_ByForumName 
  | ParamTag_ByBoardId 
  | ParamTag_ByBoardsIds 
  | ParamTag_ByBoardName 
  | ParamTag_ByThreadId 
  | ParamTag_ByThreadsIds 
  | ParamTag_ByThreadName 
  | ParamTag_ByThreadPostId 
  | ParamTag_ByThreadPostsIds 
  | ParamTag_ByThreadPostName 
  | ParamTag_ByThreadPostLikeId 
  | ParamTag_ByThreadPostLikesIds 
  | ParamTag_ByThreadPostStarId 
  | ParamTag_ByThreadPostStarsIds 
  | ParamTag_ByBucketId 
  | ParamTag_ByBucketRoundId 
  | ParamTag_ByResourceId 
  | ParamTag_ByResourcesIds 
  | ParamTag_ByResourceName 
  | ParamTag_ByLeuronId 
  | ParamTag_ByLeuronsIds 
  | ParamTag_ByPmId 
  | ParamTag_ByPmsIds 
  | ParamTag_ByReminderId 
  | ParamTag_ByReminderFolderId 
  | ParamTag_ByParentId 
  | ParamTag_ByParentsIds 
  | ParamTag_ByParentName 
  | ParamTag_ByEmail 
  | ParamTag_BySelf 
  | ParamTag_View 
  | ParamTag_Timestamp 
  | ParamTag_UnixTimestamp 
  | ParamTag_CreatedAtTimestamp 
  | ParamTag_CreatedAtUnixTimestamp 
  | ParamTag_RealIP 
  | ParamTag_IP 
  | ParamTag_WithOrganization 
  | ParamTag_WithForum 
  | ParamTag_WithBoard 
  | ParamTag_WithThread 
  | ParamTag_WithThreadPosts 
  | ParamTag_WithResource 



instance paramTagEncodeJson :: EncodeJson ParamTag where
  encodeJson (ParamTag_Limit ) =
       "tag" := "ParamTag_Limit"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_Offset ) =
       "tag" := "ParamTag_Offset"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_SortOrder ) =
       "tag" := "ParamTag_SortOrder"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_Order ) =
       "tag" := "ParamTag_Order"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByOrganizationId ) =
       "tag" := "ParamTag_ByOrganizationId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByOrganizationsIds ) =
       "tag" := "ParamTag_ByOrganizationsIds"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByOrganizationName ) =
       "tag" := "ParamTag_ByOrganizationName"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByTeamId ) =
       "tag" := "ParamTag_ByTeamId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByTeamsIds ) =
       "tag" := "ParamTag_ByTeamsIds"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByTeamName ) =
       "tag" := "ParamTag_ByTeamName"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByTeamMemberId ) =
       "tag" := "ParamTag_ByTeamMemberId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByTeamMembersIds ) =
       "tag" := "ParamTag_ByTeamMembersIds"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByUserId ) =
       "tag" := "ParamTag_ByUserId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByUsersIds ) =
       "tag" := "ParamTag_ByUsersIds"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByUserName ) =
       "tag" := "ParamTag_ByUserName"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByUsersNames ) =
       "tag" := "ParamTag_ByUsersNames"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByGlobalGroupId ) =
       "tag" := "ParamTag_ByGlobalGroupId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByGlobalGroupsIds ) =
       "tag" := "ParamTag_ByGlobalGroupsIds"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByGroupId ) =
       "tag" := "ParamTag_ByGroupId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByGroupsIds ) =
       "tag" := "ParamTag_ByGroupsIds"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByGroupMemberId ) =
       "tag" := "ParamTag_ByGroupMemberId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByGroupMembersIds ) =
       "tag" := "ParamTag_ByGroupMembersIds"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByForumId ) =
       "tag" := "ParamTag_ByForumId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByForumsIds ) =
       "tag" := "ParamTag_ByForumsIds"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByForumName ) =
       "tag" := "ParamTag_ByForumName"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByBoardId ) =
       "tag" := "ParamTag_ByBoardId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByBoardsIds ) =
       "tag" := "ParamTag_ByBoardsIds"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByBoardName ) =
       "tag" := "ParamTag_ByBoardName"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByThreadId ) =
       "tag" := "ParamTag_ByThreadId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByThreadsIds ) =
       "tag" := "ParamTag_ByThreadsIds"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByThreadName ) =
       "tag" := "ParamTag_ByThreadName"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByThreadPostId ) =
       "tag" := "ParamTag_ByThreadPostId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByThreadPostsIds ) =
       "tag" := "ParamTag_ByThreadPostsIds"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByThreadPostName ) =
       "tag" := "ParamTag_ByThreadPostName"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByThreadPostLikeId ) =
       "tag" := "ParamTag_ByThreadPostLikeId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByThreadPostLikesIds ) =
       "tag" := "ParamTag_ByThreadPostLikesIds"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByThreadPostStarId ) =
       "tag" := "ParamTag_ByThreadPostStarId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByThreadPostStarsIds ) =
       "tag" := "ParamTag_ByThreadPostStarsIds"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByBucketId ) =
       "tag" := "ParamTag_ByBucketId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByBucketRoundId ) =
       "tag" := "ParamTag_ByBucketRoundId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByResourceId ) =
       "tag" := "ParamTag_ByResourceId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByResourcesIds ) =
       "tag" := "ParamTag_ByResourcesIds"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByResourceName ) =
       "tag" := "ParamTag_ByResourceName"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByLeuronId ) =
       "tag" := "ParamTag_ByLeuronId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByLeuronsIds ) =
       "tag" := "ParamTag_ByLeuronsIds"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByPmId ) =
       "tag" := "ParamTag_ByPmId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByPmsIds ) =
       "tag" := "ParamTag_ByPmsIds"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByReminderId ) =
       "tag" := "ParamTag_ByReminderId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByReminderFolderId ) =
       "tag" := "ParamTag_ByReminderFolderId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByParentId ) =
       "tag" := "ParamTag_ByParentId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByParentsIds ) =
       "tag" := "ParamTag_ByParentsIds"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByParentName ) =
       "tag" := "ParamTag_ByParentName"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_ByEmail ) =
       "tag" := "ParamTag_ByEmail"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_BySelf ) =
       "tag" := "ParamTag_BySelf"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_View ) =
       "tag" := "ParamTag_View"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_Timestamp ) =
       "tag" := "ParamTag_Timestamp"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_UnixTimestamp ) =
       "tag" := "ParamTag_UnixTimestamp"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_CreatedAtTimestamp ) =
       "tag" := "ParamTag_CreatedAtTimestamp"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_CreatedAtUnixTimestamp ) =
       "tag" := "ParamTag_CreatedAtUnixTimestamp"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_RealIP ) =
       "tag" := "ParamTag_RealIP"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_IP ) =
       "tag" := "ParamTag_IP"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_WithOrganization ) =
       "tag" := "ParamTag_WithOrganization"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_WithForum ) =
       "tag" := "ParamTag_WithForum"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_WithBoard ) =
       "tag" := "ParamTag_WithBoard"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_WithThread ) =
       "tag" := "ParamTag_WithThread"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_WithThreadPosts ) =
       "tag" := "ParamTag_WithThreadPosts"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (ParamTag_WithResource ) =
       "tag" := "ParamTag_WithResource"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance paramTagDecodeJson :: DecodeJson ParamTag where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "ParamTag_Limit" -> do
        pure ParamTag_Limit

      "ParamTag_Offset" -> do
        pure ParamTag_Offset

      "ParamTag_SortOrder" -> do
        pure ParamTag_SortOrder

      "ParamTag_Order" -> do
        pure ParamTag_Order

      "ParamTag_ByOrganizationId" -> do
        pure ParamTag_ByOrganizationId

      "ParamTag_ByOrganizationsIds" -> do
        pure ParamTag_ByOrganizationsIds

      "ParamTag_ByOrganizationName" -> do
        pure ParamTag_ByOrganizationName

      "ParamTag_ByTeamId" -> do
        pure ParamTag_ByTeamId

      "ParamTag_ByTeamsIds" -> do
        pure ParamTag_ByTeamsIds

      "ParamTag_ByTeamName" -> do
        pure ParamTag_ByTeamName

      "ParamTag_ByTeamMemberId" -> do
        pure ParamTag_ByTeamMemberId

      "ParamTag_ByTeamMembersIds" -> do
        pure ParamTag_ByTeamMembersIds

      "ParamTag_ByUserId" -> do
        pure ParamTag_ByUserId

      "ParamTag_ByUsersIds" -> do
        pure ParamTag_ByUsersIds

      "ParamTag_ByUserName" -> do
        pure ParamTag_ByUserName

      "ParamTag_ByUsersNames" -> do
        pure ParamTag_ByUsersNames

      "ParamTag_ByGlobalGroupId" -> do
        pure ParamTag_ByGlobalGroupId

      "ParamTag_ByGlobalGroupsIds" -> do
        pure ParamTag_ByGlobalGroupsIds

      "ParamTag_ByGroupId" -> do
        pure ParamTag_ByGroupId

      "ParamTag_ByGroupsIds" -> do
        pure ParamTag_ByGroupsIds

      "ParamTag_ByGroupMemberId" -> do
        pure ParamTag_ByGroupMemberId

      "ParamTag_ByGroupMembersIds" -> do
        pure ParamTag_ByGroupMembersIds

      "ParamTag_ByForumId" -> do
        pure ParamTag_ByForumId

      "ParamTag_ByForumsIds" -> do
        pure ParamTag_ByForumsIds

      "ParamTag_ByForumName" -> do
        pure ParamTag_ByForumName

      "ParamTag_ByBoardId" -> do
        pure ParamTag_ByBoardId

      "ParamTag_ByBoardsIds" -> do
        pure ParamTag_ByBoardsIds

      "ParamTag_ByBoardName" -> do
        pure ParamTag_ByBoardName

      "ParamTag_ByThreadId" -> do
        pure ParamTag_ByThreadId

      "ParamTag_ByThreadsIds" -> do
        pure ParamTag_ByThreadsIds

      "ParamTag_ByThreadName" -> do
        pure ParamTag_ByThreadName

      "ParamTag_ByThreadPostId" -> do
        pure ParamTag_ByThreadPostId

      "ParamTag_ByThreadPostsIds" -> do
        pure ParamTag_ByThreadPostsIds

      "ParamTag_ByThreadPostName" -> do
        pure ParamTag_ByThreadPostName

      "ParamTag_ByThreadPostLikeId" -> do
        pure ParamTag_ByThreadPostLikeId

      "ParamTag_ByThreadPostLikesIds" -> do
        pure ParamTag_ByThreadPostLikesIds

      "ParamTag_ByThreadPostStarId" -> do
        pure ParamTag_ByThreadPostStarId

      "ParamTag_ByThreadPostStarsIds" -> do
        pure ParamTag_ByThreadPostStarsIds

      "ParamTag_ByBucketId" -> do
        pure ParamTag_ByBucketId

      "ParamTag_ByBucketRoundId" -> do
        pure ParamTag_ByBucketRoundId

      "ParamTag_ByResourceId" -> do
        pure ParamTag_ByResourceId

      "ParamTag_ByResourcesIds" -> do
        pure ParamTag_ByResourcesIds

      "ParamTag_ByResourceName" -> do
        pure ParamTag_ByResourceName

      "ParamTag_ByLeuronId" -> do
        pure ParamTag_ByLeuronId

      "ParamTag_ByLeuronsIds" -> do
        pure ParamTag_ByLeuronsIds

      "ParamTag_ByPmId" -> do
        pure ParamTag_ByPmId

      "ParamTag_ByPmsIds" -> do
        pure ParamTag_ByPmsIds

      "ParamTag_ByReminderId" -> do
        pure ParamTag_ByReminderId

      "ParamTag_ByReminderFolderId" -> do
        pure ParamTag_ByReminderFolderId

      "ParamTag_ByParentId" -> do
        pure ParamTag_ByParentId

      "ParamTag_ByParentsIds" -> do
        pure ParamTag_ByParentsIds

      "ParamTag_ByParentName" -> do
        pure ParamTag_ByParentName

      "ParamTag_ByEmail" -> do
        pure ParamTag_ByEmail

      "ParamTag_BySelf" -> do
        pure ParamTag_BySelf

      "ParamTag_View" -> do
        pure ParamTag_View

      "ParamTag_Timestamp" -> do
        pure ParamTag_Timestamp

      "ParamTag_UnixTimestamp" -> do
        pure ParamTag_UnixTimestamp

      "ParamTag_CreatedAtTimestamp" -> do
        pure ParamTag_CreatedAtTimestamp

      "ParamTag_CreatedAtUnixTimestamp" -> do
        pure ParamTag_CreatedAtUnixTimestamp

      "ParamTag_RealIP" -> do
        pure ParamTag_RealIP

      "ParamTag_IP" -> do
        pure ParamTag_IP

      "ParamTag_WithOrganization" -> do
        pure ParamTag_WithOrganization

      "ParamTag_WithForum" -> do
        pure ParamTag_WithForum

      "ParamTag_WithBoard" -> do
        pure ParamTag_WithBoard

      "ParamTag_WithThread" -> do
        pure ParamTag_WithThread

      "ParamTag_WithThreadPosts" -> do
        pure ParamTag_WithThreadPosts

      "ParamTag_WithResource" -> do
        pure ParamTag_WithResource

      _ -> Left $ "DecodeJson TypeMismatch for ParamTag"



instance paramTagRequestable :: Requestable ParamTag where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance paramTagRespondable :: Respondable ParamTag where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "ParamTag_Limit" -> do
        pure ParamTag_Limit

      "ParamTag_Offset" -> do
        pure ParamTag_Offset

      "ParamTag_SortOrder" -> do
        pure ParamTag_SortOrder

      "ParamTag_Order" -> do
        pure ParamTag_Order

      "ParamTag_ByOrganizationId" -> do
        pure ParamTag_ByOrganizationId

      "ParamTag_ByOrganizationsIds" -> do
        pure ParamTag_ByOrganizationsIds

      "ParamTag_ByOrganizationName" -> do
        pure ParamTag_ByOrganizationName

      "ParamTag_ByTeamId" -> do
        pure ParamTag_ByTeamId

      "ParamTag_ByTeamsIds" -> do
        pure ParamTag_ByTeamsIds

      "ParamTag_ByTeamName" -> do
        pure ParamTag_ByTeamName

      "ParamTag_ByTeamMemberId" -> do
        pure ParamTag_ByTeamMemberId

      "ParamTag_ByTeamMembersIds" -> do
        pure ParamTag_ByTeamMembersIds

      "ParamTag_ByUserId" -> do
        pure ParamTag_ByUserId

      "ParamTag_ByUsersIds" -> do
        pure ParamTag_ByUsersIds

      "ParamTag_ByUserName" -> do
        pure ParamTag_ByUserName

      "ParamTag_ByUsersNames" -> do
        pure ParamTag_ByUsersNames

      "ParamTag_ByGlobalGroupId" -> do
        pure ParamTag_ByGlobalGroupId

      "ParamTag_ByGlobalGroupsIds" -> do
        pure ParamTag_ByGlobalGroupsIds

      "ParamTag_ByGroupId" -> do
        pure ParamTag_ByGroupId

      "ParamTag_ByGroupsIds" -> do
        pure ParamTag_ByGroupsIds

      "ParamTag_ByGroupMemberId" -> do
        pure ParamTag_ByGroupMemberId

      "ParamTag_ByGroupMembersIds" -> do
        pure ParamTag_ByGroupMembersIds

      "ParamTag_ByForumId" -> do
        pure ParamTag_ByForumId

      "ParamTag_ByForumsIds" -> do
        pure ParamTag_ByForumsIds

      "ParamTag_ByForumName" -> do
        pure ParamTag_ByForumName

      "ParamTag_ByBoardId" -> do
        pure ParamTag_ByBoardId

      "ParamTag_ByBoardsIds" -> do
        pure ParamTag_ByBoardsIds

      "ParamTag_ByBoardName" -> do
        pure ParamTag_ByBoardName

      "ParamTag_ByThreadId" -> do
        pure ParamTag_ByThreadId

      "ParamTag_ByThreadsIds" -> do
        pure ParamTag_ByThreadsIds

      "ParamTag_ByThreadName" -> do
        pure ParamTag_ByThreadName

      "ParamTag_ByThreadPostId" -> do
        pure ParamTag_ByThreadPostId

      "ParamTag_ByThreadPostsIds" -> do
        pure ParamTag_ByThreadPostsIds

      "ParamTag_ByThreadPostName" -> do
        pure ParamTag_ByThreadPostName

      "ParamTag_ByThreadPostLikeId" -> do
        pure ParamTag_ByThreadPostLikeId

      "ParamTag_ByThreadPostLikesIds" -> do
        pure ParamTag_ByThreadPostLikesIds

      "ParamTag_ByThreadPostStarId" -> do
        pure ParamTag_ByThreadPostStarId

      "ParamTag_ByThreadPostStarsIds" -> do
        pure ParamTag_ByThreadPostStarsIds

      "ParamTag_ByBucketId" -> do
        pure ParamTag_ByBucketId

      "ParamTag_ByBucketRoundId" -> do
        pure ParamTag_ByBucketRoundId

      "ParamTag_ByResourceId" -> do
        pure ParamTag_ByResourceId

      "ParamTag_ByResourcesIds" -> do
        pure ParamTag_ByResourcesIds

      "ParamTag_ByResourceName" -> do
        pure ParamTag_ByResourceName

      "ParamTag_ByLeuronId" -> do
        pure ParamTag_ByLeuronId

      "ParamTag_ByLeuronsIds" -> do
        pure ParamTag_ByLeuronsIds

      "ParamTag_ByPmId" -> do
        pure ParamTag_ByPmId

      "ParamTag_ByPmsIds" -> do
        pure ParamTag_ByPmsIds

      "ParamTag_ByReminderId" -> do
        pure ParamTag_ByReminderId

      "ParamTag_ByReminderFolderId" -> do
        pure ParamTag_ByReminderFolderId

      "ParamTag_ByParentId" -> do
        pure ParamTag_ByParentId

      "ParamTag_ByParentsIds" -> do
        pure ParamTag_ByParentsIds

      "ParamTag_ByParentName" -> do
        pure ParamTag_ByParentName

      "ParamTag_ByEmail" -> do
        pure ParamTag_ByEmail

      "ParamTag_BySelf" -> do
        pure ParamTag_BySelf

      "ParamTag_View" -> do
        pure ParamTag_View

      "ParamTag_Timestamp" -> do
        pure ParamTag_Timestamp

      "ParamTag_UnixTimestamp" -> do
        pure ParamTag_UnixTimestamp

      "ParamTag_CreatedAtTimestamp" -> do
        pure ParamTag_CreatedAtTimestamp

      "ParamTag_CreatedAtUnixTimestamp" -> do
        pure ParamTag_CreatedAtUnixTimestamp

      "ParamTag_RealIP" -> do
        pure ParamTag_RealIP

      "ParamTag_IP" -> do
        pure ParamTag_IP

      "ParamTag_WithOrganization" -> do
        pure ParamTag_WithOrganization

      "ParamTag_WithForum" -> do
        pure ParamTag_WithForum

      "ParamTag_WithBoard" -> do
        pure ParamTag_WithBoard

      "ParamTag_WithThread" -> do
        pure ParamTag_WithThread

      "ParamTag_WithThreadPosts" -> do
        pure ParamTag_WithThreadPosts

      "ParamTag_WithResource" -> do
        pure ParamTag_WithResource

      _ -> fail $ TypeMismatch "ParamTag" "Respondable"



instance paramTagDecode :: Decode ParamTag where
  decode json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "ParamTag_Limit" -> do
        pure ParamTag_Limit

      "ParamTag_Offset" -> do
        pure ParamTag_Offset

      "ParamTag_SortOrder" -> do
        pure ParamTag_SortOrder

      "ParamTag_Order" -> do
        pure ParamTag_Order

      "ParamTag_ByOrganizationId" -> do
        pure ParamTag_ByOrganizationId

      "ParamTag_ByOrganizationsIds" -> do
        pure ParamTag_ByOrganizationsIds

      "ParamTag_ByOrganizationName" -> do
        pure ParamTag_ByOrganizationName

      "ParamTag_ByTeamId" -> do
        pure ParamTag_ByTeamId

      "ParamTag_ByTeamsIds" -> do
        pure ParamTag_ByTeamsIds

      "ParamTag_ByTeamName" -> do
        pure ParamTag_ByTeamName

      "ParamTag_ByTeamMemberId" -> do
        pure ParamTag_ByTeamMemberId

      "ParamTag_ByTeamMembersIds" -> do
        pure ParamTag_ByTeamMembersIds

      "ParamTag_ByUserId" -> do
        pure ParamTag_ByUserId

      "ParamTag_ByUsersIds" -> do
        pure ParamTag_ByUsersIds

      "ParamTag_ByUserName" -> do
        pure ParamTag_ByUserName

      "ParamTag_ByUsersNames" -> do
        pure ParamTag_ByUsersNames

      "ParamTag_ByGlobalGroupId" -> do
        pure ParamTag_ByGlobalGroupId

      "ParamTag_ByGlobalGroupsIds" -> do
        pure ParamTag_ByGlobalGroupsIds

      "ParamTag_ByGroupId" -> do
        pure ParamTag_ByGroupId

      "ParamTag_ByGroupsIds" -> do
        pure ParamTag_ByGroupsIds

      "ParamTag_ByGroupMemberId" -> do
        pure ParamTag_ByGroupMemberId

      "ParamTag_ByGroupMembersIds" -> do
        pure ParamTag_ByGroupMembersIds

      "ParamTag_ByForumId" -> do
        pure ParamTag_ByForumId

      "ParamTag_ByForumsIds" -> do
        pure ParamTag_ByForumsIds

      "ParamTag_ByForumName" -> do
        pure ParamTag_ByForumName

      "ParamTag_ByBoardId" -> do
        pure ParamTag_ByBoardId

      "ParamTag_ByBoardsIds" -> do
        pure ParamTag_ByBoardsIds

      "ParamTag_ByBoardName" -> do
        pure ParamTag_ByBoardName

      "ParamTag_ByThreadId" -> do
        pure ParamTag_ByThreadId

      "ParamTag_ByThreadsIds" -> do
        pure ParamTag_ByThreadsIds

      "ParamTag_ByThreadName" -> do
        pure ParamTag_ByThreadName

      "ParamTag_ByThreadPostId" -> do
        pure ParamTag_ByThreadPostId

      "ParamTag_ByThreadPostsIds" -> do
        pure ParamTag_ByThreadPostsIds

      "ParamTag_ByThreadPostName" -> do
        pure ParamTag_ByThreadPostName

      "ParamTag_ByThreadPostLikeId" -> do
        pure ParamTag_ByThreadPostLikeId

      "ParamTag_ByThreadPostLikesIds" -> do
        pure ParamTag_ByThreadPostLikesIds

      "ParamTag_ByThreadPostStarId" -> do
        pure ParamTag_ByThreadPostStarId

      "ParamTag_ByThreadPostStarsIds" -> do
        pure ParamTag_ByThreadPostStarsIds

      "ParamTag_ByBucketId" -> do
        pure ParamTag_ByBucketId

      "ParamTag_ByBucketRoundId" -> do
        pure ParamTag_ByBucketRoundId

      "ParamTag_ByResourceId" -> do
        pure ParamTag_ByResourceId

      "ParamTag_ByResourcesIds" -> do
        pure ParamTag_ByResourcesIds

      "ParamTag_ByResourceName" -> do
        pure ParamTag_ByResourceName

      "ParamTag_ByLeuronId" -> do
        pure ParamTag_ByLeuronId

      "ParamTag_ByLeuronsIds" -> do
        pure ParamTag_ByLeuronsIds

      "ParamTag_ByPmId" -> do
        pure ParamTag_ByPmId

      "ParamTag_ByPmsIds" -> do
        pure ParamTag_ByPmsIds

      "ParamTag_ByReminderId" -> do
        pure ParamTag_ByReminderId

      "ParamTag_ByReminderFolderId" -> do
        pure ParamTag_ByReminderFolderId

      "ParamTag_ByParentId" -> do
        pure ParamTag_ByParentId

      "ParamTag_ByParentsIds" -> do
        pure ParamTag_ByParentsIds

      "ParamTag_ByParentName" -> do
        pure ParamTag_ByParentName

      "ParamTag_ByEmail" -> do
        pure ParamTag_ByEmail

      "ParamTag_BySelf" -> do
        pure ParamTag_BySelf

      "ParamTag_View" -> do
        pure ParamTag_View

      "ParamTag_Timestamp" -> do
        pure ParamTag_Timestamp

      "ParamTag_UnixTimestamp" -> do
        pure ParamTag_UnixTimestamp

      "ParamTag_CreatedAtTimestamp" -> do
        pure ParamTag_CreatedAtTimestamp

      "ParamTag_CreatedAtUnixTimestamp" -> do
        pure ParamTag_CreatedAtUnixTimestamp

      "ParamTag_RealIP" -> do
        pure ParamTag_RealIP

      "ParamTag_IP" -> do
        pure ParamTag_IP

      "ParamTag_WithOrganization" -> do
        pure ParamTag_WithOrganization

      "ParamTag_WithForum" -> do
        pure ParamTag_WithForum

      "ParamTag_WithBoard" -> do
        pure ParamTag_WithBoard

      "ParamTag_WithThread" -> do
        pure ParamTag_WithThread

      "ParamTag_WithThreadPosts" -> do
        pure ParamTag_WithThreadPosts

      "ParamTag_WithResource" -> do
        pure ParamTag_WithResource

      _ -> fail $ TypeMismatch "ParamTag" "Decode"



instance paramTagEq :: Eq ParamTag where
  eq ParamTag_Limit ParamTag_Limit = true
  eq ParamTag_Offset ParamTag_Offset = true
  eq ParamTag_SortOrder ParamTag_SortOrder = true
  eq ParamTag_Order ParamTag_Order = true
  eq ParamTag_ByOrganizationId ParamTag_ByOrganizationId = true
  eq ParamTag_ByOrganizationsIds ParamTag_ByOrganizationsIds = true
  eq ParamTag_ByOrganizationName ParamTag_ByOrganizationName = true
  eq ParamTag_ByTeamId ParamTag_ByTeamId = true
  eq ParamTag_ByTeamsIds ParamTag_ByTeamsIds = true
  eq ParamTag_ByTeamName ParamTag_ByTeamName = true
  eq ParamTag_ByTeamMemberId ParamTag_ByTeamMemberId = true
  eq ParamTag_ByTeamMembersIds ParamTag_ByTeamMembersIds = true
  eq ParamTag_ByUserId ParamTag_ByUserId = true
  eq ParamTag_ByUsersIds ParamTag_ByUsersIds = true
  eq ParamTag_ByUserName ParamTag_ByUserName = true
  eq ParamTag_ByUsersNames ParamTag_ByUsersNames = true
  eq ParamTag_ByGlobalGroupId ParamTag_ByGlobalGroupId = true
  eq ParamTag_ByGlobalGroupsIds ParamTag_ByGlobalGroupsIds = true
  eq ParamTag_ByGroupId ParamTag_ByGroupId = true
  eq ParamTag_ByGroupsIds ParamTag_ByGroupsIds = true
  eq ParamTag_ByGroupMemberId ParamTag_ByGroupMemberId = true
  eq ParamTag_ByGroupMembersIds ParamTag_ByGroupMembersIds = true
  eq ParamTag_ByForumId ParamTag_ByForumId = true
  eq ParamTag_ByForumsIds ParamTag_ByForumsIds = true
  eq ParamTag_ByForumName ParamTag_ByForumName = true
  eq ParamTag_ByBoardId ParamTag_ByBoardId = true
  eq ParamTag_ByBoardsIds ParamTag_ByBoardsIds = true
  eq ParamTag_ByBoardName ParamTag_ByBoardName = true
  eq ParamTag_ByThreadId ParamTag_ByThreadId = true
  eq ParamTag_ByThreadsIds ParamTag_ByThreadsIds = true
  eq ParamTag_ByThreadName ParamTag_ByThreadName = true
  eq ParamTag_ByThreadPostId ParamTag_ByThreadPostId = true
  eq ParamTag_ByThreadPostsIds ParamTag_ByThreadPostsIds = true
  eq ParamTag_ByThreadPostName ParamTag_ByThreadPostName = true
  eq ParamTag_ByThreadPostLikeId ParamTag_ByThreadPostLikeId = true
  eq ParamTag_ByThreadPostLikesIds ParamTag_ByThreadPostLikesIds = true
  eq ParamTag_ByThreadPostStarId ParamTag_ByThreadPostStarId = true
  eq ParamTag_ByThreadPostStarsIds ParamTag_ByThreadPostStarsIds = true
  eq ParamTag_ByBucketId ParamTag_ByBucketId = true
  eq ParamTag_ByBucketRoundId ParamTag_ByBucketRoundId = true
  eq ParamTag_ByResourceId ParamTag_ByResourceId = true
  eq ParamTag_ByResourcesIds ParamTag_ByResourcesIds = true
  eq ParamTag_ByResourceName ParamTag_ByResourceName = true
  eq ParamTag_ByLeuronId ParamTag_ByLeuronId = true
  eq ParamTag_ByLeuronsIds ParamTag_ByLeuronsIds = true
  eq ParamTag_ByPmId ParamTag_ByPmId = true
  eq ParamTag_ByPmsIds ParamTag_ByPmsIds = true
  eq ParamTag_ByReminderId ParamTag_ByReminderId = true
  eq ParamTag_ByReminderFolderId ParamTag_ByReminderFolderId = true
  eq ParamTag_ByParentId ParamTag_ByParentId = true
  eq ParamTag_ByParentsIds ParamTag_ByParentsIds = true
  eq ParamTag_ByParentName ParamTag_ByParentName = true
  eq ParamTag_ByEmail ParamTag_ByEmail = true
  eq ParamTag_BySelf ParamTag_BySelf = true
  eq ParamTag_View ParamTag_View = true
  eq ParamTag_Timestamp ParamTag_Timestamp = true
  eq ParamTag_UnixTimestamp ParamTag_UnixTimestamp = true
  eq ParamTag_CreatedAtTimestamp ParamTag_CreatedAtTimestamp = true
  eq ParamTag_CreatedAtUnixTimestamp ParamTag_CreatedAtUnixTimestamp = true
  eq ParamTag_RealIP ParamTag_RealIP = true
  eq ParamTag_IP ParamTag_IP = true
  eq ParamTag_WithOrganization ParamTag_WithOrganization = true
  eq ParamTag_WithForum ParamTag_WithForum = true
  eq ParamTag_WithBoard ParamTag_WithBoard = true
  eq ParamTag_WithThread ParamTag_WithThread = true
  eq ParamTag_WithThreadPosts ParamTag_WithThreadPosts = true
  eq ParamTag_WithResource ParamTag_WithResource = true
  eq _ _ = false

instance paramTagShow :: Show ParamTag where
  show ParamTag_Limit = "limit"
  show ParamTag_Offset = "offset"
  show ParamTag_SortOrder = "sort_order"
  show ParamTag_Order = "order"
  show ParamTag_ByOrganizationId = "by_organization_id"
  show ParamTag_ByOrganizationsIds = "by_organizations_ids"
  show ParamTag_ByOrganizationName = "by_organization_name"
  show ParamTag_ByTeamId = "by_team_id"
  show ParamTag_ByTeamsIds = "by_teams_ids"
  show ParamTag_ByTeamName = "by_team_name"
  show ParamTag_ByTeamMemberId = "by_team_member_id"
  show ParamTag_ByTeamMembersIds = "by_team_members_ids"
  show ParamTag_ByUserId = "by_user_id"
  show ParamTag_ByUsersIds = "by_users_ids"
  show ParamTag_ByUserName = "by_user_name"
  show ParamTag_ByUsersNames = "by_users_names"
  show ParamTag_ByGlobalGroupId = "by_global_group_id"
  show ParamTag_ByGlobalGroupsIds = "by_global_groups_ids"
  show ParamTag_ByGroupId = "by_group_id"
  show ParamTag_ByGroupsIds = "by_groups_ids"
  show ParamTag_ByGroupMemberId = "by_group_member_id"
  show ParamTag_ByGroupMembersIds = "by_group_members_ids"
  show ParamTag_ByForumId = "by_forum_id"
  show ParamTag_ByForumsIds = "by_forums_ids"
  show ParamTag_ByForumName = "by_forum_name"
  show ParamTag_ByBoardId = "by_board_id"
  show ParamTag_ByBoardsIds = "by_boards_ids"
  show ParamTag_ByBoardName = "by_board_name"
  show ParamTag_ByThreadId = "by_thread_id"
  show ParamTag_ByThreadsIds = "by_threads_ids"
  show ParamTag_ByThreadName = "by_thread_name"
  show ParamTag_ByThreadPostId = "by_thread_post_id"
  show ParamTag_ByThreadPostsIds = "by_thread_posts_ids"
  show ParamTag_ByThreadPostName = "by_thread_post_name"
  show ParamTag_ByThreadPostLikeId = "by_thread_post_like_id"
  show ParamTag_ByThreadPostLikesIds = "by_thread_post_likes_ids"
  show ParamTag_ByThreadPostStarId = "by_thread_post_star_id"
  show ParamTag_ByThreadPostStarsIds = "by_thread_post_stars_ids"
  show ParamTag_ByBucketId = "by_bucket_id"
  show ParamTag_ByBucketRoundId = "by_bucket_round_id"
  show ParamTag_ByResourceId = "by_resource_id"
  show ParamTag_ByResourcesIds = "by_resources_ids"
  show ParamTag_ByResourceName = "by_resource_name"
  show ParamTag_ByLeuronId = "by_leuron_id"
  show ParamTag_ByLeuronsIds = "by_leurons_ids"
  show ParamTag_ByPmId = "by_pm_id"
  show ParamTag_ByPmsIds = "by_pms_ids"
  show ParamTag_ByReminderId = "by_reminder_id"
  show ParamTag_ByReminderFolderId = "by_reminder_folder_id"
  show ParamTag_ByParentId = "by_parent_id"
  show ParamTag_ByParentsIds = "by_parents_ids"
  show ParamTag_ByParentName = "by_parent_name"
  show ParamTag_ByEmail = "by_email"
  show ParamTag_BySelf = "by_self"
  show ParamTag_View = "view"
  show ParamTag_Timestamp = "timestamp"
  show ParamTag_UnixTimestamp = "unix_timestamp"
  show ParamTag_CreatedAtTimestamp = "created_at_timestamp"
  show ParamTag_CreatedAtUnixTimestamp = "created_at_unix_timestamp"
  show ParamTag_RealIP = "real_ip"
  show ParamTag_IP = "ip"
  show ParamTag_WithOrganization = "with_organization"
  show ParamTag_WithForum = "with_forum"
  show ParamTag_WithBoard = "with_board"
  show ParamTag_WithThread = "with_thread"
  show ParamTag_WithThreadPosts = "with_thread_posts"
  show ParamTag_WithResource = "with_resource"


readParamTag :: String -> Maybe ParamTag
readParamTag "limit" = Just ParamTag_Limit
readParamTag "offset" = Just ParamTag_Offset
readParamTag "sort_order" = Just ParamTag_SortOrder
readParamTag "order" = Just ParamTag_Order
readParamTag "by_organization_id" = Just ParamTag_ByOrganizationId
readParamTag "by_organizations_ids" = Just ParamTag_ByOrganizationsIds
readParamTag "by_organization_name" = Just ParamTag_ByOrganizationName
readParamTag "by_team_id" = Just ParamTag_ByTeamId
readParamTag "by_teams_ids" = Just ParamTag_ByTeamsIds
readParamTag "by_team_name" = Just ParamTag_ByTeamName
readParamTag "by_team_member_id" = Just ParamTag_ByTeamMemberId
readParamTag "by_team_members_ids" = Just ParamTag_ByTeamMembersIds
readParamTag "by_user_id" = Just ParamTag_ByUserId
readParamTag "by_users_ids" = Just ParamTag_ByUsersIds
readParamTag "by_user_name" = Just ParamTag_ByUserName
readParamTag "by_users_names" = Just ParamTag_ByUsersNames
readParamTag "by_global_group_id" = Just ParamTag_ByGlobalGroupId
readParamTag "by_global_groups_ids" = Just ParamTag_ByGlobalGroupsIds
readParamTag "by_group_id" = Just ParamTag_ByGroupId
readParamTag "by_groups_ids" = Just ParamTag_ByGroupsIds
readParamTag "by_group_member_id" = Just ParamTag_ByGroupMemberId
readParamTag "by_group_members_ids" = Just ParamTag_ByGroupMembersIds
readParamTag "by_forum_id" = Just ParamTag_ByForumId
readParamTag "by_forums_ids" = Just ParamTag_ByForumsIds
readParamTag "by_forum_name" = Just ParamTag_ByForumName
readParamTag "by_board_id" = Just ParamTag_ByBoardId
readParamTag "by_boards_ids" = Just ParamTag_ByBoardsIds
readParamTag "by_board_name" = Just ParamTag_ByBoardName
readParamTag "by_thread_id" = Just ParamTag_ByThreadId
readParamTag "by_threads_ids" = Just ParamTag_ByThreadsIds
readParamTag "by_thread_name" = Just ParamTag_ByThreadName
readParamTag "by_thread_post_id" = Just ParamTag_ByThreadPostId
readParamTag "by_thread_posts_ids" = Just ParamTag_ByThreadPostsIds
readParamTag "by_thread_post_name" = Just ParamTag_ByThreadPostName
readParamTag "by_thread_post_like_id" = Just ParamTag_ByThreadPostLikeId
readParamTag "by_thread_post_likes_ids" = Just ParamTag_ByThreadPostLikesIds
readParamTag "by_thread_post_star_id" = Just ParamTag_ByThreadPostStarId
readParamTag "by_thread_post_stars_ids" = Just ParamTag_ByThreadPostStarsIds
readParamTag "by_bucket_id" = Just ParamTag_ByBucketId
readParamTag "by_bucket_round_id" = Just ParamTag_ByBucketRoundId
readParamTag "by_resource_id" = Just ParamTag_ByResourceId
readParamTag "by_resources_ids" = Just ParamTag_ByResourcesIds
readParamTag "by_resource_name" = Just ParamTag_ByResourceName
readParamTag "by_leuron_id" = Just ParamTag_ByLeuronId
readParamTag "by_leurons_ids" = Just ParamTag_ByLeuronsIds
readParamTag "by_pm_id" = Just ParamTag_ByPmId
readParamTag "by_pms_ids" = Just ParamTag_ByPmsIds
readParamTag "by_reminder_id" = Just ParamTag_ByReminderId
readParamTag "by_reminder_folder_id" = Just ParamTag_ByReminderFolderId
readParamTag "by_parent_id" = Just ParamTag_ByParentId
readParamTag "by_parents_ids" = Just ParamTag_ByParentsIds
readParamTag "by_parent_name" = Just ParamTag_ByParentName
readParamTag "by_email" = Just ParamTag_ByEmail
readParamTag "by_self" = Just ParamTag_BySelf
readParamTag "view" = Just ParamTag_View
readParamTag "timestamp" = Just ParamTag_Timestamp
readParamTag "unix_timestamp" = Just ParamTag_UnixTimestamp
readParamTag "created_at_timestamp" = Just ParamTag_CreatedAtTimestamp
readParamTag "created_at_unix_timestamp" = Just ParamTag_CreatedAtUnixTimestamp
readParamTag "real_ip" = Just ParamTag_RealIP
readParamTag "ip" = Just ParamTag_IP
readParamTag "with_organization" = Just ParamTag_WithOrganization
readParamTag "with_forum" = Just ParamTag_WithForum
readParamTag "with_board" = Just ParamTag_WithBoard
readParamTag "with_thread" = Just ParamTag_WithThread
readParamTag "with_thread_posts" = Just ParamTag_WithThreadPosts
readParamTag "with_resource" = Just ParamTag_WithResource
readParamTag _ = Nothing

data SortOrderBy
  = SortOrderBy_Asc 
  | SortOrderBy_Dsc 
  | SortOrderBy_Rnd 
  | SortOrderBy_None 



instance sortOrderByEncodeJson :: EncodeJson SortOrderBy where
  encodeJson (SortOrderBy_Asc ) =
       "tag" := "SortOrderBy_Asc"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (SortOrderBy_Dsc ) =
       "tag" := "SortOrderBy_Dsc"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (SortOrderBy_Rnd ) =
       "tag" := "SortOrderBy_Rnd"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (SortOrderBy_None ) =
       "tag" := "SortOrderBy_None"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance sortOrderByDecodeJson :: DecodeJson SortOrderBy where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "SortOrderBy_Asc" -> do
        pure SortOrderBy_Asc

      "SortOrderBy_Dsc" -> do
        pure SortOrderBy_Dsc

      "SortOrderBy_Rnd" -> do
        pure SortOrderBy_Rnd

      "SortOrderBy_None" -> do
        pure SortOrderBy_None

      _ -> Left $ "DecodeJson TypeMismatch for SortOrderBy"



instance sortOrderByRequestable :: Requestable SortOrderBy where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance sortOrderByRespondable :: Respondable SortOrderBy where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "SortOrderBy_Asc" -> do
        pure SortOrderBy_Asc

      "SortOrderBy_Dsc" -> do
        pure SortOrderBy_Dsc

      "SortOrderBy_Rnd" -> do
        pure SortOrderBy_Rnd

      "SortOrderBy_None" -> do
        pure SortOrderBy_None

      _ -> fail $ TypeMismatch "SortOrderBy" "Respondable"



instance sortOrderByDecode :: Decode SortOrderBy where
  decode json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "SortOrderBy_Asc" -> do
        pure SortOrderBy_Asc

      "SortOrderBy_Dsc" -> do
        pure SortOrderBy_Dsc

      "SortOrderBy_Rnd" -> do
        pure SortOrderBy_Rnd

      "SortOrderBy_None" -> do
        pure SortOrderBy_None

      _ -> fail $ TypeMismatch "SortOrderBy" "Decode"



instance sortOrderByEq :: Eq SortOrderBy where
  eq SortOrderBy_Asc SortOrderBy_Asc = true
  eq SortOrderBy_Dsc SortOrderBy_Dsc = true
  eq SortOrderBy_Rnd SortOrderBy_Rnd = true
  eq SortOrderBy_None SortOrderBy_None = true
  eq _ _ = false

instance sortOrderByShow :: Show SortOrderBy where
  show SortOrderBy_Asc = "asc"
  show SortOrderBy_Dsc = "dsc"
  show SortOrderBy_Rnd = "rnd"
  show SortOrderBy_None = "none"


readSortOrderBy :: String -> Maybe SortOrderBy
readSortOrderBy "asc" = Just SortOrderBy_Asc
readSortOrderBy "dsc" = Just SortOrderBy_Dsc
readSortOrderBy "rnd" = Just SortOrderBy_Rnd
readSortOrderBy "none" = Just SortOrderBy_None
readSortOrderBy _ = Nothing

data OrderBy
  = OrderBy_UserId 
  | OrderBy_CreatedAt 
  | OrderBy_ModifiedAt 
  | OrderBy_ModifiedBy 
  | OrderBy_ActivityAt 
  | OrderBy_OrganizationId 
  | OrderBy_TeamId 
  | OrderBy_ForumId 
  | OrderBy_BoardId 
  | OrderBy_ThreadId 
  | OrderBy_Id 
  | OrderBy_None 



instance orderByEncodeJson :: EncodeJson OrderBy where
  encodeJson (OrderBy_UserId ) =
       "tag" := "OrderBy_UserId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (OrderBy_CreatedAt ) =
       "tag" := "OrderBy_CreatedAt"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (OrderBy_ModifiedAt ) =
       "tag" := "OrderBy_ModifiedAt"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (OrderBy_ModifiedBy ) =
       "tag" := "OrderBy_ModifiedBy"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (OrderBy_ActivityAt ) =
       "tag" := "OrderBy_ActivityAt"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (OrderBy_OrganizationId ) =
       "tag" := "OrderBy_OrganizationId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (OrderBy_TeamId ) =
       "tag" := "OrderBy_TeamId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (OrderBy_ForumId ) =
       "tag" := "OrderBy_ForumId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (OrderBy_BoardId ) =
       "tag" := "OrderBy_BoardId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (OrderBy_ThreadId ) =
       "tag" := "OrderBy_ThreadId"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (OrderBy_Id ) =
       "tag" := "OrderBy_Id"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (OrderBy_None ) =
       "tag" := "OrderBy_None"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance orderByDecodeJson :: DecodeJson OrderBy where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "OrderBy_UserId" -> do
        pure OrderBy_UserId

      "OrderBy_CreatedAt" -> do
        pure OrderBy_CreatedAt

      "OrderBy_ModifiedAt" -> do
        pure OrderBy_ModifiedAt

      "OrderBy_ModifiedBy" -> do
        pure OrderBy_ModifiedBy

      "OrderBy_ActivityAt" -> do
        pure OrderBy_ActivityAt

      "OrderBy_OrganizationId" -> do
        pure OrderBy_OrganizationId

      "OrderBy_TeamId" -> do
        pure OrderBy_TeamId

      "OrderBy_ForumId" -> do
        pure OrderBy_ForumId

      "OrderBy_BoardId" -> do
        pure OrderBy_BoardId

      "OrderBy_ThreadId" -> do
        pure OrderBy_ThreadId

      "OrderBy_Id" -> do
        pure OrderBy_Id

      "OrderBy_None" -> do
        pure OrderBy_None

      _ -> Left $ "DecodeJson TypeMismatch for OrderBy"



instance orderByRequestable :: Requestable OrderBy where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance orderByRespondable :: Respondable OrderBy where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "OrderBy_UserId" -> do
        pure OrderBy_UserId

      "OrderBy_CreatedAt" -> do
        pure OrderBy_CreatedAt

      "OrderBy_ModifiedAt" -> do
        pure OrderBy_ModifiedAt

      "OrderBy_ModifiedBy" -> do
        pure OrderBy_ModifiedBy

      "OrderBy_ActivityAt" -> do
        pure OrderBy_ActivityAt

      "OrderBy_OrganizationId" -> do
        pure OrderBy_OrganizationId

      "OrderBy_TeamId" -> do
        pure OrderBy_TeamId

      "OrderBy_ForumId" -> do
        pure OrderBy_ForumId

      "OrderBy_BoardId" -> do
        pure OrderBy_BoardId

      "OrderBy_ThreadId" -> do
        pure OrderBy_ThreadId

      "OrderBy_Id" -> do
        pure OrderBy_Id

      "OrderBy_None" -> do
        pure OrderBy_None

      _ -> fail $ TypeMismatch "OrderBy" "Respondable"



instance orderByDecode :: Decode OrderBy where
  decode json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "OrderBy_UserId" -> do
        pure OrderBy_UserId

      "OrderBy_CreatedAt" -> do
        pure OrderBy_CreatedAt

      "OrderBy_ModifiedAt" -> do
        pure OrderBy_ModifiedAt

      "OrderBy_ModifiedBy" -> do
        pure OrderBy_ModifiedBy

      "OrderBy_ActivityAt" -> do
        pure OrderBy_ActivityAt

      "OrderBy_OrganizationId" -> do
        pure OrderBy_OrganizationId

      "OrderBy_TeamId" -> do
        pure OrderBy_TeamId

      "OrderBy_ForumId" -> do
        pure OrderBy_ForumId

      "OrderBy_BoardId" -> do
        pure OrderBy_BoardId

      "OrderBy_ThreadId" -> do
        pure OrderBy_ThreadId

      "OrderBy_Id" -> do
        pure OrderBy_Id

      "OrderBy_None" -> do
        pure OrderBy_None

      _ -> fail $ TypeMismatch "OrderBy" "Decode"



instance orderByEq :: Eq OrderBy where
  eq OrderBy_UserId OrderBy_UserId = true
  eq OrderBy_CreatedAt OrderBy_CreatedAt = true
  eq OrderBy_ModifiedAt OrderBy_ModifiedAt = true
  eq OrderBy_ModifiedBy OrderBy_ModifiedBy = true
  eq OrderBy_ActivityAt OrderBy_ActivityAt = true
  eq OrderBy_OrganizationId OrderBy_OrganizationId = true
  eq OrderBy_TeamId OrderBy_TeamId = true
  eq OrderBy_ForumId OrderBy_ForumId = true
  eq OrderBy_BoardId OrderBy_BoardId = true
  eq OrderBy_ThreadId OrderBy_ThreadId = true
  eq OrderBy_Id OrderBy_Id = true
  eq OrderBy_None OrderBy_None = true
  eq _ _ = false

instance orderByShow :: Show OrderBy where
  show OrderBy_UserId = "user_id"
  show OrderBy_CreatedAt = "created_at"
  show OrderBy_ModifiedAt = "modified_at"
  show OrderBy_ModifiedBy = "modified_by"
  show OrderBy_ActivityAt = "activity_at"
  show OrderBy_OrganizationId = "organization_id"
  show OrderBy_TeamId = "team_id"
  show OrderBy_ForumId = "forum_id"
  show OrderBy_BoardId = "board_id"
  show OrderBy_ThreadId = "thread_id"
  show OrderBy_Id = "id"
  show OrderBy_None = "none"


readOrderBy :: String -> Maybe OrderBy
readOrderBy "user_id" = Just OrderBy_UserId
readOrderBy "created_at" = Just OrderBy_CreatedAt
readOrderBy "modified_at" = Just OrderBy_ModifiedAt
readOrderBy "modified_by" = Just OrderBy_ModifiedBy
readOrderBy "activity_at" = Just OrderBy_ActivityAt
readOrderBy "organization_id" = Just OrderBy_OrganizationId
readOrderBy "team_id" = Just OrderBy_TeamId
readOrderBy "forum_id" = Just OrderBy_ForumId
readOrderBy "board_id" = Just OrderBy_BoardId
readOrderBy "thread_id" = Just OrderBy_ThreadId
readOrderBy "id" = Just OrderBy_Id
readOrderBy "none" = Just OrderBy_None
readOrderBy _ = Nothing
-- footer