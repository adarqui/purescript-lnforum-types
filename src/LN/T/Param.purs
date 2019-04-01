module LN.T.Param where



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

data Param
  = Limit Int
  | Offset Int
  | SortOrder SortOrderBy
  | Order OrderBy
  | ByUserId Int
  | ByUsersIds (Array Int)
  | ByUserName String
  | ByUsersNames (Array String)
  | ByForumId Int
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
  | WithBoard Boolean
  | WithThread Boolean
  | WithThreadPosts Boolean



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
  encodeJson (ByForumId x0) =
       "tag" := "ByForumId"
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


      "ByForumId" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ByForumId <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ByForumId"


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
          [x0] -> Limit <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "Limit" "Respondable"


      "Offset" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> Offset <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "Offset" "Respondable"


      "SortOrder" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> SortOrder <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "SortOrder" "Respondable"


      "Order" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> Order <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "Order" "Respondable"


      "ByUserId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByUserId <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByUserId" "Respondable"


      "ByUsersIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByUsersIds <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByUsersIds" "Respondable"


      "ByUserName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByUserName <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByUserName" "Respondable"


      "ByUsersNames" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByUsersNames <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByUsersNames" "Respondable"


      "ByForumId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByForumId <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByForumId" "Respondable"


      "ByBoardId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByBoardId <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByBoardId" "Respondable"


      "ByBoardsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByBoardsIds <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByBoardsIds" "Respondable"


      "ByBoardName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByBoardName <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByBoardName" "Respondable"


      "ByThreadId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadId <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByThreadId" "Respondable"


      "ByThreadsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadsIds <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByThreadsIds" "Respondable"


      "ByThreadName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadName <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByThreadName" "Respondable"


      "ByThreadPostId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadPostId <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByThreadPostId" "Respondable"


      "ByThreadPostsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadPostsIds <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByThreadPostsIds" "Respondable"


      "ByThreadPostName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadPostName <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByThreadPostName" "Respondable"


      "ByThreadPostLikeId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadPostLikeId <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByThreadPostLikeId" "Respondable"


      "ByThreadPostLikesIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByThreadPostLikesIds <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByThreadPostLikesIds" "Respondable"


      "ByPmId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByPmId <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByPmId" "Respondable"


      "ByPmsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByPmsIds <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByPmsIds" "Respondable"


      "ByReminderId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByReminderId <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByReminderId" "Respondable"


      "ByReminderFolderId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByReminderFolderId <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByReminderFolderId" "Respondable"


      "ByParentId" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByParentId <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByParentId" "Respondable"


      "ByParentsIds" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByParentsIds <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByParentsIds" "Respondable"


      "ByParentName" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByParentName <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByParentName" "Respondable"


      "ByEmail" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> ByEmail <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "ByEmail" "Respondable"


      "BySelf" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> BySelf <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "BySelf" "Respondable"


      "View" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> View <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "View" "Respondable"


      "Timestamp" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> Timestamp <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "Timestamp" "Respondable"


      "UnixTimestamp" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> UnixTimestamp <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "UnixTimestamp" "Respondable"


      "CreatedAtTimestamp" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> CreatedAtTimestamp <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "CreatedAtTimestamp" "Respondable"


      "CreatedAtUnixTimestamp" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> CreatedAtUnixTimestamp <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "CreatedAtUnixTimestamp" "Respondable"


      "RealIP" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> RealIP <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "RealIP" "Respondable"


      "IP" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> IP <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "IP" "Respondable"


      "WithBoard" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> WithBoard <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "WithBoard" "Respondable"


      "WithThread" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> WithThread <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "WithThread" "Respondable"


      "WithThreadPosts" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> WithThreadPosts <$> exceptDecodeJsonRespondable x0
          _ -> fail $ TypeMismatch "WithThreadPosts" "Respondable"


      _ -> fail $ TypeMismatch "Param" "Respondable"



instance paramEq :: Eq Param where
  eq (Limit x0a) (Limit x0b) = x0a == x0b
  eq (Offset x0a) (Offset x0b) = x0a == x0b
  eq (SortOrder x0a) (SortOrder x0b) = x0a == x0b
  eq (Order x0a) (Order x0b) = x0a == x0b
  eq (ByUserId x0a) (ByUserId x0b) = x0a == x0b
  eq (ByUsersIds x0a) (ByUsersIds x0b) = x0a == x0b
  eq (ByUserName x0a) (ByUserName x0b) = x0a == x0b
  eq (ByUsersNames x0a) (ByUsersNames x0b) = x0a == x0b
  eq (ByForumId x0a) (ByForumId x0b) = x0a == x0b
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
  eq (WithBoard x0a) (WithBoard x0b) = x0a == x0b
  eq (WithThread x0a) (WithThread x0b) = x0a == x0b
  eq (WithThreadPosts x0a) (WithThreadPosts x0b) = x0a == x0b
  eq _ _ = false

instance paramShow :: Show Param where
  show (Limit x0) = "Limit: " <> show x0
  show (Offset x0) = "Offset: " <> show x0
  show (SortOrder x0) = "SortOrder: " <> show x0
  show (Order x0) = "Order: " <> show x0
  show (ByUserId x0) = "ByUserId: " <> show x0
  show (ByUsersIds x0) = "ByUsersIds: " <> show x0
  show (ByUserName x0) = "ByUserName: " <> show x0
  show (ByUsersNames x0) = "ByUsersNames: " <> show x0
  show (ByForumId x0) = "ByForumId: " <> show x0
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
  show (WithBoard x0) = "WithBoard: " <> show x0
  show (WithThread x0) = "WithThread: " <> show x0
  show (WithThreadPosts x0) = "WithThreadPosts: " <> show x0


instance paramQueryParam :: QueryParam Param where
  qp (Limit x0) = Tuple "limit" (show x0)
  qp (Offset x0) = Tuple "offset" (show x0)
  qp (SortOrder x0) = Tuple "sort_order" (show x0)
  qp (Order x0) = Tuple "order" (show x0)
  qp (ByUserId x0) = Tuple "by_user_id" (show x0)
  qp (ByUsersIds x0) = Tuple "by_users_ids" (show x0)
  qp (ByUserName x0) = Tuple "by_user_name" x0
  qp (ByUsersNames x0) = Tuple "by_users_names" (show x0)
  qp (ByForumId x0) = Tuple "by_forum_id" (show x0)
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
  qp (WithBoard x0) = Tuple "with_board" (show x0)
  qp (WithThread x0) = Tuple "with_thread" (show x0)
  qp (WithThreadPosts x0) = Tuple "with_thread_posts" (show x0)


data ParamTag
  = ParamTag_Limit 
  | ParamTag_Offset 
  | ParamTag_SortOrder 
  | ParamTag_Order 
  | ParamTag_ByUserId 
  | ParamTag_ByUsersIds 
  | ParamTag_ByUserName 
  | ParamTag_ByUsersNames 
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
  | ParamTag_WithBoard 
  | ParamTag_WithThread 
  | ParamTag_WithThreadPosts 



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

      "ParamTag_ByUserId" -> do
        pure ParamTag_ByUserId

      "ParamTag_ByUsersIds" -> do
        pure ParamTag_ByUsersIds

      "ParamTag_ByUserName" -> do
        pure ParamTag_ByUserName

      "ParamTag_ByUsersNames" -> do
        pure ParamTag_ByUsersNames

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

      "ParamTag_WithBoard" -> do
        pure ParamTag_WithBoard

      "ParamTag_WithThread" -> do
        pure ParamTag_WithThread

      "ParamTag_WithThreadPosts" -> do
        pure ParamTag_WithThreadPosts

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

      "ParamTag_ByUserId" -> do
        pure ParamTag_ByUserId

      "ParamTag_ByUsersIds" -> do
        pure ParamTag_ByUsersIds

      "ParamTag_ByUserName" -> do
        pure ParamTag_ByUserName

      "ParamTag_ByUsersNames" -> do
        pure ParamTag_ByUsersNames

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

      "ParamTag_WithBoard" -> do
        pure ParamTag_WithBoard

      "ParamTag_WithThread" -> do
        pure ParamTag_WithThread

      "ParamTag_WithThreadPosts" -> do
        pure ParamTag_WithThreadPosts

      _ -> fail $ TypeMismatch "ParamTag" "Respondable"



instance paramTagEq :: Eq ParamTag where
  eq ParamTag_Limit ParamTag_Limit = true
  eq ParamTag_Offset ParamTag_Offset = true
  eq ParamTag_SortOrder ParamTag_SortOrder = true
  eq ParamTag_Order ParamTag_Order = true
  eq ParamTag_ByUserId ParamTag_ByUserId = true
  eq ParamTag_ByUsersIds ParamTag_ByUsersIds = true
  eq ParamTag_ByUserName ParamTag_ByUserName = true
  eq ParamTag_ByUsersNames ParamTag_ByUsersNames = true
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
  eq ParamTag_WithBoard ParamTag_WithBoard = true
  eq ParamTag_WithThread ParamTag_WithThread = true
  eq ParamTag_WithThreadPosts ParamTag_WithThreadPosts = true
  eq _ _ = false

instance paramTagShow :: Show ParamTag where
  show ParamTag_Limit = "limit"
  show ParamTag_Offset = "offset"
  show ParamTag_SortOrder = "sort_order"
  show ParamTag_Order = "order"
  show ParamTag_ByUserId = "by_user_id"
  show ParamTag_ByUsersIds = "by_users_ids"
  show ParamTag_ByUserName = "by_user_name"
  show ParamTag_ByUsersNames = "by_users_names"
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
  show ParamTag_WithBoard = "with_board"
  show ParamTag_WithThread = "with_thread"
  show ParamTag_WithThreadPosts = "with_thread_posts"


readParamTag :: String -> Maybe ParamTag
readParamTag "limit" = Just ParamTag_Limit
readParamTag "offset" = Just ParamTag_Offset
readParamTag "sort_order" = Just ParamTag_SortOrder
readParamTag "order" = Just ParamTag_Order
readParamTag "by_user_id" = Just ParamTag_ByUserId
readParamTag "by_users_ids" = Just ParamTag_ByUsersIds
readParamTag "by_user_name" = Just ParamTag_ByUserName
readParamTag "by_users_names" = Just ParamTag_ByUsersNames
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
readParamTag "with_board" = Just ParamTag_WithBoard
readParamTag "with_thread" = Just ParamTag_WithThread
readParamTag "with_thread_posts" = Just ParamTag_WithThreadPosts
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

      "OrderBy_BoardId" -> do
        pure OrderBy_BoardId

      "OrderBy_ThreadId" -> do
        pure OrderBy_ThreadId

      "OrderBy_Id" -> do
        pure OrderBy_Id

      "OrderBy_None" -> do
        pure OrderBy_None

      _ -> fail $ TypeMismatch "OrderBy" "Respondable"



instance orderByEq :: Eq OrderBy where
  eq OrderBy_UserId OrderBy_UserId = true
  eq OrderBy_CreatedAt OrderBy_CreatedAt = true
  eq OrderBy_ModifiedAt OrderBy_ModifiedAt = true
  eq OrderBy_ModifiedBy OrderBy_ModifiedBy = true
  eq OrderBy_ActivityAt OrderBy_ActivityAt = true
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
readOrderBy "board_id" = Just OrderBy_BoardId
readOrderBy "thread_id" = Just OrderBy_ThreadId
readOrderBy "id" = Just OrderBy_Id
readOrderBy "none" = Just OrderBy_None
readOrderBy _ = Nothing
-- footer