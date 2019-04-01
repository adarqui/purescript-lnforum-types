module LN.T.Pack.Thread where
import LN.T.Thread
import LN.T.User
import LN.T.Permission
import LN.T.Like
import LN.T.Board
import LN.T.ThreadPost


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

newtype ThreadPackResponse = ThreadPackResponse {
  thread :: ThreadResponse,
  threadId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int,
  stat :: ThreadStatResponse,
  like :: (Maybe LikeResponse),
  latestThreadPost :: (Maybe ThreadPostResponse),
  latestThreadPostUser :: (Maybe UserSanitizedResponse),
  withBoard :: (Maybe BoardResponse),
  permissions :: Permissions
}


type ThreadPackResponseR = {
  thread :: ThreadResponse,
  threadId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int,
  stat :: ThreadStatResponse,
  like :: (Maybe LikeResponse),
  latestThreadPost :: (Maybe ThreadPostResponse),
  latestThreadPostUser :: (Maybe UserSanitizedResponse),
  withBoard :: (Maybe BoardResponse),
  permissions :: Permissions
}


_ThreadPackResponse :: Lens' ThreadPackResponse {
  thread :: ThreadResponse,
  threadId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int,
  stat :: ThreadStatResponse,
  like :: (Maybe LikeResponse),
  latestThreadPost :: (Maybe ThreadPostResponse),
  latestThreadPostUser :: (Maybe UserSanitizedResponse),
  withBoard :: (Maybe BoardResponse),
  permissions :: Permissions
}
_ThreadPackResponse f (ThreadPackResponse o) = ThreadPackResponse <$> f o


mkThreadPackResponse :: ThreadResponse -> Int -> UserSanitizedResponse -> Int -> ThreadStatResponse -> (Maybe LikeResponse) -> (Maybe ThreadPostResponse) -> (Maybe UserSanitizedResponse) -> (Maybe BoardResponse) -> Permissions -> ThreadPackResponse
mkThreadPackResponse thread threadId user userId stat like latestThreadPost latestThreadPostUser withBoard permissions =
  ThreadPackResponse{thread, threadId, user, userId, stat, like, latestThreadPost, latestThreadPostUser, withBoard, permissions}


unwrapThreadPackResponse :: ThreadPackResponse -> {
  thread :: ThreadResponse,
  threadId :: Int,
  user :: UserSanitizedResponse,
  userId :: Int,
  stat :: ThreadStatResponse,
  like :: (Maybe LikeResponse),
  latestThreadPost :: (Maybe ThreadPostResponse),
  latestThreadPostUser :: (Maybe UserSanitizedResponse),
  withBoard :: (Maybe BoardResponse),
  permissions :: Permissions
}
unwrapThreadPackResponse (ThreadPackResponse r) = r

instance threadPackResponseEncodeJson :: EncodeJson ThreadPackResponse where
  encodeJson (ThreadPackResponse o) =
       "tag" := "ThreadPackResponse"
    ~> "thread" := o.thread
    ~> "thread_id" := o.threadId
    ~> "user" := o.user
    ~> "user_id" := o.userId
    ~> "stat" := o.stat
    ~> "like" := o.like
    ~> "latest_thread_post" := o.latestThreadPost
    ~> "latest_thread_post_user" := o.latestThreadPostUser
    ~> "with_board" := o.withBoard
    ~> "permissions" := o.permissions
    ~> jsonEmptyObject


instance threadPackResponseDecodeJson :: DecodeJson ThreadPackResponse where
  decodeJson o = do
    obj <- decodeJson o
    thread <- obj .? "thread"
    threadId <- obj .? "thread_id"
    user <- obj .? "user"
    userId <- obj .? "user_id"
    stat <- obj .? "stat"
    like <- obj .? "like"
    latestThreadPost <- obj .? "latest_thread_post"
    latestThreadPostUser <- obj .? "latest_thread_post_user"
    withBoard <- obj .? "with_board"
    permissions <- obj .? "permissions"
    pure $ ThreadPackResponse {
      thread,
      threadId,
      user,
      userId,
      stat,
      like,
      latestThreadPost,
      latestThreadPostUser,
      withBoard,
      permissions
    }


instance threadPackResponseRequestable :: Requestable ThreadPackResponse where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance threadPackResponseRespondable :: Respondable ThreadPackResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype ThreadPackResponses = ThreadPackResponses {
  threadPackResponses :: (Array ThreadPackResponse)
}


type ThreadPackResponsesR = {
  threadPackResponses :: (Array ThreadPackResponse)
}


_ThreadPackResponses :: Lens' ThreadPackResponses {
  threadPackResponses :: (Array ThreadPackResponse)
}
_ThreadPackResponses f (ThreadPackResponses o) = ThreadPackResponses <$> f o


mkThreadPackResponses :: (Array ThreadPackResponse) -> ThreadPackResponses
mkThreadPackResponses threadPackResponses =
  ThreadPackResponses{threadPackResponses}


unwrapThreadPackResponses :: ThreadPackResponses -> {
  threadPackResponses :: (Array ThreadPackResponse)
}
unwrapThreadPackResponses (ThreadPackResponses r) = r

instance threadPackResponsesEncodeJson :: EncodeJson ThreadPackResponses where
  encodeJson (ThreadPackResponses o) =
       "tag" := "ThreadPackResponses"
    ~> "thread_pack_responses" := o.threadPackResponses
    ~> jsonEmptyObject


instance threadPackResponsesDecodeJson :: DecodeJson ThreadPackResponses where
  decodeJson o = do
    obj <- decodeJson o
    threadPackResponses <- obj .? "thread_pack_responses"
    pure $ ThreadPackResponses {
      threadPackResponses
    }


instance threadPackResponsesRequestable :: Requestable ThreadPackResponses where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance threadPackResponsesRespondable :: Respondable ThreadPackResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson

-- footer