module LN.T.Pack.Board where
import LN.T.Board
import LN.T.User
import LN.T.Permission
import LN.T.Like
import LN.T.Thread
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

newtype BoardPackResponse = BoardPackResponse {
  board :: BoardResponse,
  boardId :: Int,
  user :: (Maybe UserSanitizedResponse),
  userId :: (Maybe Int),
  stat :: BoardStatResponse,
  like :: (Maybe LikeResponse),
  permissions :: Permissions,
  latestThread :: (Maybe ThreadResponse),
  latestThreadPost :: (Maybe ThreadPostResponse),
  latestThreadPostUser :: (Maybe UserSanitizedResponse)
}


type BoardPackResponseR = {
  board :: BoardResponse,
  boardId :: Int,
  user :: (Maybe UserSanitizedResponse),
  userId :: (Maybe Int),
  stat :: BoardStatResponse,
  like :: (Maybe LikeResponse),
  permissions :: Permissions,
  latestThread :: (Maybe ThreadResponse),
  latestThreadPost :: (Maybe ThreadPostResponse),
  latestThreadPostUser :: (Maybe UserSanitizedResponse)
}


_BoardPackResponse :: Lens' BoardPackResponse {
  board :: BoardResponse,
  boardId :: Int,
  user :: (Maybe UserSanitizedResponse),
  userId :: (Maybe Int),
  stat :: BoardStatResponse,
  like :: (Maybe LikeResponse),
  permissions :: Permissions,
  latestThread :: (Maybe ThreadResponse),
  latestThreadPost :: (Maybe ThreadPostResponse),
  latestThreadPostUser :: (Maybe UserSanitizedResponse)
}
_BoardPackResponse f (BoardPackResponse o) = BoardPackResponse <$> f o


mkBoardPackResponse :: BoardResponse -> Int -> (Maybe UserSanitizedResponse) -> (Maybe Int) -> BoardStatResponse -> (Maybe LikeResponse) -> Permissions -> (Maybe ThreadResponse) -> (Maybe ThreadPostResponse) -> (Maybe UserSanitizedResponse) -> BoardPackResponse
mkBoardPackResponse board boardId user userId stat like permissions latestThread latestThreadPost latestThreadPostUser =
  BoardPackResponse{board, boardId, user, userId, stat, like, permissions, latestThread, latestThreadPost, latestThreadPostUser}


unwrapBoardPackResponse :: BoardPackResponse -> {
  board :: BoardResponse,
  boardId :: Int,
  user :: (Maybe UserSanitizedResponse),
  userId :: (Maybe Int),
  stat :: BoardStatResponse,
  like :: (Maybe LikeResponse),
  permissions :: Permissions,
  latestThread :: (Maybe ThreadResponse),
  latestThreadPost :: (Maybe ThreadPostResponse),
  latestThreadPostUser :: (Maybe UserSanitizedResponse)
}
unwrapBoardPackResponse (BoardPackResponse r) = r

instance boardPackResponseEncodeJson :: EncodeJson BoardPackResponse where
  encodeJson (BoardPackResponse o) =
       "tag" := "BoardPackResponse"
    ~> "board" := o.board
    ~> "board_id" := o.boardId
    ~> "user" := o.user
    ~> "user_id" := o.userId
    ~> "stat" := o.stat
    ~> "like" := o.like
    ~> "permissions" := o.permissions
    ~> "latest_thread" := o.latestThread
    ~> "latest_thread_post" := o.latestThreadPost
    ~> "latest_thread_post_user" := o.latestThreadPostUser
    ~> jsonEmptyObject


instance boardPackResponseDecodeJson :: DecodeJson BoardPackResponse where
  decodeJson o = do
    obj <- decodeJson o
    board <- obj .? "board"
    boardId <- obj .? "board_id"
    user <- obj .? "user"
    userId <- obj .? "user_id"
    stat <- obj .? "stat"
    like <- obj .? "like"
    permissions <- obj .? "permissions"
    latestThread <- obj .? "latest_thread"
    latestThreadPost <- obj .? "latest_thread_post"
    latestThreadPostUser <- obj .? "latest_thread_post_user"
    pure $ BoardPackResponse {
      board,
      boardId,
      user,
      userId,
      stat,
      like,
      permissions,
      latestThread,
      latestThreadPost,
      latestThreadPostUser
    }


instance boardPackResponseRequestable :: Requestable BoardPackResponse where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance boardPackResponseRespondable :: Respondable BoardPackResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson


newtype BoardPackResponses = BoardPackResponses {
  boardPackResponses :: (Array BoardPackResponse)
}


type BoardPackResponsesR = {
  boardPackResponses :: (Array BoardPackResponse)
}


_BoardPackResponses :: Lens' BoardPackResponses {
  boardPackResponses :: (Array BoardPackResponse)
}
_BoardPackResponses f (BoardPackResponses o) = BoardPackResponses <$> f o


mkBoardPackResponses :: (Array BoardPackResponse) -> BoardPackResponses
mkBoardPackResponses boardPackResponses =
  BoardPackResponses{boardPackResponses}


unwrapBoardPackResponses :: BoardPackResponses -> {
  boardPackResponses :: (Array BoardPackResponse)
}
unwrapBoardPackResponses (BoardPackResponses r) = r

instance boardPackResponsesEncodeJson :: EncodeJson BoardPackResponses where
  encodeJson (BoardPackResponses o) =
       "tag" := "BoardPackResponses"
    ~> "board_pack_responses" := o.boardPackResponses
    ~> jsonEmptyObject


instance boardPackResponsesDecodeJson :: DecodeJson BoardPackResponses where
  decodeJson o = do
    obj <- decodeJson o
    boardPackResponses <- obj .? "board_pack_responses"
    pure $ BoardPackResponses {
      boardPackResponses
    }


instance boardPackResponsesRequestable :: Requestable BoardPackResponses where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance boardPackResponsesRespondable :: Respondable BoardPackResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson

-- footer