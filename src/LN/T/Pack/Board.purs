module LN.T.Pack.Board where
import LN.T.Permission
import LN.T.Organization
import LN.T.User
import LN.T.Forum
import LN.T.Board
import LN.T.Thread
import LN.T.ThreadPost
import LN.T.Like
import LN.T.Star


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

newtype BoardPackResponse = BoardPackResponse {
  board :: BoardResponse,
  boardId :: Int,
  stat :: BoardStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  latestThread :: (Maybe ThreadResponse),
  latestThreadPost :: (Maybe ThreadPostResponse),
  latestThreadPostUser :: (Maybe UserSanitizedResponse),
  withOrganization :: (Maybe OrganizationResponse),
  withForum :: (Maybe ForumResponse),
  permissions :: Permissions
}


type BoardPackResponseR = {
  board :: BoardResponse,
  boardId :: Int,
  stat :: BoardStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  latestThread :: (Maybe ThreadResponse),
  latestThreadPost :: (Maybe ThreadPostResponse),
  latestThreadPostUser :: (Maybe UserSanitizedResponse),
  withOrganization :: (Maybe OrganizationResponse),
  withForum :: (Maybe ForumResponse),
  permissions :: Permissions
}


_BoardPackResponse :: Lens' BoardPackResponse {
  board :: BoardResponse,
  boardId :: Int,
  stat :: BoardStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  latestThread :: (Maybe ThreadResponse),
  latestThreadPost :: (Maybe ThreadPostResponse),
  latestThreadPostUser :: (Maybe UserSanitizedResponse),
  withOrganization :: (Maybe OrganizationResponse),
  withForum :: (Maybe ForumResponse),
  permissions :: Permissions
}
_BoardPackResponse f (BoardPackResponse o) = BoardPackResponse <$> f o


mkBoardPackResponse :: BoardResponse -> Int -> BoardStatResponse -> (Maybe LikeResponse) -> (Maybe StarResponse) -> (Maybe ThreadResponse) -> (Maybe ThreadPostResponse) -> (Maybe UserSanitizedResponse) -> (Maybe OrganizationResponse) -> (Maybe ForumResponse) -> Permissions -> BoardPackResponse
mkBoardPackResponse board boardId stat like star latestThread latestThreadPost latestThreadPostUser withOrganization withForum permissions =
  BoardPackResponse{board, boardId, stat, like, star, latestThread, latestThreadPost, latestThreadPostUser, withOrganization, withForum, permissions}


unwrapBoardPackResponse :: BoardPackResponse -> {
  board :: BoardResponse,
  boardId :: Int,
  stat :: BoardStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  latestThread :: (Maybe ThreadResponse),
  latestThreadPost :: (Maybe ThreadPostResponse),
  latestThreadPostUser :: (Maybe UserSanitizedResponse),
  withOrganization :: (Maybe OrganizationResponse),
  withForum :: (Maybe ForumResponse),
  permissions :: Permissions
}
unwrapBoardPackResponse (BoardPackResponse r) = r

instance boardPackResponseEncodeJson :: EncodeJson BoardPackResponse where
  encodeJson (BoardPackResponse o) =
       "tag" := "BoardPackResponse"
    ~> "board" := o.board
    ~> "board_id" := o.boardId
    ~> "stat" := o.stat
    ~> "like" := o.like
    ~> "star" := o.star
    ~> "latest_thread" := o.latestThread
    ~> "latest_thread_post" := o.latestThreadPost
    ~> "latest_thread_post_user" := o.latestThreadPostUser
    ~> "with_organization" := o.withOrganization
    ~> "with_forum" := o.withForum
    ~> "permissions" := o.permissions
    ~> jsonEmptyObject


instance boardPackResponseDecodeJson :: DecodeJson BoardPackResponse where
  decodeJson o = do
    obj <- decodeJson o
    board <- obj .? "board"
    boardId <- obj .? "board_id"
    stat <- obj .? "stat"
    like <- obj .? "like"
    star <- obj .? "star"
    latestThread <- obj .? "latest_thread"
    latestThreadPost <- obj .? "latest_thread_post"
    latestThreadPostUser <- obj .? "latest_thread_post_user"
    withOrganization <- obj .? "with_organization"
    withForum <- obj .? "with_forum"
    permissions <- obj .? "permissions"
    pure $ BoardPackResponse {
      board,
      boardId,
      stat,
      like,
      star,
      latestThread,
      latestThreadPost,
      latestThreadPostUser,
      withOrganization,
      withForum,
      permissions
    }


instance boardPackResponseRequestable :: Requestable BoardPackResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance boardPackResponseRespondable :: Respondable BoardPackResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkBoardPackResponse
      <$> readProp "board" json
      <*> readProp "board_id" json
      <*> readProp "stat" json
      <*> (unNullOrUndefined <$> readProp "like" json)
      <*> (unNullOrUndefined <$> readProp "star" json)
      <*> (unNullOrUndefined <$> readProp "latest_thread" json)
      <*> (unNullOrUndefined <$> readProp "latest_thread_post" json)
      <*> (unNullOrUndefined <$> readProp "latest_thread_post_user" json)
      <*> (unNullOrUndefined <$> readProp "with_organization" json)
      <*> (unNullOrUndefined <$> readProp "with_forum" json)
      <*> readProp "permissions" json


instance boardPackResponseIsForeign :: IsForeign BoardPackResponse where
  read json =
      mkBoardPackResponse
      <$> readProp "board" json
      <*> readProp "board_id" json
      <*> readProp "stat" json
      <*> (unNullOrUndefined <$> readProp "like" json)
      <*> (unNullOrUndefined <$> readProp "star" json)
      <*> (unNullOrUndefined <$> readProp "latest_thread" json)
      <*> (unNullOrUndefined <$> readProp "latest_thread_post" json)
      <*> (unNullOrUndefined <$> readProp "latest_thread_post_user" json)
      <*> (unNullOrUndefined <$> readProp "with_organization" json)
      <*> (unNullOrUndefined <$> readProp "with_forum" json)
      <*> readProp "permissions" json


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
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance boardPackResponsesRespondable :: Respondable BoardPackResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkBoardPackResponses
      <$> readProp "board_pack_responses" json


instance boardPackResponsesIsForeign :: IsForeign BoardPackResponses where
  read json =
      mkBoardPackResponses
      <$> readProp "board_pack_responses" json

-- footer