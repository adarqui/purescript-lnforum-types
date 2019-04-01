module LN.T.Pack.Boot where
import LN.T.Pack.User
import LN.T.Pack.Forum
import LN.T.Pack.Board
import LN.T.Pack.ThreadPost


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

newtype BootPackResponse = BootPackResponse {
  forumPack :: ForumPackResponse,
  boardPacks :: (Array BoardPackResponse),
  recentPosts :: (Array ThreadPostPackResponse),
  postsOfTheWeek :: (Array ThreadPostPackResponse),
  usersOnline :: (Array UserPackResponse)
}


type BootPackResponseR = {
  forumPack :: ForumPackResponse,
  boardPacks :: (Array BoardPackResponse),
  recentPosts :: (Array ThreadPostPackResponse),
  postsOfTheWeek :: (Array ThreadPostPackResponse),
  usersOnline :: (Array UserPackResponse)
}


_BootPackResponse :: Lens' BootPackResponse {
  forumPack :: ForumPackResponse,
  boardPacks :: (Array BoardPackResponse),
  recentPosts :: (Array ThreadPostPackResponse),
  postsOfTheWeek :: (Array ThreadPostPackResponse),
  usersOnline :: (Array UserPackResponse)
}
_BootPackResponse f (BootPackResponse o) = BootPackResponse <$> f o


mkBootPackResponse :: ForumPackResponse -> (Array BoardPackResponse) -> (Array ThreadPostPackResponse) -> (Array ThreadPostPackResponse) -> (Array UserPackResponse) -> BootPackResponse
mkBootPackResponse forumPack boardPacks recentPosts postsOfTheWeek usersOnline =
  BootPackResponse{forumPack, boardPacks, recentPosts, postsOfTheWeek, usersOnline}


unwrapBootPackResponse :: BootPackResponse -> {
  forumPack :: ForumPackResponse,
  boardPacks :: (Array BoardPackResponse),
  recentPosts :: (Array ThreadPostPackResponse),
  postsOfTheWeek :: (Array ThreadPostPackResponse),
  usersOnline :: (Array UserPackResponse)
}
unwrapBootPackResponse (BootPackResponse r) = r

instance bootPackResponseEncodeJson :: EncodeJson BootPackResponse where
  encodeJson (BootPackResponse o) =
       "tag" := "BootPackResponse"
    ~> "forum_pack" := o.forumPack
    ~> "board_packs" := o.boardPacks
    ~> "recent_posts" := o.recentPosts
    ~> "posts_of_the_week" := o.postsOfTheWeek
    ~> "users_online" := o.usersOnline
    ~> jsonEmptyObject


instance bootPackResponseDecodeJson :: DecodeJson BootPackResponse where
  decodeJson o = do
    obj <- decodeJson o
    forumPack <- obj .? "forum_pack"
    boardPacks <- obj .? "board_packs"
    recentPosts <- obj .? "recent_posts"
    postsOfTheWeek <- obj .? "posts_of_the_week"
    usersOnline <- obj .? "users_online"
    pure $ BootPackResponse {
      forumPack,
      boardPacks,
      recentPosts,
      postsOfTheWeek,
      usersOnline
    }


instance bootPackResponseRequestable :: Requestable BootPackResponse where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance bootPackResponseRespondable :: Respondable BootPackResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson

-- footer