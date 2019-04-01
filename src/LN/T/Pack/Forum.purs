module LN.T.Pack.Forum where
import LN.T.Forum
import LN.T.User
import LN.T.Permission
import LN.T.Like


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

newtype ForumPackResponse = ForumPackResponse {
  permissions :: Permissions,
  forum :: ForumResponse,
  stat :: ForumStatResponse
}


type ForumPackResponseR = {
  permissions :: Permissions,
  forum :: ForumResponse,
  stat :: ForumStatResponse
}


_ForumPackResponse :: Lens' ForumPackResponse {
  permissions :: Permissions,
  forum :: ForumResponse,
  stat :: ForumStatResponse
}
_ForumPackResponse f (ForumPackResponse o) = ForumPackResponse <$> f o


mkForumPackResponse :: Permissions -> ForumResponse -> ForumStatResponse -> ForumPackResponse
mkForumPackResponse permissions forum stat =
  ForumPackResponse{permissions, forum, stat}


unwrapForumPackResponse :: ForumPackResponse -> {
  permissions :: Permissions,
  forum :: ForumResponse,
  stat :: ForumStatResponse
}
unwrapForumPackResponse (ForumPackResponse r) = r

instance forumPackResponseEncodeJson :: EncodeJson ForumPackResponse where
  encodeJson (ForumPackResponse o) =
       "tag" := "ForumPackResponse"
    ~> "permissions" := o.permissions
    ~> "forum" := o.forum
    ~> "stat" := o.stat
    ~> jsonEmptyObject


instance forumPackResponseDecodeJson :: DecodeJson ForumPackResponse where
  decodeJson o = do
    obj <- decodeJson o
    permissions <- obj .? "permissions"
    forum <- obj .? "forum"
    stat <- obj .? "stat"
    pure $ ForumPackResponse {
      permissions,
      forum,
      stat
    }


instance forumPackResponseRequestable :: Requestable ForumPackResponse where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance forumPackResponseRespondable :: Respondable ForumPackResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse = fromResponseDecodeJson

-- footer