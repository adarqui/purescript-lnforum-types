module LN.T.Pack.Forum where
import LN.T.Forum
import LN.T.User
import LN.T.Permission
import LN.T.Organization
import LN.T.Star
import LN.T.Like


import Data.Argonaut.Core               (jsonEmptyObject)
import Data.Argonaut.Decode             (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode             (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Argonaut.Printer            (printJson)
import Data.Date.Helpers                (Date)
import Data.Either                      (Either(..))
import Data.Foreign                     (ForeignError(..), fail)
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
import Data.Default

import Purescript.Api.Helpers

newtype ForumPackResponse = ForumPackResponse {
  forum :: ForumResponse,
  forumId :: Int,
  stat :: ForumStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  withOrganization :: (Maybe OrganizationResponse),
  permissions :: Permissions
}


type ForumPackResponseR = {
  forum :: ForumResponse,
  forumId :: Int,
  stat :: ForumStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  withOrganization :: (Maybe OrganizationResponse),
  permissions :: Permissions
}


_ForumPackResponse :: Lens' ForumPackResponse {
  forum :: ForumResponse,
  forumId :: Int,
  stat :: ForumStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  withOrganization :: (Maybe OrganizationResponse),
  permissions :: Permissions
}
_ForumPackResponse f (ForumPackResponse o) = ForumPackResponse <$> f o


mkForumPackResponse :: ForumResponse -> Int -> ForumStatResponse -> (Maybe LikeResponse) -> (Maybe StarResponse) -> (Maybe OrganizationResponse) -> Permissions -> ForumPackResponse
mkForumPackResponse forum forumId stat like star withOrganization permissions =
  ForumPackResponse{forum, forumId, stat, like, star, withOrganization, permissions}


unwrapForumPackResponse :: ForumPackResponse -> {
  forum :: ForumResponse,
  forumId :: Int,
  stat :: ForumStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse),
  withOrganization :: (Maybe OrganizationResponse),
  permissions :: Permissions
}
unwrapForumPackResponse (ForumPackResponse r) = r

instance forumPackResponseEncodeJson :: EncodeJson ForumPackResponse where
  encodeJson (ForumPackResponse o) =
       "tag" := "ForumPackResponse"
    ~> "forum" := o.forum
    ~> "forum_id" := o.forumId
    ~> "stat" := o.stat
    ~> "like" := o.like
    ~> "star" := o.star
    ~> "with_organization" := o.withOrganization
    ~> "permissions" := o.permissions
    ~> jsonEmptyObject


instance forumPackResponseDecodeJson :: DecodeJson ForumPackResponse where
  decodeJson o = do
    obj <- decodeJson o
    forum <- obj .? "forum"
    forumId <- obj .? "forum_id"
    stat <- obj .? "stat"
    like <- obj .? "like"
    star <- obj .? "star"
    withOrganization <- obj .? "with_organization"
    permissions <- obj .? "permissions"
    pure $ ForumPackResponse {
      forum,
      forumId,
      stat,
      like,
      star,
      withOrganization,
      permissions
    }


instance forumPackResponseRequestable :: Requestable ForumPackResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance forumPackResponseRespondable :: Respondable ForumPackResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkForumPackResponse
      <$> readProp "forum" json
      <*> readProp "forum_id" json
      <*> readProp "stat" json
      <*> (unNullOrUndefined <$> readProp "like" json)
      <*> (unNullOrUndefined <$> readProp "star" json)
      <*> (unNullOrUndefined <$> readProp "with_organization" json)
      <*> readProp "permissions" json


instance forumPackResponseIsForeign :: IsForeign ForumPackResponse where
  read json =
      mkForumPackResponse
      <$> readProp "forum" json
      <*> readProp "forum_id" json
      <*> readProp "stat" json
      <*> (unNullOrUndefined <$> readProp "like" json)
      <*> (unNullOrUndefined <$> readProp "star" json)
      <*> (unNullOrUndefined <$> readProp "with_organization" json)
      <*> readProp "permissions" json


newtype ForumPackResponses = ForumPackResponses {
  forumPackResponses :: (Array ForumPackResponse)
}


type ForumPackResponsesR = {
  forumPackResponses :: (Array ForumPackResponse)
}


_ForumPackResponses :: Lens' ForumPackResponses {
  forumPackResponses :: (Array ForumPackResponse)
}
_ForumPackResponses f (ForumPackResponses o) = ForumPackResponses <$> f o


mkForumPackResponses :: (Array ForumPackResponse) -> ForumPackResponses
mkForumPackResponses forumPackResponses =
  ForumPackResponses{forumPackResponses}


unwrapForumPackResponses :: ForumPackResponses -> {
  forumPackResponses :: (Array ForumPackResponse)
}
unwrapForumPackResponses (ForumPackResponses r) = r

instance forumPackResponsesEncodeJson :: EncodeJson ForumPackResponses where
  encodeJson (ForumPackResponses o) =
       "tag" := "ForumPackResponses"
    ~> "forum_pack_responses" := o.forumPackResponses
    ~> jsonEmptyObject


instance forumPackResponsesDecodeJson :: DecodeJson ForumPackResponses where
  decodeJson o = do
    obj <- decodeJson o
    forumPackResponses <- obj .? "forum_pack_responses"
    pure $ ForumPackResponses {
      forumPackResponses
    }


instance forumPackResponsesRequestable :: Requestable ForumPackResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance forumPackResponsesRespondable :: Respondable ForumPackResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkForumPackResponses
      <$> readProp "forum_pack_responses" json


instance forumPackResponsesIsForeign :: IsForeign ForumPackResponses where
  read json =
      mkForumPackResponses
      <$> readProp "forum_pack_responses" json

-- footer