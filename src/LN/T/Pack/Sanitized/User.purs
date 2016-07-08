module LN.T.Pack.Sanitized.User where


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

newtype UserSanitizedPackResponse = UserSanitizedPackResponse {
  user :: UserSanitizedResponse,
  userId :: Int,
  profile :: ProfileResponse,
  profileId :: Int,
  stat :: UserSanitizedStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse)
}


type UserSanitizedPackResponseR = {
  user :: UserSanitizedResponse,
  userId :: Int,
  profile :: ProfileResponse,
  profileId :: Int,
  stat :: UserSanitizedStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse)
}


_UserSanitizedPackResponse :: Lens' UserSanitizedPackResponse {
  user :: UserSanitizedResponse,
  userId :: Int,
  profile :: ProfileResponse,
  profileId :: Int,
  stat :: UserSanitizedStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse)
}
_UserSanitizedPackResponse f (UserSanitizedPackResponse o) = UserSanitizedPackResponse <$> f o


mkUserSanitizedPackResponse :: UserSanitizedResponse -> Int -> ProfileResponse -> Int -> UserSanitizedStatResponse -> (Maybe LikeResponse) -> (Maybe StarResponse) -> UserSanitizedPackResponse
mkUserSanitizedPackResponse user userId profile profileId stat like star =
  UserSanitizedPackResponse{user, userId, profile, profileId, stat, like, star}


unwrapUserSanitizedPackResponse :: UserSanitizedPackResponse -> {
  user :: UserSanitizedResponse,
  userId :: Int,
  profile :: ProfileResponse,
  profileId :: Int,
  stat :: UserSanitizedStatResponse,
  like :: (Maybe LikeResponse),
  star :: (Maybe StarResponse)
}
unwrapUserSanitizedPackResponse (UserSanitizedPackResponse r) = r

instance userSanitizedPackResponseEncodeJson :: EncodeJson UserSanitizedPackResponse where
  encodeJson (UserSanitizedPackResponse o) =
       "tag" := "UserSanitizedPackResponse"
    ~> "user" := o.user
    ~> "user_id" := o.userId
    ~> "profile" := o.profile
    ~> "profile_id" := o.profileId
    ~> "stat" := o.stat
    ~> "like" := o.like
    ~> "star" := o.star
    ~> jsonEmptyObject


instance userSanitizedPackResponseDecodeJson :: DecodeJson UserSanitizedPackResponse where
  decodeJson o = do
    obj <- decodeJson o
    user <- obj .? "user"
    userId <- obj .? "user_id"
    profile <- obj .? "profile"
    profileId <- obj .? "profile_id"
    stat <- obj .? "stat"
    like <- obj .? "like"
    star <- obj .? "star"
    pure $ UserSanitizedPackResponse {
      user,
      userId,
      profile,
      profileId,
      stat,
      like,
      star
    }


instance userSanitizedPackResponseRequestable :: Requestable UserSanitizedPackResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance userSanitizedPackResponseRespondable :: Respondable UserSanitizedPackResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkUserSanitizedPackResponse
      <$> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "profile" json
      <*> readProp "profile_id" json
      <*> readProp "stat" json
      <*> (unNullOrUndefined <$> readProp "like" json)
      <*> (unNullOrUndefined <$> readProp "star" json)


instance userSanitizedPackResponseIsForeign :: IsForeign UserSanitizedPackResponse where
  read json =
      mkUserSanitizedPackResponse
      <$> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "profile" json
      <*> readProp "profile_id" json
      <*> readProp "stat" json
      <*> (unNullOrUndefined <$> readProp "like" json)
      <*> (unNullOrUndefined <$> readProp "star" json)


newtype UserSanitizedPackResponses = UserSanitizedPackResponses {
  userSanitizedPackResponses :: (Array UserSanitizedPackResponse)
}


type UserSanitizedPackResponsesR = {
  userSanitizedPackResponses :: (Array UserSanitizedPackResponse)
}


_UserSanitizedPackResponses :: Lens' UserSanitizedPackResponses {
  userSanitizedPackResponses :: (Array UserSanitizedPackResponse)
}
_UserSanitizedPackResponses f (UserSanitizedPackResponses o) = UserSanitizedPackResponses <$> f o


mkUserSanitizedPackResponses :: (Array UserSanitizedPackResponse) -> UserSanitizedPackResponses
mkUserSanitizedPackResponses userSanitizedPackResponses =
  UserSanitizedPackResponses{userSanitizedPackResponses}


unwrapUserSanitizedPackResponses :: UserSanitizedPackResponses -> {
  userSanitizedPackResponses :: (Array UserSanitizedPackResponse)
}
unwrapUserSanitizedPackResponses (UserSanitizedPackResponses r) = r

instance userSanitizedPackResponsesEncodeJson :: EncodeJson UserSanitizedPackResponses where
  encodeJson (UserSanitizedPackResponses o) =
       "tag" := "UserSanitizedPackResponses"
    ~> "user_sanitized_pack_responses" := o.userSanitizedPackResponses
    ~> jsonEmptyObject


instance userSanitizedPackResponsesDecodeJson :: DecodeJson UserSanitizedPackResponses where
  decodeJson o = do
    obj <- decodeJson o
    userSanitizedPackResponses <- obj .? "user_sanitized_pack_responses"
    pure $ UserSanitizedPackResponses {
      userSanitizedPackResponses
    }


instance userSanitizedPackResponsesRequestable :: Requestable UserSanitizedPackResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance userSanitizedPackResponsesRespondable :: Respondable UserSanitizedPackResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkUserSanitizedPackResponses
      <$> readProp "user_sanitized_pack_responses" json


instance userSanitizedPackResponsesIsForeign :: IsForeign UserSanitizedPackResponses where
  read json =
      mkUserSanitizedPackResponses
      <$> readProp "user_sanitized_pack_responses" json

-- footer