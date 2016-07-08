module LN.T.Pack.User where
import LN.T.Profile
import LN.T.User


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
import Data.Default

import Purescript.Api.Helpers

newtype UserPackResponse = UserPackResponse {
  user :: UserResponse,
  userId :: Int,
  stat :: UserSanitizedStatResponse,
  profile :: ProfileResponse,
  profileId :: Int
}


type UserPackResponseR = {
  user :: UserResponse,
  userId :: Int,
  stat :: UserSanitizedStatResponse,
  profile :: ProfileResponse,
  profileId :: Int
}


_UserPackResponse :: Lens' UserPackResponse {
  user :: UserResponse,
  userId :: Int,
  stat :: UserSanitizedStatResponse,
  profile :: ProfileResponse,
  profileId :: Int
}
_UserPackResponse f (UserPackResponse o) = UserPackResponse <$> f o


mkUserPackResponse :: UserResponse -> Int -> UserSanitizedStatResponse -> ProfileResponse -> Int -> UserPackResponse
mkUserPackResponse user userId stat profile profileId =
  UserPackResponse{user, userId, stat, profile, profileId}


unwrapUserPackResponse :: UserPackResponse -> {
  user :: UserResponse,
  userId :: Int,
  stat :: UserSanitizedStatResponse,
  profile :: ProfileResponse,
  profileId :: Int
}
unwrapUserPackResponse (UserPackResponse r) = r

instance userPackResponseEncodeJson :: EncodeJson UserPackResponse where
  encodeJson (UserPackResponse o) =
       "tag" := "UserPackResponse"
    ~> "user" := o.user
    ~> "user_id" := o.userId
    ~> "stat" := o.stat
    ~> "profile" := o.profile
    ~> "profile_id" := o.profileId
    ~> jsonEmptyObject


instance userPackResponseDecodeJson :: DecodeJson UserPackResponse where
  decodeJson o = do
    obj <- decodeJson o
    user <- obj .? "user"
    userId <- obj .? "user_id"
    stat <- obj .? "stat"
    profile <- obj .? "profile"
    profileId <- obj .? "profile_id"
    pure $ UserPackResponse {
      user,
      userId,
      stat,
      profile,
      profileId
    }


instance userPackResponseRequestable :: Requestable UserPackResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance userPackResponseRespondable :: Respondable UserPackResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkUserPackResponse
      <$> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "stat" json
      <*> readProp "profile" json
      <*> readProp "profile_id" json


instance userPackResponseIsForeign :: IsForeign UserPackResponse where
  read json =
      mkUserPackResponse
      <$> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "stat" json
      <*> readProp "profile" json
      <*> readProp "profile_id" json


newtype UserPackResponses = UserPackResponses {
  userPackResponses :: (Array UserPackResponse)
}


type UserPackResponsesR = {
  userPackResponses :: (Array UserPackResponse)
}


_UserPackResponses :: Lens' UserPackResponses {
  userPackResponses :: (Array UserPackResponse)
}
_UserPackResponses f (UserPackResponses o) = UserPackResponses <$> f o


mkUserPackResponses :: (Array UserPackResponse) -> UserPackResponses
mkUserPackResponses userPackResponses =
  UserPackResponses{userPackResponses}


unwrapUserPackResponses :: UserPackResponses -> {
  userPackResponses :: (Array UserPackResponse)
}
unwrapUserPackResponses (UserPackResponses r) = r

instance userPackResponsesEncodeJson :: EncodeJson UserPackResponses where
  encodeJson (UserPackResponses o) =
       "tag" := "UserPackResponses"
    ~> "user_pack_responses" := o.userPackResponses
    ~> jsonEmptyObject


instance userPackResponsesDecodeJson :: DecodeJson UserPackResponses where
  decodeJson o = do
    obj <- decodeJson o
    userPackResponses <- obj .? "user_pack_responses"
    pure $ UserPackResponses {
      userPackResponses
    }


instance userPackResponsesRequestable :: Requestable UserPackResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance userPackResponsesRespondable :: Respondable UserPackResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkUserPackResponses
      <$> readProp "user_pack_responses" json


instance userPackResponsesIsForeign :: IsForeign UserPackResponses where
  read json =
      mkUserPackResponses
      <$> readProp "user_pack_responses" json

-- footer