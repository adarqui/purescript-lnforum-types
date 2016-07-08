module LN.T.Pack.GlobalGroup where


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

newtype GlobalGroupPackResponse = GlobalGroupPackResponse {
  user :: UserSanitizedResponse,
  userId :: Int,
  globalGroup :: GlobalGroupResponse,
  globalGroupId :: Int,
  stat :: GlobalGroupStatResponse,
  permissions :: Permissions
}


type GlobalGroupPackResponseR = {
  user :: UserSanitizedResponse,
  userId :: Int,
  globalGroup :: GlobalGroupResponse,
  globalGroupId :: Int,
  stat :: GlobalGroupStatResponse,
  permissions :: Permissions
}


_GlobalGroupPackResponse :: Lens' GlobalGroupPackResponse {
  user :: UserSanitizedResponse,
  userId :: Int,
  globalGroup :: GlobalGroupResponse,
  globalGroupId :: Int,
  stat :: GlobalGroupStatResponse,
  permissions :: Permissions
}
_GlobalGroupPackResponse f (GlobalGroupPackResponse o) = GlobalGroupPackResponse <$> f o


mkGlobalGroupPackResponse :: UserSanitizedResponse -> Int -> GlobalGroupResponse -> Int -> GlobalGroupStatResponse -> Permissions -> GlobalGroupPackResponse
mkGlobalGroupPackResponse user userId globalGroup globalGroupId stat permissions =
  GlobalGroupPackResponse{user, userId, globalGroup, globalGroupId, stat, permissions}


unwrapGlobalGroupPackResponse :: GlobalGroupPackResponse -> {
  user :: UserSanitizedResponse,
  userId :: Int,
  globalGroup :: GlobalGroupResponse,
  globalGroupId :: Int,
  stat :: GlobalGroupStatResponse,
  permissions :: Permissions
}
unwrapGlobalGroupPackResponse (GlobalGroupPackResponse r) = r

instance globalGroupPackResponseEncodeJson :: EncodeJson GlobalGroupPackResponse where
  encodeJson (GlobalGroupPackResponse o) =
       "tag" := "GlobalGroupPackResponse"
    ~> "user" := o.user
    ~> "user_id" := o.userId
    ~> "global_group" := o.globalGroup
    ~> "global_group_id" := o.globalGroupId
    ~> "stat" := o.stat
    ~> "permissions" := o.permissions
    ~> jsonEmptyObject


instance globalGroupPackResponseDecodeJson :: DecodeJson GlobalGroupPackResponse where
  decodeJson o = do
    obj <- decodeJson o
    user <- obj .? "user"
    userId <- obj .? "user_id"
    globalGroup <- obj .? "global_group"
    globalGroupId <- obj .? "global_group_id"
    stat <- obj .? "stat"
    permissions <- obj .? "permissions"
    pure $ GlobalGroupPackResponse {
      user,
      userId,
      globalGroup,
      globalGroupId,
      stat,
      permissions
    }


instance globalGroupPackResponseRequestable :: Requestable GlobalGroupPackResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance globalGroupPackResponseRespondable :: Respondable GlobalGroupPackResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkGlobalGroupPackResponse
      <$> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "global_group" json
      <*> readProp "global_group_id" json
      <*> readProp "stat" json
      <*> readProp "permissions" json


instance globalGroupPackResponseIsForeign :: IsForeign GlobalGroupPackResponse where
  read json =
      mkGlobalGroupPackResponse
      <$> readProp "user" json
      <*> readProp "user_id" json
      <*> readProp "global_group" json
      <*> readProp "global_group_id" json
      <*> readProp "stat" json
      <*> readProp "permissions" json


newtype GlobalGroupPackResponses = GlobalGroupPackResponses {
  globalGroupPackResponses :: (Array GlobalGroupPackResponse)
}


type GlobalGroupPackResponsesR = {
  globalGroupPackResponses :: (Array GlobalGroupPackResponse)
}


_GlobalGroupPackResponses :: Lens' GlobalGroupPackResponses {
  globalGroupPackResponses :: (Array GlobalGroupPackResponse)
}
_GlobalGroupPackResponses f (GlobalGroupPackResponses o) = GlobalGroupPackResponses <$> f o


mkGlobalGroupPackResponses :: (Array GlobalGroupPackResponse) -> GlobalGroupPackResponses
mkGlobalGroupPackResponses globalGroupPackResponses =
  GlobalGroupPackResponses{globalGroupPackResponses}


unwrapGlobalGroupPackResponses :: GlobalGroupPackResponses -> {
  globalGroupPackResponses :: (Array GlobalGroupPackResponse)
}
unwrapGlobalGroupPackResponses (GlobalGroupPackResponses r) = r

instance globalGroupPackResponsesEncodeJson :: EncodeJson GlobalGroupPackResponses where
  encodeJson (GlobalGroupPackResponses o) =
       "tag" := "GlobalGroupPackResponses"
    ~> "global_group_pack_responses" := o.globalGroupPackResponses
    ~> jsonEmptyObject


instance globalGroupPackResponsesDecodeJson :: DecodeJson GlobalGroupPackResponses where
  decodeJson o = do
    obj <- decodeJson o
    globalGroupPackResponses <- obj .? "global_group_pack_responses"
    pure $ GlobalGroupPackResponses {
      globalGroupPackResponses
    }


instance globalGroupPackResponsesRequestable :: Requestable GlobalGroupPackResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance globalGroupPackResponsesRespondable :: Respondable GlobalGroupPackResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkGlobalGroupPackResponses
      <$> readProp "global_group_pack_responses" json


instance globalGroupPackResponsesIsForeign :: IsForeign GlobalGroupPackResponses where
  read json =
      mkGlobalGroupPackResponses
      <$> readProp "global_group_pack_responses" json

-- footer