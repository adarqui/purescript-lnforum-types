module LN.T.Permission where



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

data Permission
  = Perm_Create 
  | Perm_Read 
  | Perm_Update 
  | Perm_Delete 
  | Perm_Execute 



instance permissionEncodeJson :: EncodeJson Permission where
  encodeJson (Perm_Create ) =
       "tag" := "Perm_Create"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Perm_Read ) =
       "tag" := "Perm_Read"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Perm_Update ) =
       "tag" := "Perm_Update"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Perm_Delete ) =
       "tag" := "Perm_Delete"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Perm_Execute ) =
       "tag" := "Perm_Execute"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance permissionDecodeJson :: DecodeJson Permission where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Perm_Create" -> do
        pure Perm_Create

      "Perm_Read" -> do
        pure Perm_Read

      "Perm_Update" -> do
        pure Perm_Update

      "Perm_Delete" -> do
        pure Perm_Delete

      "Perm_Execute" -> do
        pure Perm_Execute

      _ -> Left $ "DecodeJson TypeMismatch for Permission"



instance permissionRequestable :: Requestable Permission where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance permissionRespondable :: Respondable Permission where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "Perm_Create" -> do
        pure Perm_Create

      "Perm_Read" -> do
        pure Perm_Read

      "Perm_Update" -> do
        pure Perm_Update

      "Perm_Delete" -> do
        pure Perm_Delete

      "Perm_Execute" -> do
        pure Perm_Execute

      _ -> Left $ TypeMismatch "Permission" "Respondable"



instance permissionIsForeign :: IsForeign Permission where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "Perm_Create" -> do
        pure Perm_Create

      "Perm_Read" -> do
        pure Perm_Read

      "Perm_Update" -> do
        pure Perm_Update

      "Perm_Delete" -> do
        pure Perm_Delete

      "Perm_Execute" -> do
        pure Perm_Execute

      _ -> Left $ TypeMismatch "Permission" "IsForeign"



instance permissionEq :: Eq Permission where
  eq Perm_Create Perm_Create = true
  eq Perm_Read Perm_Read = true
  eq Perm_Update Perm_Update = true
  eq Perm_Delete Perm_Delete = true
  eq Perm_Execute Perm_Execute = true
  eq _ _ = false

type Permissions  = (Array Permission)

-- footer