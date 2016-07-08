module LN.T.ACL where
import LN.T.Permission


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

data ACL
  = ACL_Grant Permissions
  | ACL_Deny 



instance aCLEncodeJson :: EncodeJson ACL where
  encodeJson (ACL_Grant x0) =
       "tag" := "ACL_Grant"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (ACL_Deny ) =
       "tag" := "ACL_Deny"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance aCLDecodeJson :: DecodeJson ACL where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "ACL_Grant" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> ACL_Grant <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for ACL_Grant"


      "ACL_Deny" -> do
        pure ACL_Deny

      _ -> Left $ "DecodeJson TypeMismatch for ACL"



instance aCLRequestable :: Requestable ACL where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance aCLRespondable :: Respondable ACL where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "ACL_Grant" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> ACL_Grant <$> read x0
          _ -> Left $ TypeMismatch "ACL_Grant" "Respondable"


      "ACL_Deny" -> do
        pure ACL_Deny

      _ -> Left $ TypeMismatch "ACL" "Respondable"



instance aCLIsForeign :: IsForeign ACL where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "ACL_Grant" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> ACL_Grant <$> read x0
          _ -> Left $ TypeMismatch "ACL_Grant" "IsForeign"


      "ACL_Deny" -> do
        pure ACL_Deny

      _ -> Left $ TypeMismatch "ACL" "IsForeign"



instance aCLEq :: Eq ACL where
  eq (ACL_Grant x0a) (ACL_Grant x0b) = x0a == x0b
  eq ACL_Deny ACL_Deny = true
  eq _ _ = false
-- footer