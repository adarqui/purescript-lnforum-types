module LN.T.Size where



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
import Data.Foreign.Class               (class Decode, read, readProp)
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

data Size
  = XSmall 
  | Small 
  | Medium 
  | Large 
  | XLarge 



instance sizeEncodeJson :: EncodeJson Size where
  encodeJson (XSmall ) =
       "tag" := "XSmall"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Small ) =
       "tag" := "Small"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Medium ) =
       "tag" := "Medium"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Large ) =
       "tag" := "Large"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (XLarge ) =
       "tag" := "XLarge"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance sizeDecodeJson :: DecodeJson Size where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "XSmall" -> do
        pure XSmall

      "Small" -> do
        pure Small

      "Medium" -> do
        pure Medium

      "Large" -> do
        pure Large

      "XLarge" -> do
        pure XLarge

      _ -> Left $ "DecodeJson TypeMismatch for Size"



instance sizeRequestable :: Requestable Size where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance sizeRespondable :: Respondable Size where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "XSmall" -> do
        pure XSmall

      "Small" -> do
        pure Small

      "Medium" -> do
        pure Medium

      "Large" -> do
        pure Large

      "XLarge" -> do
        pure XLarge

      _ -> fail $ TypeMismatch "Size" "Respondable"



instance sizeDecode :: Decode Size where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "XSmall" -> do
        pure XSmall

      "Small" -> do
        pure Small

      "Medium" -> do
        pure Medium

      "Large" -> do
        pure Large

      "XLarge" -> do
        pure XLarge

      _ -> fail $ TypeMismatch "Size" "Decode"



instance sizeEq :: Eq Size where
  eq XSmall XSmall = true
  eq Small Small = true
  eq Medium Medium = true
  eq Large Large = true
  eq XLarge XLarge = true
  eq _ _ = false
-- footer