module LN.T.Visibility where



import Data.Argonaut.Core               (jsonEmptyObject, stringify)
import Data.Argonaut.Decode             (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode             (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
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

data Visibility
  = Public 
  | Private 



instance visibilityEncodeJson :: EncodeJson Visibility where
  encodeJson (Public ) =
       "tag" := "Public"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Private ) =
       "tag" := "Private"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance visibilityDecodeJson :: DecodeJson Visibility where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Public" -> do
        pure Public

      "Private" -> do
        pure Private

      _ -> Left $ "DecodeJson TypeMismatch for Visibility"



instance visibilityRequestable :: Requestable Visibility where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance visibilityRespondable :: Respondable Visibility where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "Public" -> do
        pure Public

      "Private" -> do
        pure Private

      _ -> fail $ TypeMismatch "Visibility" "Respondable"



instance visibilityDecode :: Decode Visibility where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "Public" -> do
        pure Public

      "Private" -> do
        pure Private

      _ -> fail $ TypeMismatch "Visibility" "Decode"



instance visibilityEq :: Eq Visibility where
  eq Public Public = true
  eq Private Private = true
  eq _ _ = false

readVisibility :: String -> Maybe Visibility
readVisibility "public" = Just Public
readVisibility "private" = Just Private
readVisibility _ = Nothing

instance visibilityShow :: Show Visibility where
  show Public = "public"
  show Private = "private"

-- footer