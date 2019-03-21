module LN.T.Ent where



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

data Ent
  = Ent_User 
  | Ent_UserSanitized 
  | Ent_Forum 
  | Ent_Board 
  | Ent_Thread 
  | Ent_ThreadPost 
  | Ent_Api 
  | Ent_Like 
  | Ent_None 



instance entEncodeJson :: EncodeJson Ent where
  encodeJson (Ent_User ) =
       "tag" := "Ent_User"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Ent_UserSanitized ) =
       "tag" := "Ent_UserSanitized"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Ent_Forum ) =
       "tag" := "Ent_Forum"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Ent_Board ) =
       "tag" := "Ent_Board"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Ent_Thread ) =
       "tag" := "Ent_Thread"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Ent_ThreadPost ) =
       "tag" := "Ent_ThreadPost"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Ent_Api ) =
       "tag" := "Ent_Api"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Ent_Like ) =
       "tag" := "Ent_Like"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Ent_None ) =
       "tag" := "Ent_None"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance entDecodeJson :: DecodeJson Ent where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Ent_User" -> do
        pure Ent_User

      "Ent_UserSanitized" -> do
        pure Ent_UserSanitized

      "Ent_Forum" -> do
        pure Ent_Forum

      "Ent_Board" -> do
        pure Ent_Board

      "Ent_Thread" -> do
        pure Ent_Thread

      "Ent_ThreadPost" -> do
        pure Ent_ThreadPost

      "Ent_Api" -> do
        pure Ent_Api

      "Ent_Like" -> do
        pure Ent_Like

      "Ent_None" -> do
        pure Ent_None

      _ -> Left $ "DecodeJson TypeMismatch for Ent"



instance entRequestable :: Requestable Ent where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance entRespondable :: Respondable Ent where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "Ent_User" -> do
        pure Ent_User

      "Ent_UserSanitized" -> do
        pure Ent_UserSanitized

      "Ent_Forum" -> do
        pure Ent_Forum

      "Ent_Board" -> do
        pure Ent_Board

      "Ent_Thread" -> do
        pure Ent_Thread

      "Ent_ThreadPost" -> do
        pure Ent_ThreadPost

      "Ent_Api" -> do
        pure Ent_Api

      "Ent_Like" -> do
        pure Ent_Like

      "Ent_None" -> do
        pure Ent_None

      _ -> fail $ TypeMismatch "Ent" "Respondable"



instance entEq :: Eq Ent where
  eq Ent_User Ent_User = true
  eq Ent_UserSanitized Ent_UserSanitized = true
  eq Ent_Forum Ent_Forum = true
  eq Ent_Board Ent_Board = true
  eq Ent_Thread Ent_Thread = true
  eq Ent_ThreadPost Ent_ThreadPost = true
  eq Ent_Api Ent_Api = true
  eq Ent_Like Ent_Like = true
  eq Ent_None Ent_None = true
  eq _ _ = false

readEnt :: String -> Maybe Ent
readEnt "user" = Just Ent_User
readEnt "user_sanitized" = Just Ent_UserSanitized
readEnt "forum" = Just Ent_Forum
readEnt "board" = Just Ent_Board
readEnt "thread" = Just Ent_Thread
readEnt "thread_post" = Just Ent_ThreadPost
readEnt "api" = Just Ent_Api
readEnt "like" = Just Ent_Like
readEnt "none" = Just Ent_None
readEnt _ = Nothing
-- footer