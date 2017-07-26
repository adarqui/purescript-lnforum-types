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
  = Ent_Organization 
  | Ent_Team 
  | Ent_TeamMember 
  | Ent_GlobalGroup 
  | Ent_Group 
  | Ent_GroupMember 
  | Ent_User 
  | Ent_UserSanitized 
  | Ent_Forum 
  | Ent_Board 
  | Ent_Thread 
  | Ent_ThreadPost 
  | Ent_Blog 
  | Ent_BlogPost 
  | Ent_BlogComment 
  | Ent_Resource 
  | Ent_Leuron 
  | Ent_Comment 
  | Ent_Api 
  | Ent_Like 
  | Ent_Star 
  | Ent_None 



instance entEncodeJson :: EncodeJson Ent where
  encodeJson (Ent_Organization ) =
       "tag" := "Ent_Organization"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Ent_Team ) =
       "tag" := "Ent_Team"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Ent_TeamMember ) =
       "tag" := "Ent_TeamMember"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Ent_GlobalGroup ) =
       "tag" := "Ent_GlobalGroup"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Ent_Group ) =
       "tag" := "Ent_Group"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Ent_GroupMember ) =
       "tag" := "Ent_GroupMember"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
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
  encodeJson (Ent_Blog ) =
       "tag" := "Ent_Blog"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Ent_BlogPost ) =
       "tag" := "Ent_BlogPost"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Ent_BlogComment ) =
       "tag" := "Ent_BlogComment"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Ent_Resource ) =
       "tag" := "Ent_Resource"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Ent_Leuron ) =
       "tag" := "Ent_Leuron"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Ent_Comment ) =
       "tag" := "Ent_Comment"
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
  encodeJson (Ent_Star ) =
       "tag" := "Ent_Star"
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
      "Ent_Organization" -> do
        pure Ent_Organization

      "Ent_Team" -> do
        pure Ent_Team

      "Ent_TeamMember" -> do
        pure Ent_TeamMember

      "Ent_GlobalGroup" -> do
        pure Ent_GlobalGroup

      "Ent_Group" -> do
        pure Ent_Group

      "Ent_GroupMember" -> do
        pure Ent_GroupMember

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

      "Ent_Blog" -> do
        pure Ent_Blog

      "Ent_BlogPost" -> do
        pure Ent_BlogPost

      "Ent_BlogComment" -> do
        pure Ent_BlogComment

      "Ent_Resource" -> do
        pure Ent_Resource

      "Ent_Leuron" -> do
        pure Ent_Leuron

      "Ent_Comment" -> do
        pure Ent_Comment

      "Ent_Api" -> do
        pure Ent_Api

      "Ent_Like" -> do
        pure Ent_Like

      "Ent_Star" -> do
        pure Ent_Star

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
      "Ent_Organization" -> do
        pure Ent_Organization

      "Ent_Team" -> do
        pure Ent_Team

      "Ent_TeamMember" -> do
        pure Ent_TeamMember

      "Ent_GlobalGroup" -> do
        pure Ent_GlobalGroup

      "Ent_Group" -> do
        pure Ent_Group

      "Ent_GroupMember" -> do
        pure Ent_GroupMember

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

      "Ent_Blog" -> do
        pure Ent_Blog

      "Ent_BlogPost" -> do
        pure Ent_BlogPost

      "Ent_BlogComment" -> do
        pure Ent_BlogComment

      "Ent_Resource" -> do
        pure Ent_Resource

      "Ent_Leuron" -> do
        pure Ent_Leuron

      "Ent_Comment" -> do
        pure Ent_Comment

      "Ent_Api" -> do
        pure Ent_Api

      "Ent_Like" -> do
        pure Ent_Like

      "Ent_Star" -> do
        pure Ent_Star

      "Ent_None" -> do
        pure Ent_None

      _ -> fail $ TypeMismatch "Ent" "Respondable"



instance entEq :: Eq Ent where
  eq Ent_Organization Ent_Organization = true
  eq Ent_Team Ent_Team = true
  eq Ent_TeamMember Ent_TeamMember = true
  eq Ent_GlobalGroup Ent_GlobalGroup = true
  eq Ent_Group Ent_Group = true
  eq Ent_GroupMember Ent_GroupMember = true
  eq Ent_User Ent_User = true
  eq Ent_UserSanitized Ent_UserSanitized = true
  eq Ent_Forum Ent_Forum = true
  eq Ent_Board Ent_Board = true
  eq Ent_Thread Ent_Thread = true
  eq Ent_ThreadPost Ent_ThreadPost = true
  eq Ent_Blog Ent_Blog = true
  eq Ent_BlogPost Ent_BlogPost = true
  eq Ent_BlogComment Ent_BlogComment = true
  eq Ent_Resource Ent_Resource = true
  eq Ent_Leuron Ent_Leuron = true
  eq Ent_Comment Ent_Comment = true
  eq Ent_Api Ent_Api = true
  eq Ent_Like Ent_Like = true
  eq Ent_Star Ent_Star = true
  eq Ent_None Ent_None = true
  eq _ _ = false

readEnt :: String -> Maybe Ent
readEnt "organization" = Just Ent_Organization
readEnt "team" = Just Ent_Team
readEnt "team_member" = Just Ent_TeamMember
readEnt "global_group" = Just Ent_GlobalGroup
readEnt "group" = Just Ent_Group
readEnt "group_member" = Just Ent_GroupMember
readEnt "user" = Just Ent_User
readEnt "user_sanitized" = Just Ent_UserSanitized
readEnt "forum" = Just Ent_Forum
readEnt "board" = Just Ent_Board
readEnt "thread" = Just Ent_Thread
readEnt "thread_post" = Just Ent_ThreadPost
readEnt "blog" = Just Ent_Blog
readEnt "blog_post" = Just Ent_BlogPost
readEnt "blog_comment" = Just Ent_BlogComment
readEnt "resource" = Just Ent_Resource
readEnt "leuron" = Just Ent_Leuron
readEnt "comment" = Just Ent_Comment
readEnt "api" = Just Ent_Api
readEnt "like" = Just Ent_Like
readEnt "star" = Just Ent_Star
readEnt "none" = Just Ent_None
readEnt _ = Nothing
-- footer