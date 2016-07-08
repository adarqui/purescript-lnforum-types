module LN.T.Membership where



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

data Membership
  = Membership_InviteOnly 
  | Membership_RequestInvite 
  | Membership_Join 
  | Membership_Locked 



instance membershipEncodeJson :: EncodeJson Membership where
  encodeJson (Membership_InviteOnly ) =
       "tag" := "Membership_InviteOnly"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Membership_RequestInvite ) =
       "tag" := "Membership_RequestInvite"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Membership_Join ) =
       "tag" := "Membership_Join"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Membership_Locked ) =
       "tag" := "Membership_Locked"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance membershipDecodeJson :: DecodeJson Membership where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Membership_InviteOnly" -> do
        pure Membership_InviteOnly

      "Membership_RequestInvite" -> do
        pure Membership_RequestInvite

      "Membership_Join" -> do
        pure Membership_Join

      "Membership_Locked" -> do
        pure Membership_Locked

      _ -> Left $ "DecodeJson TypeMismatch for Membership"



instance membershipRequestable :: Requestable Membership where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance membershipRespondable :: Respondable Membership where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "Membership_InviteOnly" -> do
        pure Membership_InviteOnly

      "Membership_RequestInvite" -> do
        pure Membership_RequestInvite

      "Membership_Join" -> do
        pure Membership_Join

      "Membership_Locked" -> do
        pure Membership_Locked

      _ -> Left $ TypeMismatch "Membership" "Respondable"



instance membershipIsForeign :: IsForeign Membership where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "Membership_InviteOnly" -> do
        pure Membership_InviteOnly

      "Membership_RequestInvite" -> do
        pure Membership_RequestInvite

      "Membership_Join" -> do
        pure Membership_Join

      "Membership_Locked" -> do
        pure Membership_Locked

      _ -> Left $ TypeMismatch "Membership" "IsForeign"



instance membershipEq :: Eq Membership where
  eq Membership_InviteOnly Membership_InviteOnly = true
  eq Membership_RequestInvite Membership_RequestInvite = true
  eq Membership_Join Membership_Join = true
  eq Membership_Locked Membership_Locked = true
  eq _ _ = false

readMembership :: String -> Maybe Membership
readMembership "invite_only" = Just Membership_InviteOnly
readMembership "request_invite" = Just Membership_RequestInvite
readMembership "join" = Just Membership_Join
readMembership "locked" = Just Membership_Locked
readMembership _ = Nothing
-- footer