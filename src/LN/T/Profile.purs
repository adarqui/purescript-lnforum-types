module LN.T.Profile where
import LN.T.Ent


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

newtype ProfileX = ProfileX {
  profileLogin :: String,
  profileName :: String,
  profileEmail :: String
}


type ProfileXR = {
  profileLogin :: String,
  profileName :: String,
  profileEmail :: String
}


_ProfileX :: Lens' ProfileX {
  profileLogin :: String,
  profileName :: String,
  profileEmail :: String
}
_ProfileX f (ProfileX o) = ProfileX <$> f o


mkProfileX :: String -> String -> String -> ProfileX
mkProfileX profileLogin profileName profileEmail =
  ProfileX{profileLogin, profileName, profileEmail}


unwrapProfileX :: ProfileX -> {
  profileLogin :: String,
  profileName :: String,
  profileEmail :: String
}
unwrapProfileX (ProfileX r) = r

instance profileXEncodeJson :: EncodeJson ProfileX where
  encodeJson (ProfileX o) =
       "tag" := "ProfileX"
    ~> "profile_login" := o.profileLogin
    ~> "profile_name" := o.profileName
    ~> "profile_email" := o.profileEmail
    ~> jsonEmptyObject


instance profileXDecodeJson :: DecodeJson ProfileX where
  decodeJson o = do
    obj <- decodeJson o
    profileLogin <- obj .? "profile_login"
    profileName <- obj .? "profile_name"
    profileEmail <- obj .? "profile_email"
    pure $ ProfileX {
      profileLogin,
      profileName,
      profileEmail
    }


instance profileXRequestable :: Requestable ProfileX where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance profileXRespondable :: Respondable ProfileX where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkProfileX
      <$> readProp "profile_login" json
      <*> readProp "profile_name" json
      <*> readProp "profile_email" json


instance profileXDecode :: Decode ProfileX where
  read json =
      mkProfileX
      <$> readProp "profile_login" json
      <*> readProp "profile_name" json
      <*> readProp "profile_email" json


data ProfileGender
  = GenderMale 
  | GenderFemale 
  | GenderUnknown 



instance profileGenderEncodeJson :: EncodeJson ProfileGender where
  encodeJson (GenderMale ) =
       "tag" := "GenderMale"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (GenderFemale ) =
       "tag" := "GenderFemale"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (GenderUnknown ) =
       "tag" := "GenderUnknown"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance profileGenderDecodeJson :: DecodeJson ProfileGender where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "GenderMale" -> do
        pure GenderMale

      "GenderFemale" -> do
        pure GenderFemale

      "GenderUnknown" -> do
        pure GenderUnknown

      _ -> Left $ "DecodeJson TypeMismatch for ProfileGender"



instance profileGenderRequestable :: Requestable ProfileGender where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance profileGenderRespondable :: Respondable ProfileGender where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "GenderMale" -> do
        pure GenderMale

      "GenderFemale" -> do
        pure GenderFemale

      "GenderUnknown" -> do
        pure GenderUnknown

      _ -> fail $ TypeMismatch "ProfileGender" "Respondable"



instance profileGenderDecode :: Decode ProfileGender where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "GenderMale" -> do
        pure GenderMale

      "GenderFemale" -> do
        pure GenderFemale

      "GenderUnknown" -> do
        pure GenderUnknown

      _ -> fail $ TypeMismatch "ProfileGender" "Decode"



instance profileGenderEq :: Eq ProfileGender where
  eq GenderMale GenderMale = true
  eq GenderFemale GenderFemale = true
  eq GenderUnknown GenderUnknown = true
  eq _ _ = false

readProfileGender :: String -> Maybe ProfileGender
readProfileGender "gender_male" = Just GenderMale
readProfileGender "gender_female" = Just GenderFemale
readProfileGender "gender_unknown" = Just GenderUnknown
readProfileGender _ = Nothing

newtype ProfileRequest = ProfileRequest {
  gender :: ProfileGender,
  birthdate :: Date,
  website :: (Maybe String),
  websites :: (Array String),
  location :: (Maybe String),
  signature :: (Maybe String),
  debug :: Boolean,
  guard :: Int,
  stateWebsites :: (Maybe String)
}


type ProfileRequestR = {
  gender :: ProfileGender,
  birthdate :: Date,
  website :: (Maybe String),
  websites :: (Array String),
  location :: (Maybe String),
  signature :: (Maybe String),
  debug :: Boolean,
  guard :: Int,
  stateWebsites :: (Maybe String)
}


_ProfileRequest :: Lens' ProfileRequest {
  gender :: ProfileGender,
  birthdate :: Date,
  website :: (Maybe String),
  websites :: (Array String),
  location :: (Maybe String),
  signature :: (Maybe String),
  debug :: Boolean,
  guard :: Int,
  stateWebsites :: (Maybe String)
}
_ProfileRequest f (ProfileRequest o) = ProfileRequest <$> f o


mkProfileRequest :: ProfileGender -> Date -> (Maybe String) -> (Array String) -> (Maybe String) -> (Maybe String) -> Boolean -> Int -> (Maybe String) -> ProfileRequest
mkProfileRequest gender birthdate website websites location signature debug guard stateWebsites =
  ProfileRequest{gender, birthdate, website, websites, location, signature, debug, guard, stateWebsites}


unwrapProfileRequest :: ProfileRequest -> {
  gender :: ProfileGender,
  birthdate :: Date,
  website :: (Maybe String),
  websites :: (Array String),
  location :: (Maybe String),
  signature :: (Maybe String),
  debug :: Boolean,
  guard :: Int,
  stateWebsites :: (Maybe String)
}
unwrapProfileRequest (ProfileRequest r) = r

instance profileRequestEncodeJson :: EncodeJson ProfileRequest where
  encodeJson (ProfileRequest o) =
       "tag" := "ProfileRequest"
    ~> "gender" := o.gender
    ~> "birthdate" := o.birthdate
    ~> "website" := o.website
    ~> "websites" := o.websites
    ~> "location" := o.location
    ~> "signature" := o.signature
    ~> "debug" := o.debug
    ~> "guard" := o.guard
    ~> "state_websites" := o.stateWebsites
    ~> jsonEmptyObject


instance profileRequestDecodeJson :: DecodeJson ProfileRequest where
  decodeJson o = do
    obj <- decodeJson o
    gender <- obj .? "gender"
    birthdate <- obj .? "birthdate"
    website <- obj .? "website"
    websites <- obj .? "websites"
    location <- obj .? "location"
    signature <- obj .? "signature"
    debug <- obj .? "debug"
    guard <- obj .? "guard"
    stateWebsites <- obj .? "state_websites"
    pure $ ProfileRequest {
      gender,
      birthdate,
      website,
      websites,
      location,
      signature,
      debug,
      guard,
      stateWebsites
    }


instance profileRequestRequestable :: Requestable ProfileRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance profileRequestRespondable :: Respondable ProfileRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkProfileRequest
      <$> readProp "gender" json
      <*> readProp "birthdate" json
      <*> (unNullOrUndefined <$> readProp "website" json)
      <*> readProp "websites" json
      <*> (unNullOrUndefined <$> readProp "location" json)
      <*> (unNullOrUndefined <$> readProp "signature" json)
      <*> readProp "debug" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "state_websites" json)


instance profileRequestDecode :: Decode ProfileRequest where
  read json =
      mkProfileRequest
      <$> readProp "gender" json
      <*> readProp "birthdate" json
      <*> (unNullOrUndefined <$> readProp "website" json)
      <*> readProp "websites" json
      <*> (unNullOrUndefined <$> readProp "location" json)
      <*> (unNullOrUndefined <$> readProp "signature" json)
      <*> readProp "debug" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "state_websites" json)


newtype ProfileResponse = ProfileResponse {
  id :: Int,
  ent :: Ent,
  entId :: Int,
  gender :: ProfileGender,
  birthdate :: Date,
  website :: (Maybe String),
  location :: (Maybe String),
  signature :: (Maybe String),
  debug :: Boolean,
  karmaGood :: Int,
  karmaBad :: Int,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}


type ProfileResponseR = {
  id :: Int,
  ent :: Ent,
  entId :: Int,
  gender :: ProfileGender,
  birthdate :: Date,
  website :: (Maybe String),
  location :: (Maybe String),
  signature :: (Maybe String),
  debug :: Boolean,
  karmaGood :: Int,
  karmaBad :: Int,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}


_ProfileResponse :: Lens' ProfileResponse {
  id :: Int,
  ent :: Ent,
  entId :: Int,
  gender :: ProfileGender,
  birthdate :: Date,
  website :: (Maybe String),
  location :: (Maybe String),
  signature :: (Maybe String),
  debug :: Boolean,
  karmaGood :: Int,
  karmaBad :: Int,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}
_ProfileResponse f (ProfileResponse o) = ProfileResponse <$> f o


mkProfileResponse :: Int -> Ent -> Int -> ProfileGender -> Date -> (Maybe String) -> (Maybe String) -> (Maybe String) -> Boolean -> Int -> Int -> Int -> (Maybe Date) -> (Maybe Date) -> ProfileResponse
mkProfileResponse id ent entId gender birthdate website location signature debug karmaGood karmaBad guard createdAt modifiedAt =
  ProfileResponse{id, ent, entId, gender, birthdate, website, location, signature, debug, karmaGood, karmaBad, guard, createdAt, modifiedAt}


unwrapProfileResponse :: ProfileResponse -> {
  id :: Int,
  ent :: Ent,
  entId :: Int,
  gender :: ProfileGender,
  birthdate :: Date,
  website :: (Maybe String),
  location :: (Maybe String),
  signature :: (Maybe String),
  debug :: Boolean,
  karmaGood :: Int,
  karmaBad :: Int,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date)
}
unwrapProfileResponse (ProfileResponse r) = r

instance profileResponseEncodeJson :: EncodeJson ProfileResponse where
  encodeJson (ProfileResponse o) =
       "tag" := "ProfileResponse"
    ~> "id" := o.id
    ~> "ent" := o.ent
    ~> "ent_id" := o.entId
    ~> "gender" := o.gender
    ~> "birthdate" := o.birthdate
    ~> "website" := o.website
    ~> "location" := o.location
    ~> "signature" := o.signature
    ~> "debug" := o.debug
    ~> "karma_good" := o.karmaGood
    ~> "karma_bad" := o.karmaBad
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
    ~> jsonEmptyObject


instance profileResponseDecodeJson :: DecodeJson ProfileResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    ent <- obj .? "ent"
    entId <- obj .? "ent_id"
    gender <- obj .? "gender"
    birthdate <- obj .? "birthdate"
    website <- obj .? "website"
    location <- obj .? "location"
    signature <- obj .? "signature"
    debug <- obj .? "debug"
    karmaGood <- obj .? "karma_good"
    karmaBad <- obj .? "karma_bad"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    pure $ ProfileResponse {
      id,
      ent,
      entId,
      gender,
      birthdate,
      website,
      location,
      signature,
      debug,
      karmaGood,
      karmaBad,
      guard,
      createdAt,
      modifiedAt
    }


instance profileResponseRequestable :: Requestable ProfileResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance profileResponseRespondable :: Respondable ProfileResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkProfileResponse
      <$> readProp "id" json
      <*> readProp "ent" json
      <*> readProp "ent_id" json
      <*> readProp "gender" json
      <*> readProp "birthdate" json
      <*> (unNullOrUndefined <$> readProp "website" json)
      <*> (unNullOrUndefined <$> readProp "location" json)
      <*> (unNullOrUndefined <$> readProp "signature" json)
      <*> readProp "debug" json
      <*> readProp "karma_good" json
      <*> readProp "karma_bad" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)


instance profileResponseDecode :: Decode ProfileResponse where
  read json =
      mkProfileResponse
      <$> readProp "id" json
      <*> readProp "ent" json
      <*> readProp "ent_id" json
      <*> readProp "gender" json
      <*> readProp "birthdate" json
      <*> (unNullOrUndefined <$> readProp "website" json)
      <*> (unNullOrUndefined <$> readProp "location" json)
      <*> (unNullOrUndefined <$> readProp "signature" json)
      <*> readProp "debug" json
      <*> readProp "karma_good" json
      <*> readProp "karma_bad" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)


newtype ProfileResponses = ProfileResponses {
  profileResponses :: (Array ProfileResponse)
}


type ProfileResponsesR = {
  profileResponses :: (Array ProfileResponse)
}


_ProfileResponses :: Lens' ProfileResponses {
  profileResponses :: (Array ProfileResponse)
}
_ProfileResponses f (ProfileResponses o) = ProfileResponses <$> f o


mkProfileResponses :: (Array ProfileResponse) -> ProfileResponses
mkProfileResponses profileResponses =
  ProfileResponses{profileResponses}


unwrapProfileResponses :: ProfileResponses -> {
  profileResponses :: (Array ProfileResponse)
}
unwrapProfileResponses (ProfileResponses r) = r

instance profileResponsesEncodeJson :: EncodeJson ProfileResponses where
  encodeJson (ProfileResponses o) =
       "tag" := "ProfileResponses"
    ~> "profile_responses" := o.profileResponses
    ~> jsonEmptyObject


instance profileResponsesDecodeJson :: DecodeJson ProfileResponses where
  decodeJson o = do
    obj <- decodeJson o
    profileResponses <- obj .? "profile_responses"
    pure $ ProfileResponses {
      profileResponses
    }


instance profileResponsesRequestable :: Requestable ProfileResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance profileResponsesRespondable :: Respondable ProfileResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkProfileResponses
      <$> readProp "profile_responses" json


instance profileResponsesDecode :: Decode ProfileResponses where
  read json =
      mkProfileResponses
      <$> readProp "profile_responses" json

-- footer