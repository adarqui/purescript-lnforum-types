module LN.T.Organization where


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

newtype OrganizationRequest = OrganizationRequest {
  displayName :: String,
  description :: (Maybe String),
  company :: String,
  location :: String,
  email :: String,
  membership :: Membership,
  tags :: (Array String),
  icon :: (Maybe String),
  visibility :: Visibility,
  guard :: Int
}


type OrganizationRequestR = {
  displayName :: String,
  description :: (Maybe String),
  company :: String,
  location :: String,
  email :: String,
  membership :: Membership,
  tags :: (Array String),
  icon :: (Maybe String),
  visibility :: Visibility,
  guard :: Int
}


_OrganizationRequest :: Lens' OrganizationRequest {
  displayName :: String,
  description :: (Maybe String),
  company :: String,
  location :: String,
  email :: String,
  membership :: Membership,
  tags :: (Array String),
  icon :: (Maybe String),
  visibility :: Visibility,
  guard :: Int
}
_OrganizationRequest f (OrganizationRequest o) = OrganizationRequest <$> f o


mkOrganizationRequest :: String -> (Maybe String) -> String -> String -> String -> Membership -> (Array String) -> (Maybe String) -> Visibility -> Int -> OrganizationRequest
mkOrganizationRequest displayName description company location email membership tags icon visibility guard =
  OrganizationRequest{displayName, description, company, location, email, membership, tags, icon, visibility, guard}


unwrapOrganizationRequest :: OrganizationRequest -> {
  displayName :: String,
  description :: (Maybe String),
  company :: String,
  location :: String,
  email :: String,
  membership :: Membership,
  tags :: (Array String),
  icon :: (Maybe String),
  visibility :: Visibility,
  guard :: Int
}
unwrapOrganizationRequest (OrganizationRequest r) = r

instance organizationRequestEncodeJson :: EncodeJson OrganizationRequest where
  encodeJson (OrganizationRequest o) =
       "tag" := "OrganizationRequest"
    ~> "display_name" := o.displayName
    ~> "description" := o.description
    ~> "company" := o.company
    ~> "location" := o.location
    ~> "email" := o.email
    ~> "membership" := o.membership
    ~> "tags" := o.tags
    ~> "icon" := o.icon
    ~> "visibility" := o.visibility
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance organizationRequestDecodeJson :: DecodeJson OrganizationRequest where
  decodeJson o = do
    obj <- decodeJson o
    displayName <- obj .? "display_name"
    description <- obj .? "description"
    company <- obj .? "company"
    location <- obj .? "location"
    email <- obj .? "email"
    membership <- obj .? "membership"
    tags <- obj .? "tags"
    icon <- obj .? "icon"
    visibility <- obj .? "visibility"
    guard <- obj .? "guard"
    pure $ OrganizationRequest {
      displayName,
      description,
      company,
      location,
      email,
      membership,
      tags,
      icon,
      visibility,
      guard
    }


instance organizationRequestRequestable :: Requestable OrganizationRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance organizationRequestRespondable :: Respondable OrganizationRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkOrganizationRequest
      <$> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "company" json
      <*> readProp "location" json
      <*> readProp "email" json
      <*> readProp "membership" json
      <*> readProp "tags" json
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "visibility" json
      <*> readProp "guard" json


instance organizationRequestIsForeign :: IsForeign OrganizationRequest where
  read json =
      mkOrganizationRequest
      <$> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "company" json
      <*> readProp "location" json
      <*> readProp "email" json
      <*> readProp "membership" json
      <*> readProp "tags" json
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "visibility" json
      <*> readProp "guard" json


newtype OrganizationResponse = OrganizationResponse {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  company :: String,
  location :: String,
  email :: String,
  emailMD5 :: String,
  membership :: Membership,
  icon :: (Maybe String),
  tags :: (Array String),
  visibility :: Visibility,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


type OrganizationResponseR = {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  company :: String,
  location :: String,
  email :: String,
  emailMD5 :: String,
  membership :: Membership,
  icon :: (Maybe String),
  tags :: (Array String),
  visibility :: Visibility,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


_OrganizationResponse :: Lens' OrganizationResponse {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  company :: String,
  location :: String,
  email :: String,
  emailMD5 :: String,
  membership :: Membership,
  icon :: (Maybe String),
  tags :: (Array String),
  visibility :: Visibility,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
_OrganizationResponse f (OrganizationResponse o) = OrganizationResponse <$> f o


mkOrganizationResponse :: Int -> Int -> String -> String -> (Maybe String) -> String -> String -> String -> String -> Membership -> (Maybe String) -> (Array String) -> Visibility -> Boolean -> Int -> (Maybe Date) -> (Maybe Int) -> (Maybe Date) -> (Maybe Date) -> OrganizationResponse
mkOrganizationResponse id userId name displayName description company location email emailMD5 membership icon tags visibility active guard createdAt modifiedBy modifiedAt activityAt =
  OrganizationResponse{id, userId, name, displayName, description, company, location, email, emailMD5, membership, icon, tags, visibility, active, guard, createdAt, modifiedBy, modifiedAt, activityAt}


unwrapOrganizationResponse :: OrganizationResponse -> {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  company :: String,
  location :: String,
  email :: String,
  emailMD5 :: String,
  membership :: Membership,
  icon :: (Maybe String),
  tags :: (Array String),
  visibility :: Visibility,
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedBy :: (Maybe Int),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
unwrapOrganizationResponse (OrganizationResponse r) = r

instance organizationResponseEncodeJson :: EncodeJson OrganizationResponse where
  encodeJson (OrganizationResponse o) =
       "tag" := "OrganizationResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "name" := o.name
    ~> "display_name" := o.displayName
    ~> "description" := o.description
    ~> "company" := o.company
    ~> "location" := o.location
    ~> "email" := o.email
    ~> "email_md5" := o.emailMD5
    ~> "membership" := o.membership
    ~> "icon" := o.icon
    ~> "tags" := o.tags
    ~> "visibility" := o.visibility
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_by" := o.modifiedBy
    ~> "modified_at" := o.modifiedAt
    ~> "activity_at" := o.activityAt
    ~> jsonEmptyObject


instance organizationResponseDecodeJson :: DecodeJson OrganizationResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    name <- obj .? "name"
    displayName <- obj .? "display_name"
    description <- obj .? "description"
    company <- obj .? "company"
    location <- obj .? "location"
    email <- obj .? "email"
    emailMD5 <- obj .? "email_md5"
    membership <- obj .? "membership"
    icon <- obj .? "icon"
    tags <- obj .? "tags"
    visibility <- obj .? "visibility"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedBy <- obj .? "modified_by"
    modifiedAt <- obj .? "modified_at"
    activityAt <- obj .? "activity_at"
    pure $ OrganizationResponse {
      id,
      userId,
      name,
      displayName,
      description,
      company,
      location,
      email,
      emailMD5,
      membership,
      icon,
      tags,
      visibility,
      active,
      guard,
      createdAt,
      modifiedBy,
      modifiedAt,
      activityAt
    }


instance organizationResponseRequestable :: Requestable OrganizationResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance organizationResponseRespondable :: Respondable OrganizationResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkOrganizationResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "name" json
      <*> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "company" json
      <*> readProp "location" json
      <*> readProp "email" json
      <*> readProp "email_md5" json
      <*> readProp "membership" json
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "tags" json
      <*> readProp "visibility" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_by" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


instance organizationResponseIsForeign :: IsForeign OrganizationResponse where
  read json =
      mkOrganizationResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "name" json
      <*> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "company" json
      <*> readProp "location" json
      <*> readProp "email" json
      <*> readProp "email_md5" json
      <*> readProp "membership" json
      <*> (unNullOrUndefined <$> readProp "icon" json)
      <*> readProp "tags" json
      <*> readProp "visibility" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_by" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


newtype OrganizationResponses = OrganizationResponses {
  organizationResponses :: (Array OrganizationResponse)
}


type OrganizationResponsesR = {
  organizationResponses :: (Array OrganizationResponse)
}


_OrganizationResponses :: Lens' OrganizationResponses {
  organizationResponses :: (Array OrganizationResponse)
}
_OrganizationResponses f (OrganizationResponses o) = OrganizationResponses <$> f o


mkOrganizationResponses :: (Array OrganizationResponse) -> OrganizationResponses
mkOrganizationResponses organizationResponses =
  OrganizationResponses{organizationResponses}


unwrapOrganizationResponses :: OrganizationResponses -> {
  organizationResponses :: (Array OrganizationResponse)
}
unwrapOrganizationResponses (OrganizationResponses r) = r

instance organizationResponsesEncodeJson :: EncodeJson OrganizationResponses where
  encodeJson (OrganizationResponses o) =
       "tag" := "OrganizationResponses"
    ~> "organization_responses" := o.organizationResponses
    ~> jsonEmptyObject


instance organizationResponsesDecodeJson :: DecodeJson OrganizationResponses where
  decodeJson o = do
    obj <- decodeJson o
    organizationResponses <- obj .? "organization_responses"
    pure $ OrganizationResponses {
      organizationResponses
    }


instance organizationResponsesRequestable :: Requestable OrganizationResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance organizationResponsesRespondable :: Respondable OrganizationResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkOrganizationResponses
      <$> readProp "organization_responses" json


instance organizationResponsesIsForeign :: IsForeign OrganizationResponses where
  read json =
      mkOrganizationResponses
      <$> readProp "organization_responses" json


newtype OrganizationStatResponse = OrganizationStatResponse {
  organizationId :: Int,
  teams :: Int,
  members :: Int,
  forums :: Int,
  boards :: Int,
  threads :: Int,
  threadPosts :: Int,
  views :: Int
}


type OrganizationStatResponseR = {
  organizationId :: Int,
  teams :: Int,
  members :: Int,
  forums :: Int,
  boards :: Int,
  threads :: Int,
  threadPosts :: Int,
  views :: Int
}


_OrganizationStatResponse :: Lens' OrganizationStatResponse {
  organizationId :: Int,
  teams :: Int,
  members :: Int,
  forums :: Int,
  boards :: Int,
  threads :: Int,
  threadPosts :: Int,
  views :: Int
}
_OrganizationStatResponse f (OrganizationStatResponse o) = OrganizationStatResponse <$> f o


mkOrganizationStatResponse :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> OrganizationStatResponse
mkOrganizationStatResponse organizationId teams members forums boards threads threadPosts views =
  OrganizationStatResponse{organizationId, teams, members, forums, boards, threads, threadPosts, views}


unwrapOrganizationStatResponse :: OrganizationStatResponse -> {
  organizationId :: Int,
  teams :: Int,
  members :: Int,
  forums :: Int,
  boards :: Int,
  threads :: Int,
  threadPosts :: Int,
  views :: Int
}
unwrapOrganizationStatResponse (OrganizationStatResponse r) = r

instance organizationStatResponseEncodeJson :: EncodeJson OrganizationStatResponse where
  encodeJson (OrganizationStatResponse o) =
       "tag" := "OrganizationStatResponse"
    ~> "organization_id" := o.organizationId
    ~> "teams" := o.teams
    ~> "members" := o.members
    ~> "forums" := o.forums
    ~> "boards" := o.boards
    ~> "threads" := o.threads
    ~> "thread_posts" := o.threadPosts
    ~> "views" := o.views
    ~> jsonEmptyObject


instance organizationStatResponseDecodeJson :: DecodeJson OrganizationStatResponse where
  decodeJson o = do
    obj <- decodeJson o
    organizationId <- obj .? "organization_id"
    teams <- obj .? "teams"
    members <- obj .? "members"
    forums <- obj .? "forums"
    boards <- obj .? "boards"
    threads <- obj .? "threads"
    threadPosts <- obj .? "thread_posts"
    views <- obj .? "views"
    pure $ OrganizationStatResponse {
      organizationId,
      teams,
      members,
      forums,
      boards,
      threads,
      threadPosts,
      views
    }


instance organizationStatResponseRequestable :: Requestable OrganizationStatResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance organizationStatResponseRespondable :: Respondable OrganizationStatResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkOrganizationStatResponse
      <$> readProp "organization_id" json
      <*> readProp "teams" json
      <*> readProp "members" json
      <*> readProp "forums" json
      <*> readProp "boards" json
      <*> readProp "threads" json
      <*> readProp "thread_posts" json
      <*> readProp "views" json


instance organizationStatResponseIsForeign :: IsForeign OrganizationStatResponse where
  read json =
      mkOrganizationStatResponse
      <$> readProp "organization_id" json
      <*> readProp "teams" json
      <*> readProp "members" json
      <*> readProp "forums" json
      <*> readProp "boards" json
      <*> readProp "threads" json
      <*> readProp "thread_posts" json
      <*> readProp "views" json


newtype OrganizationStatResponses = OrganizationStatResponses {
  organizationStatResponses :: (Array OrganizationStatResponse)
}


type OrganizationStatResponsesR = {
  organizationStatResponses :: (Array OrganizationStatResponse)
}


_OrganizationStatResponses :: Lens' OrganizationStatResponses {
  organizationStatResponses :: (Array OrganizationStatResponse)
}
_OrganizationStatResponses f (OrganizationStatResponses o) = OrganizationStatResponses <$> f o


mkOrganizationStatResponses :: (Array OrganizationStatResponse) -> OrganizationStatResponses
mkOrganizationStatResponses organizationStatResponses =
  OrganizationStatResponses{organizationStatResponses}


unwrapOrganizationStatResponses :: OrganizationStatResponses -> {
  organizationStatResponses :: (Array OrganizationStatResponse)
}
unwrapOrganizationStatResponses (OrganizationStatResponses r) = r

instance organizationStatResponsesEncodeJson :: EncodeJson OrganizationStatResponses where
  encodeJson (OrganizationStatResponses o) =
       "tag" := "OrganizationStatResponses"
    ~> "organization_stat_responses" := o.organizationStatResponses
    ~> jsonEmptyObject


instance organizationStatResponsesDecodeJson :: DecodeJson OrganizationStatResponses where
  decodeJson o = do
    obj <- decodeJson o
    organizationStatResponses <- obj .? "organization_stat_responses"
    pure $ OrganizationStatResponses {
      organizationStatResponses
    }


instance organizationStatResponsesRequestable :: Requestable OrganizationStatResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance organizationStatResponsesRespondable :: Respondable OrganizationStatResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkOrganizationStatResponses
      <$> readProp "organization_stat_responses" json


instance organizationStatResponsesIsForeign :: IsForeign OrganizationStatResponses where
  read json =
      mkOrganizationStatResponses
      <$> readProp "organization_stat_responses" json

-- footer