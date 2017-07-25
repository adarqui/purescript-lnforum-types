module LN.T.Substitutions where



import Data.Argonaut.Core               (jsonEmptyObject, stringify)
import Data.Argonaut.Decode             (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode             (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Date.Helpers                (Date)
import Data.Either                      (Either(..))
import Data.Foreign                     (ForeignError(..), fail, unsafeFromForeign)
import Data.Foreign.NullOrUndefined     (unNullOrUndefined)
import Data.Foreign.Class               (class Decode, decode)
import Data.Foreign.Helpers             (readPropUnsafe)
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

data Substitutions
  = SubsExpr Substitutions Substitutions
  | SubsOneOf (Array String)
  | SubsAllOf (Array String)
  | SubsBoth Substitutions Substitutions



instance substitutionsEncodeJson :: EncodeJson Substitutions where
  encodeJson (SubsExpr x0 x1) =
       "tag" := "SubsExpr"
    ~> "contents" := [encodeJson x0, encodeJson x1]
    ~> jsonEmptyObject
  encodeJson (SubsOneOf x0) =
       "tag" := "SubsOneOf"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (SubsAllOf x0) =
       "tag" := "SubsAllOf"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (SubsBoth x0 x1) =
       "tag" := "SubsBoth"
    ~> "contents" := [encodeJson x0, encodeJson x1]
    ~> jsonEmptyObject


instance substitutionsDecodeJson :: DecodeJson Substitutions where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "SubsExpr" -> do
        r <- obj .? "contents"
        case r of
          [x0, x1] -> SubsExpr <$> decodeJson x0 <*> decodeJson x1
          _ -> Left $ "DecodeJson TypeMismatch for SubsExpr"


      "SubsOneOf" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> SubsOneOf <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for SubsOneOf"


      "SubsAllOf" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> SubsAllOf <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for SubsAllOf"


      "SubsBoth" -> do
        r <- obj .? "contents"
        case r of
          [x0, x1] -> SubsBoth <$> decodeJson x0 <*> decodeJson x1
          _ -> Left $ "DecodeJson TypeMismatch for SubsBoth"


      _ -> Left $ "DecodeJson TypeMismatch for Substitutions"



instance substitutionsRequestable :: Requestable Substitutions where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance substitutionsRespondable :: Respondable Substitutions where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "SubsExpr" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0, x1] -> SubsExpr <$> decode x0 <*> decode x1
          _ -> fail $ TypeMismatch "SubsExpr" "Respondable"


      "SubsOneOf" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> SubsOneOf <$> decode x0
          _ -> fail $ TypeMismatch "SubsOneOf" "Respondable"


      "SubsAllOf" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> SubsAllOf <$> decode x0
          _ -> fail $ TypeMismatch "SubsAllOf" "Respondable"


      "SubsBoth" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0, x1] -> SubsBoth <$> decode x0 <*> decode x1
          _ -> fail $ TypeMismatch "SubsBoth" "Respondable"


      _ -> fail $ TypeMismatch "Substitutions" "Respondable"



instance substitutionsDecode :: Decode Substitutions where
  decode json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "SubsExpr" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0, x1] -> SubsExpr <$> decode x0 <*> decode x1
          _ -> fail $ TypeMismatch "SubsExpr" "Decode"


      "SubsOneOf" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> SubsOneOf <$> decode x0
          _ -> fail $ TypeMismatch "SubsOneOf" "Decode"


      "SubsAllOf" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> SubsAllOf <$> decode x0
          _ -> fail $ TypeMismatch "SubsAllOf" "Decode"


      "SubsBoth" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0, x1] -> SubsBoth <$> decode x0 <*> decode x1
          _ -> fail $ TypeMismatch "SubsBoth" "Decode"


      _ -> fail $ TypeMismatch "Substitutions" "Decode"



data TySubstitutions
  = TySubsExpr 
  | TySubsOneOf 
  | TySubsAllOf 
  | TySubsBoth 



instance tySubstitutionsEncodeJson :: EncodeJson TySubstitutions where
  encodeJson (TySubsExpr ) =
       "tag" := "TySubsExpr"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TySubsOneOf ) =
       "tag" := "TySubsOneOf"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TySubsAllOf ) =
       "tag" := "TySubsAllOf"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TySubsBoth ) =
       "tag" := "TySubsBoth"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance tySubstitutionsDecodeJson :: DecodeJson TySubstitutions where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "TySubsExpr" -> do
        pure TySubsExpr

      "TySubsOneOf" -> do
        pure TySubsOneOf

      "TySubsAllOf" -> do
        pure TySubsAllOf

      "TySubsBoth" -> do
        pure TySubsBoth

      _ -> Left $ "DecodeJson TypeMismatch for TySubstitutions"



instance tySubstitutionsRequestable :: Requestable TySubstitutions where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance tySubstitutionsRespondable :: Respondable TySubstitutions where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "TySubsExpr" -> do
        pure TySubsExpr

      "TySubsOneOf" -> do
        pure TySubsOneOf

      "TySubsAllOf" -> do
        pure TySubsAllOf

      "TySubsBoth" -> do
        pure TySubsBoth

      _ -> fail $ TypeMismatch "TySubstitutions" "Respondable"



instance tySubstitutionsDecode :: Decode TySubstitutions where
  decode json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "TySubsExpr" -> do
        pure TySubsExpr

      "TySubsOneOf" -> do
        pure TySubsOneOf

      "TySubsAllOf" -> do
        pure TySubsAllOf

      "TySubsBoth" -> do
        pure TySubsBoth

      _ -> fail $ TypeMismatch "TySubstitutions" "Decode"



instance tySubstitutionsEq :: Eq TySubstitutions where
  eq TySubsExpr TySubsExpr = true
  eq TySubsOneOf TySubsOneOf = true
  eq TySubsAllOf TySubsAllOf = true
  eq TySubsBoth TySubsBoth = true
  eq _ _ = false
-- footer