module LN.T.Substitution where


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
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance substitutionsRespondable :: Respondable Substitutions where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "SubsExpr" -> do
        r <- readProp "contents" json
        case r of
          [x0, x1] -> SubsExpr <$> read x0 <*> read x1
          _ -> Left $ TypeMismatch "SubsExpr" "Respondable"


      "SubsOneOf" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> SubsOneOf <$> read x0
          _ -> Left $ TypeMismatch "SubsOneOf" "Respondable"


      "SubsAllOf" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> SubsAllOf <$> read x0
          _ -> Left $ TypeMismatch "SubsAllOf" "Respondable"


      "SubsBoth" -> do
        r <- readProp "contents" json
        case r of
          [x0, x1] -> SubsBoth <$> read x0 <*> read x1
          _ -> Left $ TypeMismatch "SubsBoth" "Respondable"


      _ -> Left $ TypeMismatch "Substitutions" "Respondable"



instance substitutionsIsForeign :: IsForeign Substitutions where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "SubsExpr" -> do
        r <- readProp "contents" json
        case r of
          [x0, x1] -> SubsExpr <$> read x0 <*> read x1
          _ -> Left $ TypeMismatch "SubsExpr" "IsForeign"


      "SubsOneOf" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> SubsOneOf <$> read x0
          _ -> Left $ TypeMismatch "SubsOneOf" "IsForeign"


      "SubsAllOf" -> do
        r <- readProp "contents" json
        case r of
          [x0] -> SubsAllOf <$> read x0
          _ -> Left $ TypeMismatch "SubsAllOf" "IsForeign"


      "SubsBoth" -> do
        r <- readProp "contents" json
        case r of
          [x0, x1] -> SubsBoth <$> read x0 <*> read x1
          _ -> Left $ TypeMismatch "SubsBoth" "IsForeign"


      _ -> Left $ TypeMismatch "Substitutions" "IsForeign"



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
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance tySubstitutionsRespondable :: Respondable TySubstitutions where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readProp "tag" json
    case tag of
      "TySubsExpr" -> do
        pure TySubsExpr

      "TySubsOneOf" -> do
        pure TySubsOneOf

      "TySubsAllOf" -> do
        pure TySubsAllOf

      "TySubsBoth" -> do
        pure TySubsBoth

      _ -> Left $ TypeMismatch "TySubstitutions" "Respondable"



instance tySubstitutionsIsForeign :: IsForeign TySubstitutions where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "TySubsExpr" -> do
        pure TySubsExpr

      "TySubsOneOf" -> do
        pure TySubsOneOf

      "TySubsAllOf" -> do
        pure TySubsAllOf

      "TySubsBoth" -> do
        pure TySubsBoth

      _ -> Left $ TypeMismatch "TySubstitutions" "IsForeign"



instance tySubstitutionsEq :: Eq TySubstitutions where
  eq TySubsExpr TySubsExpr = true
  eq TySubsOneOf TySubsOneOf = true
  eq TySubsAllOf TySubsAllOf = true
  eq TySubsBoth TySubsBoth = true
  eq _ _ = false
-- footer