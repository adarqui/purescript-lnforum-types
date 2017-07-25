module LN.T.Error where



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

data ApplicationError
  = Error_Unknown 
  | Error_NotFound 
  | Error_PermissionDenied 
  | Error_AlreadyExists 
  | Error_Visibility 
  | Error_Membership 
  | Error_Validation ValidationError
  | Error_NotImplemented 
  | Error_InvalidArguments String
  | Error_Unexpected 



instance applicationErrorEncodeJson :: EncodeJson ApplicationError where
  encodeJson (Error_Unknown ) =
       "tag" := "Error_Unknown"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Error_NotFound ) =
       "tag" := "Error_NotFound"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Error_PermissionDenied ) =
       "tag" := "Error_PermissionDenied"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Error_AlreadyExists ) =
       "tag" := "Error_AlreadyExists"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Error_Visibility ) =
       "tag" := "Error_Visibility"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Error_Membership ) =
       "tag" := "Error_Membership"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Error_Validation x0) =
       "tag" := "Error_Validation"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (Error_NotImplemented ) =
       "tag" := "Error_NotImplemented"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Error_InvalidArguments x0) =
       "tag" := "Error_InvalidArguments"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject
  encodeJson (Error_Unexpected ) =
       "tag" := "Error_Unexpected"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance applicationErrorDecodeJson :: DecodeJson ApplicationError where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Error_Unknown" -> do
        pure Error_Unknown

      "Error_NotFound" -> do
        pure Error_NotFound

      "Error_PermissionDenied" -> do
        pure Error_PermissionDenied

      "Error_AlreadyExists" -> do
        pure Error_AlreadyExists

      "Error_Visibility" -> do
        pure Error_Visibility

      "Error_Membership" -> do
        pure Error_Membership

      "Error_Validation" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> Error_Validation <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for Error_Validation"


      "Error_NotImplemented" -> do
        pure Error_NotImplemented

      "Error_InvalidArguments" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> Error_InvalidArguments <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for Error_InvalidArguments"


      "Error_Unexpected" -> do
        pure Error_Unexpected

      _ -> Left $ "DecodeJson TypeMismatch for ApplicationError"



instance applicationErrorRequestable :: Requestable ApplicationError where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance applicationErrorRespondable :: Respondable ApplicationError where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "Error_Unknown" -> do
        pure Error_Unknown

      "Error_NotFound" -> do
        pure Error_NotFound

      "Error_PermissionDenied" -> do
        pure Error_PermissionDenied

      "Error_AlreadyExists" -> do
        pure Error_AlreadyExists

      "Error_Visibility" -> do
        pure Error_Visibility

      "Error_Membership" -> do
        pure Error_Membership

      "Error_Validation" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> Error_Validation <$> decode x0
          _ -> fail $ TypeMismatch "Error_Validation" "Respondable"


      "Error_NotImplemented" -> do
        pure Error_NotImplemented

      "Error_InvalidArguments" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> Error_InvalidArguments <$> decode x0
          _ -> fail $ TypeMismatch "Error_InvalidArguments" "Respondable"


      "Error_Unexpected" -> do
        pure Error_Unexpected

      _ -> fail $ TypeMismatch "ApplicationError" "Respondable"



instance applicationErrorDecode :: Decode ApplicationError where
  decode json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "Error_Unknown" -> do
        pure Error_Unknown

      "Error_NotFound" -> do
        pure Error_NotFound

      "Error_PermissionDenied" -> do
        pure Error_PermissionDenied

      "Error_AlreadyExists" -> do
        pure Error_AlreadyExists

      "Error_Visibility" -> do
        pure Error_Visibility

      "Error_Membership" -> do
        pure Error_Membership

      "Error_Validation" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> Error_Validation <$> decode x0
          _ -> fail $ TypeMismatch "Error_Validation" "Decode"


      "Error_NotImplemented" -> do
        pure Error_NotImplemented

      "Error_InvalidArguments" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> Error_InvalidArguments <$> decode x0
          _ -> fail $ TypeMismatch "Error_InvalidArguments" "Decode"


      "Error_Unexpected" -> do
        pure Error_Unexpected

      _ -> fail $ TypeMismatch "ApplicationError" "Decode"



instance applicationErrorEq :: Eq ApplicationError where
  eq Error_Unknown Error_Unknown = true
  eq Error_NotFound Error_NotFound = true
  eq Error_PermissionDenied Error_PermissionDenied = true
  eq Error_AlreadyExists Error_AlreadyExists = true
  eq Error_Visibility Error_Visibility = true
  eq Error_Membership Error_Membership = true
  eq (Error_Validation x0a) (Error_Validation x0b) = x0a == x0b
  eq Error_NotImplemented Error_NotImplemented = true
  eq (Error_InvalidArguments x0a) (Error_InvalidArguments x0b) = x0a == x0b
  eq Error_Unexpected Error_Unexpected = true
  eq _ _ = false

instance applicationErrorDefault :: Default ApplicationError where
  def = Error_Unknown

data ValidationError
  = Validate ValidationErrorCode (Maybe String)



instance validationErrorEncodeJson :: EncodeJson ValidationError where
  encodeJson (Validate x0 x1) =
       "tag" := "Validate"
    ~> "contents" := [encodeJson x0, encodeJson x1]
    ~> jsonEmptyObject


instance validationErrorDecodeJson :: DecodeJson ValidationError where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Validate" -> do
        r <- obj .? "contents"
        case r of
          [x0, x1] -> Validate <$> decodeJson x0 <*> decodeJson x1
          _ -> Left $ "DecodeJson TypeMismatch for Validate"


      _ -> Left $ "DecodeJson TypeMismatch for ValidationError"



instance validationErrorRequestable :: Requestable ValidationError where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance validationErrorRespondable :: Respondable ValidationError where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "Validate" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0, x1] -> Validate <$> decode x0 <*> decode x1
          _ -> fail $ TypeMismatch "Validate" "Respondable"


      _ -> fail $ TypeMismatch "ValidationError" "Respondable"



instance validationErrorDecode :: Decode ValidationError where
  decode json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "Validate" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0, x1] -> Validate <$> decode x0 <*> decode x1
          _ -> fail $ TypeMismatch "Validate" "Decode"


      _ -> fail $ TypeMismatch "ValidationError" "Decode"



instance validationErrorEq :: Eq ValidationError where
  eq (Validate x0a x1a) (Validate x0b x1b) = x0a == x0b && x1a == x1b


instance validationErrorDefault :: Default ValidationError where
  def = Validate Validate_Unknown Nothing

data ValidationErrorCode
  = Validate_Unknown 
  | Validate_InvalidCharacters 
  | Validate_InvalidEmail 
  | Validate_InvalidDate 
  | Validate_CannotBeEmpty 
  | Validate_TooLong 
  | Validate_TooShort 
  | Validate_GreaterThanMaximum 
  | Validate_SmallerThanMinimum 
  | Validate_Reason String



instance validationErrorCodeEncodeJson :: EncodeJson ValidationErrorCode where
  encodeJson (Validate_Unknown ) =
       "tag" := "Validate_Unknown"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Validate_InvalidCharacters ) =
       "tag" := "Validate_InvalidCharacters"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Validate_InvalidEmail ) =
       "tag" := "Validate_InvalidEmail"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Validate_InvalidDate ) =
       "tag" := "Validate_InvalidDate"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Validate_CannotBeEmpty ) =
       "tag" := "Validate_CannotBeEmpty"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Validate_TooLong ) =
       "tag" := "Validate_TooLong"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Validate_TooShort ) =
       "tag" := "Validate_TooShort"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Validate_GreaterThanMaximum ) =
       "tag" := "Validate_GreaterThanMaximum"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Validate_SmallerThanMinimum ) =
       "tag" := "Validate_SmallerThanMinimum"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (Validate_Reason x0) =
       "tag" := "Validate_Reason"
    ~> "contents" := [encodeJson x0]
    ~> jsonEmptyObject


instance validationErrorCodeDecodeJson :: DecodeJson ValidationErrorCode where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Validate_Unknown" -> do
        pure Validate_Unknown

      "Validate_InvalidCharacters" -> do
        pure Validate_InvalidCharacters

      "Validate_InvalidEmail" -> do
        pure Validate_InvalidEmail

      "Validate_InvalidDate" -> do
        pure Validate_InvalidDate

      "Validate_CannotBeEmpty" -> do
        pure Validate_CannotBeEmpty

      "Validate_TooLong" -> do
        pure Validate_TooLong

      "Validate_TooShort" -> do
        pure Validate_TooShort

      "Validate_GreaterThanMaximum" -> do
        pure Validate_GreaterThanMaximum

      "Validate_SmallerThanMinimum" -> do
        pure Validate_SmallerThanMinimum

      "Validate_Reason" -> do
        r <- obj .? "contents"
        case r of
          [x0] -> Validate_Reason <$> decodeJson x0
          _ -> Left $ "DecodeJson TypeMismatch for Validate_Reason"


      _ -> Left $ "DecodeJson TypeMismatch for ValidationErrorCode"



instance validationErrorCodeRequestable :: Requestable ValidationErrorCode where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance validationErrorCodeRespondable :: Respondable ValidationErrorCode where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "Validate_Unknown" -> do
        pure Validate_Unknown

      "Validate_InvalidCharacters" -> do
        pure Validate_InvalidCharacters

      "Validate_InvalidEmail" -> do
        pure Validate_InvalidEmail

      "Validate_InvalidDate" -> do
        pure Validate_InvalidDate

      "Validate_CannotBeEmpty" -> do
        pure Validate_CannotBeEmpty

      "Validate_TooLong" -> do
        pure Validate_TooLong

      "Validate_TooShort" -> do
        pure Validate_TooShort

      "Validate_GreaterThanMaximum" -> do
        pure Validate_GreaterThanMaximum

      "Validate_SmallerThanMinimum" -> do
        pure Validate_SmallerThanMinimum

      "Validate_Reason" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> Validate_Reason <$> decode x0
          _ -> fail $ TypeMismatch "Validate_Reason" "Respondable"


      _ -> fail $ TypeMismatch "ValidationErrorCode" "Respondable"



instance validationErrorCodeDecode :: Decode ValidationErrorCode where
  decode json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "Validate_Unknown" -> do
        pure Validate_Unknown

      "Validate_InvalidCharacters" -> do
        pure Validate_InvalidCharacters

      "Validate_InvalidEmail" -> do
        pure Validate_InvalidEmail

      "Validate_InvalidDate" -> do
        pure Validate_InvalidDate

      "Validate_CannotBeEmpty" -> do
        pure Validate_CannotBeEmpty

      "Validate_TooLong" -> do
        pure Validate_TooLong

      "Validate_TooShort" -> do
        pure Validate_TooShort

      "Validate_GreaterThanMaximum" -> do
        pure Validate_GreaterThanMaximum

      "Validate_SmallerThanMinimum" -> do
        pure Validate_SmallerThanMinimum

      "Validate_Reason" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0] -> Validate_Reason <$> decode x0
          _ -> fail $ TypeMismatch "Validate_Reason" "Decode"


      _ -> fail $ TypeMismatch "ValidationErrorCode" "Decode"



instance validationErrorCodeEq :: Eq ValidationErrorCode where
  eq Validate_Unknown Validate_Unknown = true
  eq Validate_InvalidCharacters Validate_InvalidCharacters = true
  eq Validate_InvalidEmail Validate_InvalidEmail = true
  eq Validate_InvalidDate Validate_InvalidDate = true
  eq Validate_CannotBeEmpty Validate_CannotBeEmpty = true
  eq Validate_TooLong Validate_TooLong = true
  eq Validate_TooShort Validate_TooShort = true
  eq Validate_GreaterThanMaximum Validate_GreaterThanMaximum = true
  eq Validate_SmallerThanMinimum Validate_SmallerThanMinimum = true
  eq (Validate_Reason x0a) (Validate_Reason x0b) = x0a == x0b
  eq _ _ = false

instance validationErrorCodeDefault :: Default ValidationErrorCode where
  def = Validate_Unknown
-- footer
