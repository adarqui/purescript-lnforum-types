module LN.T.Splits where



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

data Splits
  = SplitAt Char String String
  | SplitNone 



instance splitsEncodeJson :: EncodeJson Splits where
  encodeJson (SplitAt x0 x1 x2) =
       "tag" := "SplitAt"
    ~> "contents" := [encodeJson x0, encodeJson x1, encodeJson x2]
    ~> jsonEmptyObject
  encodeJson (SplitNone ) =
       "tag" := "SplitNone"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance splitsDecodeJson :: DecodeJson Splits where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "SplitAt" -> do
        r <- obj .? "contents"
        case r of
          [x0, x1, x2] -> SplitAt <$> decodeJson x0 <*> decodeJson x1 <*> decodeJson x2
          _ -> Left $ "DecodeJson TypeMismatch for SplitAt"


      "SplitNone" -> do
        pure SplitNone

      _ -> Left $ "DecodeJson TypeMismatch for Splits"



instance splitsRequestable :: Requestable Splits where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance splitsRespondable :: Respondable Splits where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "SplitAt" -> do
        r <- readPropUnsafe "contents" json
        case r of
          [x0, x1, x2] -> SplitAt <$> exceptDecodeJsonRespondable x0 <*> exceptDecodeJsonRespondable x1 <*> exceptDecodeJsonRespondable x2
          _ -> fail $ TypeMismatch "SplitAt" "Respondable"


      "SplitNone" -> do
        pure SplitNone

      _ -> fail $ TypeMismatch "Splits" "Respondable"



data TySplits
  = TySplitA 
  | TySplitNone 



instance tySplitsEncodeJson :: EncodeJson TySplits where
  encodeJson (TySplitA ) =
       "tag" := "TySplitA"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject
  encodeJson (TySplitNone ) =
       "tag" := "TySplitNone"
    ~> "contents" := ([] :: Array String)
    ~> jsonEmptyObject


instance tySplitsDecodeJson :: DecodeJson TySplits where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "TySplitA" -> do
        pure TySplitA

      "TySplitNone" -> do
        pure TySplitNone

      _ -> Left $ "DecodeJson TypeMismatch for TySplits"



instance tySplitsRequestable :: Requestable TySplits where
  toRequest s =
    let str = stringify (encodeJson s) :: String
    in toRequest str


instance tySplitsRespondable :: Respondable TySplits where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json = do
    tag <- readPropUnsafe "tag" json
    case tag of
      "TySplitA" -> do
        pure TySplitA

      "TySplitNone" -> do
        pure TySplitNone

      _ -> fail $ TypeMismatch "TySplits" "Respondable"



instance tySplitsEq :: Eq TySplits where
  eq TySplitA TySplitA = true
  eq TySplitNone TySplitNone = true
  eq _ _ = false
-- footer