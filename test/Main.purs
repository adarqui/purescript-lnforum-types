module Test.Main where



import Control.Monad.Aff.AVar           (AVAR())
import Control.Monad.Eff                (Eff())
import Control.Monad.Eff.Console        (CONSOLE())
import Control.Monad.Except.Trans
import Data.Date.Helpers
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Argonaut.Core               (jsonEmptyObject)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Argonaut.Decode             (class DecodeJson, decodeJson)
import Data.Argonaut.Encode             (class EncodeJson, encodeJson)
import Data.Either                      (Either, either)
import Data.Maybe                       (Maybe (..))
import Prelude                          (class Show, show, class Eq, id, Unit, const, pure, bind, discard, unit, ($), (==), (&&))
import Test.Unit                        (test, suite)
import Test.Unit.Assert                 as Assert
import Test.Unit.Console                (TESTOUTPUT())
import Test.Unit.Main                   (runTest)

import LN.T
import LN.T.Pack.Resource



main :: forall eff. Eff (avar :: AVAR, console :: CONSOLE, testOutput :: TESTOUTPUT | eff) Unit
main = runTest do

  suite "purescript-lnotes-types" do

    test "decode tests" do

      Assert.assertFalse "Should not be equal." $ true == false
      Assert.equal true true

      -- Assert.equal
      --  "2017-01-01T05:00:00.000Z"
      --  $ either id show (decodeJson (encodeJson (dateFromString "2017-01-01T05:00:00.000Z")) :: Either String Date)

  suite "resources" do

    test "decode resource packs" do

      let pack_json = encodeJson resource_pack
      let e_decoded = decodeJson pack_json :: Either String ResourcePackResponse
      case e_decoded of
        Left _ -> pure unit
        Right (ResourcePackResponse pack_) -> do
          let (ResourceResponse resource) = pack_.resource
          Assert.equal resource.id 1
          Assert.equal resource.activityAt Nothing
          Assert.equal resource.createdAt (dateFromString "2016-08-19T22:47:43.886804Z")



resource_pack :: String
resource_pack = "{\"tag\":\"ResourcePackResponses\",\"resource_pack_responses\":[{\"tag\":\"ResourcePackResponse\",\"user\":{\"email_md5\":\"b2eff1bf71b3445e307ec91e4018d24c\",\"display_name\":\"Andrew Darqui\",\"tag\":\"UserSanitizedResponse\",\"guard\":0,\"active\":true,\"name\":\"andrewdarqui\",\"activity_at\":null,\"created_at\":\"2016-08-19T22:47:43.886804Z\",\"id\":1},\"star\":null,\"resource\":{\"display_name\":\"Continuation Passing Style\",\"tag\":\"ResourceResponse\",\"guard\":0,\"urls\":[\"https://en.wikipedia.org/wiki/Continuation-passing_style\"],\"modified_at\":null,\"icon\":null,\"visibility\":{\"tag\":\"Public\",\"contents\":[]},\"categories\":[],\"counter\":0,\"active\":true,\"name\":\"continuation-passing-style\",\"activity_at\":null,\"version\":\"1.0\",\"created_at\":\"2015-09-15T05:19:17.090548Z\",\"author\":[\"Wikipedia\"],\"source\":{\"tag\":\"SourceNone\",\"contents\":[]},\"id\":1,\"prerequisites\":[],\"user_id\":1,\"description\":\"Continuation passing style\",\"tags\":[]},\"stat\":{\"stars\":0,\"tag\":\"ResourceStatResponse\",\"views\":0,\"dislikes\":0,\"leurons\":6,\"neutral\":0,\"resource_id\":1,\"likes\":0},\"permissions\":[],\"resource_id\":1,\"user_id\":1,\"like\":null}"
