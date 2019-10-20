{-# LANGUAGE OverloadedStrings #-}

import App
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Time as TI
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test
import Test.Framework.Providers.API as TF
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Runners.Console
import Test.QuickCheck

main :: IO ()
main = do
  defaultMain $
    [ "do something" ~: \() -> do
        actual <- get "/bookings"
        assertStatus 200 actual
        assertBody (encode bookings) actual
    ]

bookings =
  [ Hotel "Foo" 123 (TI.fromGregorian 2019 1 1),
    Train "Lille" "Paris"
  ]

(~:) :: (Functor f, Testable prop, Testable (f Property)) => TestName -> f (Session prop) -> TF.Test
(~:) = appProperty

get :: BS.ByteString -> Session SResponse
get url = request $ setPath defaultRequest {requestMethod = methodGet} url

runSessionWithApp :: Session a -> IO a
runSessionWithApp s = application >>= runSession s

appProperty ::
  (Functor f, Testable prop, Testable (f Property)) =>
  TestName ->
  f (Session prop) ->
  TF.Test
appProperty name =
  testProperty name . fmap (ioProperty . runSessionWithApp)
