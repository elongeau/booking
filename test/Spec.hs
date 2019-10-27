{-# LANGUAGE OverloadedStrings #-}

import App
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Map as Map
import Data.Time as TI
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test
import Test.Framework.Providers.API as TF
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Runners.Console
import Test.QuickCheck

main :: IO ()
main =
  defaultMain
    [ "No bookings" `should` \() -> do
        actual <- get "/bookings"
        assertStatus 200 actual
        assertBody (encode noBooking) actual,
      "Create booking" `should` \() -> do
        actual <- postJSON "/bookings" (encode bookings)
        assertStatus 200 actual
        -- actualG <- get "/bookings"
        -- assertStatus 200 actualG
        -- assertBody (encode bookings) actualG
    ]
  where
    noBooking :: [Booking]
    noBooking = []

bookings =
  [ Hotel 1 "Foo" 123 (TI.fromGregorian 2019 1 1)
  ]

get :: BS.ByteString -> Session SResponse
get url = request $ setPath defaultRequest {requestMethod = methodGet} url

postJSON :: BS.ByteString -> LBS.ByteString -> Session SResponse
postJSON url json = srequest $ SRequest req json
  where
    req =
      setPath
        defaultRequest
          { requestMethod = methodPost,
            requestHeaders = [(hContentType, "application/json")]
          }
        url

inMemoryDB :: IO (IORef (Map.Map Int Booking))
inMemoryDB = newIORef Map.empty

testHandle :: Handle
testHandle = Handle
  { repo = Repository
      { -- TODO return from IORef
        findAll = do
          dbRef <- inMemoryDB
          db <- readIORef dbRef
          return $ Map.elems db,
        save = \booking -> do
          dbRef <- inMemoryDB
          db <- readIORef dbRef
          let key = Map.size db
          modifyIORef dbRef $ Map.insert key booking
          return key
      }
  }

runSessionWithApp :: Session a -> IO a
runSessionWithApp s = application testHandle >>= runSession s

should ::
  (Functor f, Testable prop, Testable (f Property)) =>
  TestName ->
  f (Session prop) ->
  TF.Test
should name =
  testProperty name . fmap (ioProperty . runSessionWithApp)
