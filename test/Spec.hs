{-# LANGUAGE OverloadedStrings #-}

import App
import Control.Concurrent.MVar
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
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
      -- TODO ajouter un générateur de booking
      "Create booking" `should` \() -> do
        let booking = Hotel 1 "Foo" 123 (TI.fromGregorian 2019 1 1)
        actual <- postJSON "/bookings" (encode booking)
        assertStatus 200 actual
        actualG <- get "/bookings"
        assertStatus 200 actualG
        assertBody (encode [booking]) actualG
    ]
  where
    noBooking :: [Booking]
    noBooking = []

get :: BS.ByteString -> Session SResponse
get url = request $ setPath defaultRequest {requestMethod = methodGet} url

postJSON :: BS.ByteString -> LBS.ByteString -> Session SResponse
postJSON url json = srequest $ SRequest req json
  where
    req = setPath defaultReq url
    defaultReq =
      defaultRequest
        { requestMethod = methodPost,
          requestHeaders = [(hContentType, "application/json")]
        }

newtype DB = DB {db :: MVar (Map.Map Int Booking)}

inMemoryDB :: IO DB
inMemoryDB = do
  _db <- newMVar Map.empty
  return $ DB _db

testHandle :: DB -> Handle
testHandle (DB dbVar) = Handle
  { repo = Repository
      { findAll = do
          db <- takeMVar dbVar
          return $ Map.elems db,
        save = \booking -> do
          dbRef <- inMemoryDB
          db <- takeMVar dbVar
          let key = Map.size db
              updatedDB = Map.insert key booking db
          putMVar dbVar updatedDB
          return key
      }
  }

runSessionWithApp :: Session a -> IO a
runSessionWithApp s = do
  db <- inMemoryDB
  application (testHandle db) >>= runSession s

should ::
  (Functor f, Testable prop, Testable (f Property)) =>
  TestName ->
  f (Session prop) ->
  TF.Test
should name =
  testProperty name . fmap (ioProperty . runSessionWithApp)
