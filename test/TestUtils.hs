{-# LANGUAGE OverloadedStrings #-}

module TestUtils where

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

runSessionWithApp :: Session a -> IO a
runSessionWithApp s = inMemoryDB >>= application . testHandle >>= runSession s

should ::
  (Functor f, Testable prop, Testable (f Property)) =>
  TestName ->
  f (Session prop) ->
  TF.Test
should name = testProperty name . fmap (ioProperty . runSessionWithApp)
