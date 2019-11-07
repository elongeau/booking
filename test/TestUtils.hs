{-# LANGUAGE OverloadedStrings #-}

module TestUtils where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test

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
