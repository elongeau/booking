{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

-- import qualified Data.Monoid as M
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Time as TI
import GHC.Generics
import qualified Web.Scotty as S
import Network.Wai

data Booking =
  Hotel { name :: T.Text,room :: Int, date:: TI.Day} | 
  Train { start :: T.Text, stop :: T.Text }
  deriving (Show, Eq, Generic)

instance ToJSON Booking

application :: IO Application
application = S.scottyApp $
    S.get "/bookings" $ do
      S.json $ [
        Hotel "Foo" 123 (TI.fromGregorian 2019 1 1)
          , Train "Lille" "Paris" ]


