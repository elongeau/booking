{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

-- import qualified Data.Monoid as M

import Control.Monad.IO.Class
import Data.Aeson
import Data.Pool (Pool, createPool, withResource)
import qualified Data.Text as T
import qualified Data.Time as TI
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, close, connect, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field, fromRow)
import GHC.Generics
import Network.Wai
import System.Environment (getEnv)
import qualified Web.Scotty as S

data Booking
  = Hotel
      { id :: Int,
        name :: T.Text,
        room :: Int,
        date :: TI.Day
      }
  deriving (Show, Eq, Generic, ToJSON)

instance FromRow Booking where
  fromRow = Hotel <$> field <*> field <*> field <*> field

newtype Repository a = Repository {findAll :: IO [a]}

type DBPool = Pool Connection

mkRepository :: DBPool -> Repository Booking
mkRepository pool = Repository {findAll = _findAll}
  where
    _findAll :: IO [Booking]
    _findAll = withResource pool $ \conn ->
      query_ conn "SELECT * FROM booking"

readConnectionInfo :: IO ConnectInfo
readConnectionInfo =
  ConnectInfo <$> getEnv "DB_HOST"
    <*> (read <$> getEnv "DB_PORT")
    <*> getEnv "DB_USER"
    <*> getEnv "DB_PASSWORD"
    <*> getEnv "DB_DATABASE"

mkDbPool :: ConnectInfo -> IO DBPool
mkDbPool connectionInfo = do
  let create = connect connectionInfo
  createPool create close 1 0.5 1

newtype Handle
  = Handle
      { repo :: Repository Booking
      }

mkHandle :: DBPool -> Handle
mkHandle pool = Handle
  { repo = mkRepository pool
  }

application :: Handle -> IO Application
application handle =
  S.scottyApp $
    S.get "/bookings" (getBookings handle)

getBookings :: Handle -> S.ActionM ()
getBookings handle = do
  bookings <- liftIO $ findAll $ repo handle
  S.json bookings
