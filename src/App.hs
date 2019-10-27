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
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, close, connect, execute, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field, fromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow (..), toRow)
import GHC.Generics
import Network.Wai
import System.Environment (getEnv)
import qualified Web.Scotty as S

data Booking
  = Hotel
      { idBooking :: Int,
        name :: T.Text,
        room :: Int,
        date :: TI.Day
      }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRow Booking where
  fromRow = Hotel <$> field <*> field <*> field <*> field

instance ToRow Booking where
  toRow (Hotel idBooking name room date) = toRow (idBooking, name, room, date)

data Repository a
  = Repository
      { findAll :: IO [a],
        save :: a -> IO Int
      }

type DBPool = Pool Connection

mkRepository :: DBPool -> Repository Booking
mkRepository pool = Repository
  { findAll = _findAll,
    save = _save
  }
  where
    _findAll :: IO [Booking]
    _findAll = withResource pool $ \conn ->
      query_ conn "SELECT * FROM booking"
    _save :: Booking -> IO Int
    _save booking = withResource pool $ \conn -> do
      execute conn "insert into booking values (?, ?, ?, ?)" booking
      return $ idBooking booking

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
  S.scottyApp $ do
    S.get
      "/bookings"
      (getBookings handle)
    S.post
      "/bookings"
      (createBooking handle)

getBookings :: Handle -> S.ActionM ()
getBookings handle = do
  bookings <- liftIO $ findAll $ repo handle
  S.json bookings

createBooking :: Handle -> S.ActionM ()
createBooking handle = do
  booking <- S.jsonData :: S.ActionM Booking
  -- TODO set ID
  idBooking <- liftIO $ save (repo handle) booking
  S.json idBooking
