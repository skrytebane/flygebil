{-# LANGUAGE DeriveGeneric #-}

module Types where

import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Map               as Map
import qualified Data.Text.Lazy         as T
import           Data.Time.Clock
import           Database.SQLite.Simple
import           GHC.Generics

-- Database session
data Session = Session {
  connection :: Connection
  , secret   :: T.Text
  }

-- Sensor platforms
type PlatformName = T.Text

data Platform = Platform
  {
    name        :: PlatformName
  , description :: Maybe T.Text
  , location    :: Maybe T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON Platform
instance FromJSON Platform
instance FromRow Platform where
  fromRow = Platform <$> field <*> field <*> field


-- Sensor readings
data Reading = Reading {
  platform        :: PlatformName
  , timestamp     :: UTCTime
  , receivedTime  :: Maybe UTCTime
  , acceleration  :: Double
  , accelerationX :: Int
  , accelerationY :: Int
  , accelerationZ :: Int
  , temperature   :: Double
  , pressure      :: Double
  , humidity      :: Double
  , battery       :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON Reading
instance FromJSON Reading
instance FromRow Reading where
  fromRow = Reading <$> field <*> field <*> field <*> field <*> field
    <*> field <*> field <*> field <*> field <*> field <*> field


-- User (for basic auth)
type Username = T.Text
type Password = T.Text

data User = User
  { user :: Username
  , pass :: Password
  } deriving (Eq, Show)

type UserDB = Map.Map Username User

createUserDB :: [(Username, User)] -> UserDB
createUserDB = Map.fromList

userLookup :: Username -> UserDB -> Maybe User
userLookup = Map.lookup
