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


-- Sensor names
type SensorName = T.Text

newtype Sensor = Sensor
  { name :: SensorName
  } deriving (Eq, Show, Generic)

instance ToJSON Sensor
instance FromJSON Sensor
instance FromRow Sensor where
  fromRow = Sensor <$> field


-- Individual sensor readings
data Reading = Reading {
  sensorName     :: SensorName
  , timestamp    :: Maybe UTCTime
  , receivedTime :: Maybe UTCTime
  , value        :: Double
  } deriving (Eq, Show, Generic)

instance ToJSON Reading
instance FromJSON Reading
instance FromRow Reading where
  fromRow = Reading <$> field <*> field <*> field <*> field


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
