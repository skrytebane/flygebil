{-# LANGUAGE DeriveGeneric #-}

module Types where

import           Data.Aeson             (FromJSON, ToJSON)
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
