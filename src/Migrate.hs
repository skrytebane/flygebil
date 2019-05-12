{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
-- Code to migrate from narrow to wide table layout.

module Migrate
  (
    importNarrowFile
  )
where

import           Control.Exception      (SomeException, handle)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Either            (rights)
import qualified Data.Map               as Map
import qualified Data.Text.Lazy         as T
import           Data.Time.Clock
import           Database.SQLite.Simple
import           GHC.Generics

import qualified Types                  as NT

type SensorName = T.Text

-- Old-style sensor readings
data Reading = Reading {
  sensorName     :: SensorName
  , timestamp    :: UTCTime
  , receivedTime :: UTCTime
  , value        :: Double
  } deriving (Eq, Show, Generic)

instance ToJSON Reading
instance FromJSON Reading
instance FromRow Reading where
  fromRow = Reading <$> field <*> field <*> field <*> field

data NarrowSignal = NarrowSignal {
  platform        :: T.Text
  , sTimestamp    :: UTCTime
  , sReceivedTime :: UTCTime
  , values        :: [(T.Text, Double)]
  } deriving (Eq, Show)

type PlatTime = Map.Map (T.Text, UTCTime) NarrowSignal

data ConversionError = MissingValues NarrowSignal
  deriving (Eq, Show)

convertSignalsToReadings :: PlatTime -> [Either ConversionError NT.Reading]
convertSignalsToReadings pt =
  Map.foldr (:) [] $ fmap convertSignal pt

  where
    convertSignal item =
        let
          v = values item
          asInt k = floor <$> lookup k v
          asDouble k = lookup k v
          wideReading = NT.Reading (platform item) (sTimestamp item) (Just $ sReceivedTime item) <$>
            asDouble "acceleration" <*>
            asInt "acceleration_x" <*>
            asInt "acceleration_y" <*>
            asInt "acceleration_z" <*>
            asDouble "temperature" <*>
            asDouble "pressure" <*>
            asDouble "humidity" <*>
            asInt "battery"
        in
          case wideReading of
            Just r' -> Right r'
            _       -> Left $ MissingValues item

readNarrowSignals :: Connection -> IO PlatTime
readNarrowSignals conn =
  fold_ conn "SELECT sensor, timestamp, received_time, CAST(value as FLOAT) FROM reading ORDER BY timestamp DESC"
  Map.empty action
  where
    action m (Reading name' time' received' value') =
      case T.splitOn "." name' of
        [name, index, sensor] -> do
          let platform' = T.concat [name, ".", index]
          return $ Map.insertWith
            (\g g' -> g { values = values g ++ values g' })
            (platform', time')
            (NarrowSignal platform' time' received' [(sensor, value')])
            m
        _ -> return m

importNarrowFile :: String -> IO [NT.Reading]
importNarrowFile filename =
  handle handler $
  withConnection filename
  (\conn -> do
      signals <- readNarrowSignals conn
      return $ rights $ convertSignalsToReadings signals)
  where
    handler :: SomeException -> IO [NT.Reading]
    handler e = do
      putStrLn $ "Unable to import database, starting up anyway! Error: " ++ show e
      return []
