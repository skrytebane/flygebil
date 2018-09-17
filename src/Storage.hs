{-# LANGUAGE OverloadedStrings #-}

module Storage
  ( getReadings
  , insertReading
  , getSensors
  , initializeTables
  )
where

import           Data.Char              (isAlphaNum, isAscii)
import           Data.Maybe             (fromMaybe)
import qualified Data.Text.Lazy         as T
import           Data.Time.Clock        (getCurrentTime)
import           Database.SQLite.Simple

import           Types

getReadings :: Session -> T.Text -> IO [Reading]
getReadings (Session conn _) sensor' =
  queryNamed conn "SELECT sensor, timestamp, received_time, CAST(value as FLOAT) \
                  \FROM reading \
                  \WHERE sensor = :sensor \
                  \ORDER BY timestamp DESC"
  [":sensor" := sensor']

insertReading :: Session -> Reading -> IO (Either T.Text Reading)
insertReading (Session conn _) r =
  case validateIdentifiers r of
    Left e -> return $ Left e
    Right reading -> do
      time <- getCurrentTime
      let reading'@(Reading sensor' timestamp' receivedTime' value')
            = reading { receivedTime = Just time
                      , timestamp = Just $ fromMaybe time $ timestamp r }
      executeNamed conn "INSERT INTO reading (sensor, timestamp, received_time, value) \
                        \VALUES (:sensor, :timestamp, :received_time, :value) "
        [ ":sensor" := sensor'
        , ":value" := value'
        , ":timestamp" := timestamp'
        , ":received_time" := receivedTime'
        ]
      return $ Right reading'

getSensors :: Session -> IO [Sensor]
getSensors (Session conn _) =
  query_ conn "SELECT DISTINCT sensor FROM reading"

validateIdentifiers :: Reading -> Either T.Text Reading
validateIdentifiers r@(Reading sensor' _ _ _) =
  if T.all isValidIdChar sensor'
  then Right r
  else Left "Invalid identifier!"
  where isValidIdChar c = isAscii c && (isAlphaNum c || c == '.')

initializeTables :: Session -> IO ()
initializeTables (Session conn _) =
  mapM_ (execute_ conn) ["CREATE TABLE IF NOT EXISTS reading\
                         \ (sensor TEXT NOT NULL,\
                         \ timestamp TIMESTAMP NOT NULL,\
                         \ received_time TIMESTAMP NOT NULL,\
                         \ value NUMBER NOT NULL)"
                        , "CREATE INDEX sensor_idx ON reading (sensor)"
                        , "CREATE INDEX time_idx ON reading (timestamp)"
                        ]
