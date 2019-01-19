{-# LANGUAGE OverloadedStrings #-}

module Storage
  ( getReadings
  , insertReadings
  , getSensors
  , initializeTables
  )
where

import           Data.Char              (isAlphaNum, isAscii)
import           Data.Either            (lefts)
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
                  \ORDER BY timestamp DESC \
                  \LIMIT 10800"
  [":sensor" := sensor']

insertReadings :: Session -> [Reading] -> IO (Either T.Text [Reading])
insertReadings s@(Session conn _) rs = do
  case validateInputs of
    Left e -> return $ Left e
    Right _ -> do
      time <- getCurrentTime
      let readings = timedReading time <$> rs
      withTransaction conn $ mapM_ insert readings
      return $ Right readings

  where
    validateInputs =
      case lefts $ validateIdentifiers <$> rs of
        []    -> Right rs
        i : _ -> Left i

    timedReading t r =
      r { receivedTime = Just t
        , timestamp = Just $ fromMaybe t $ timestamp r }

    insert (Reading sensor' timestamp' receivedTime' value') =
      executeNamed conn "INSERT INTO reading (sensor, timestamp, received_time, value) \
                        \VALUES (:sensor, :timestamp, :received_time, :value) "
      [ ":sensor" := sensor'
      , ":value" := value'
      , ":timestamp" := timestamp'
      , ":received_time" := receivedTime'
      ]

getSensors :: Session -> IO [Sensor]
getSensors (Session conn _) =
  query_ conn "SELECT DISTINCT sensor FROM reading"

validateIdentifiers :: Reading -> Either T.Text Reading
validateIdentifiers r@(Reading sensor' _ _ _) =
  if T.all isValidIdChar sensor'
  then Right r
  else Left "Invalid identifier!"
  where isValidIdChar c = isAscii c && (isAlphaNum c || c == '.' || c == '_')

initializeTables :: Session -> IO ()
initializeTables (Session conn _) =
  mapM_ (execute_ conn) ["CREATE TABLE IF NOT EXISTS reading\
                         \ (sensor TEXT NOT NULL,\
                         \ timestamp TIMESTAMP NOT NULL,\
                         \ received_time TIMESTAMP NOT NULL,\
                         \ value NUMBER NOT NULL)"
                        , "CREATE INDEX IF NOT EXISTS sensor_idx ON reading (sensor)"
                        , "CREATE INDEX IF NOT EXISTS time_idx ON reading (timestamp)"
                        ]
