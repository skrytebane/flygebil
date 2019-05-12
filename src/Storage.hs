{-# LANGUAGE OverloadedStrings #-}

module Storage
  ( getReadings
  , insertReadings
  , getPlatforms
  , initializeTables
  )
where

import           Data.Char              (isAlphaNum, isAscii)
import           Data.Either            (lefts)
import qualified Data.Text.Lazy         as T
import           Data.Time.Clock        (getCurrentTime)
import           Database.SQLite.Simple

import           Types

getReadings :: Session -> PlatformName -> Int -> IO [Reading]
getReadings (Session conn _) platform' limit =
  queryNamed conn "SELECT platform, timestamp, received_time, \
                  \CAST(acceleration as FLOAT), \
                  \CAST(acceleration_x AS INT), \
                  \CAST(acceleration_y AS INT), \
                  \CAST(acceleration_z AS INT), \
                  \CAST(temperature AS FLOAT), \
                  \CAST(pressure AS FLOAT), \
                  \CAST(humidity AS FLOAT), \
                  \CAST(battery AS INT) \
                  \FROM reading \
                  \WHERE platform = :platform \
                  \ORDER BY timestamp DESC \
                  \LIMIT :limit"
  [
    ":platform" := platform'
  , ":limit" := limit
  ]

insertReadings :: Session -> [Reading] -> IO (Either T.Text [Reading])
insertReadings s@(Session conn _) rs = do
  case validateInputs of
    Left e -> return $ Left e
    Right _ -> do
      time <- getCurrentTime
      let readings = timedReading time <$> rs
      insertPlatforms s $ platform <$> readings
      withTransaction conn $ mapM_ insert readings
      return $ Right readings

  where
    validateInputs =
      case lefts $ validateIdentifiers <$> rs of
        []    -> Right rs
        i : _ -> Left i

    timedReading t r =
      case receivedTime r of
        Nothing -> r { receivedTime = Just t }
        Just _  -> r

    insert (Reading platform' timestamp' receivedTime' acceleration' accelerationX'
            accelerationY' accelerationZ' temperature' pressure' humidity' battery') =
      executeNamed conn
      "INSERT INTO reading (platform, timestamp, received_time, acceleration, \
      \acceleration_x, acceleration_y, acceleration_z, temperature, pressure, \
      \humidity, battery) \
      \VALUES (:platform, :timestamp, :received_time, :acceleration, \
      \:acceleration_x, :acceleration_y, :acceleration_z, :temperature, :pressure, \
      \:humidity, :battery) "
      [
        ":platform" := platform'
      , ":timestamp" := timestamp'
      , ":received_time" := receivedTime'
      , ":acceleration" := acceleration'
      , ":acceleration_x" := accelerationX'
      , ":acceleration_y" := accelerationY'
      , ":acceleration_z" := accelerationZ'
      , ":temperature" := temperature'
      , ":pressure" := pressure'
      , ":humidity" := humidity'
      , ":battery" := battery'
      ]

insertPlatforms :: Session -> [T.Text] -> IO ()
insertPlatforms (Session conn _) names =
  mapM_ insertPlatform names
  where insertPlatform name' =
          executeNamed conn "INSERT OR REPLACE INTO platform (name, description, location)\
                            \VALUES (:name, \
                            \ (SELECT description FROM platform WHERE name = :name),\
                            \ (SELECT location FROM platform WHERE name = :name))"
          [":name" := name']

getPlatforms :: Session -> IO [Platform]
getPlatforms (Session conn _) =
  query_ conn "SELECT name, description, location FROM platform"

validateIdentifiers :: Reading -> Either T.Text Reading
validateIdentifiers r =
  if T.all isValidIdChar (platform r)
  then Right r
  else Left "Invalid identifier!"
  where isValidIdChar c = isAscii c && (isAlphaNum c || c == '.' || c == '_')

initializeTables :: Session -> IO ()
initializeTables (Session conn _) =
  mapM_ (execute_ conn)
  [
    "CREATE TABLE IF NOT EXISTS platform (\
    \ name TEXT NOT NULL PRIMARY KEY,\
    \ description TEXT,\
    \ location TEXT)"
  , "CREATE TABLE IF NOT EXISTS reading (\
    \ platform TEXT NOT NULL REFERENCES platform (name),\
    \ timestamp TIMESTAMP WITH TIME ZONE NOT NULL,\
    \ received_time TIMESTAMP WITH TIME ZONE NOT NULL,\
    \ acceleration NUMERIC,\
    \ acceleration_x INT,\
    \ acceleration_y INT,\
    \ acceleration_z INT,\
    \ temperature NUMERIC,\
    \ pressure NUMERIC,\
    \ humidity NUMERIC,\
    \ battery INT)"
  , "CREATE UNIQUE INDEX IF NOT EXISTS unique_reading \
    \ON reading (platform, timestamp)"
  ]
