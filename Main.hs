{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson             (ToJSON)
import qualified Data.ByteString.Lazy   as B
import qualified Data.Text.Encoding     as E
import qualified Data.Text.Lazy         as T
import qualified Data.Text.Read         as R
import           Data.Time.Clock
import           Database.SQLite.Simple
import           GHC.Generics
import           Web.Scotty
import qualified Web.Scotty.Trans       as ST

type SourceName = T.Text
type SensorName = T.Text

data Sensor = Sensor {
  source :: SourceName
  , name :: SensorName
  } deriving (Eq, Show, Generic)

instance ToJSON Sensor
instance FromRow Sensor where
  fromRow = Sensor <$> field <*> field

data Reading = Reading {
  sourceName   :: SourceName
  , sensorName :: SensorName
  , timestamp  :: UTCTime
  , value      :: Double
  } deriving (Eq, Show, Generic)

instance ToJSON Reading
instance FromRow Reading where
  fromRow = Reading <$> field <*> field <*> field <*> field

parseData :: B.ByteString -> Either String (Double, T.Text)
parseData data' =
  let t = E.decodeUtf8 $ B.toStrict data'
  in do
    (num, txt) <- R.double t
    return (num, T.fromStrict txt)

insertReading :: Connection -> SourceName -> SensorName -> UTCTime -> Double -> IO ()
insertReading conn source' sensor' timestamp' value' =
  executeNamed conn "INSERT INTO reading (source, sensor, timestamp, value) \
                    \VALUES (:source, :sensor, :timestamp, :value) "
    [":source" := source'
    , ":sensor" := sensor'
    , ":value" := value'
    , ":timestamp" := timestamp'
    ]

postToSensor :: Connection -> SourceName -> SensorName -> B.ByteString -> ActionM ()
postToSensor conn source' sensor' rawData = do
  time <- ST.liftAndCatchIO getCurrentTime
  case parseData rawData of
    Left e        -> json e
    Right (d, "") ->
      ST.liftAndCatchIO $ insertReading conn source' sensor' time d
    Right (_, _)  -> json ("Invalid number"::T.Text)

getSources :: Connection -> IO [Sensor]
getSources conn =
  query_ conn "SELECT source, name FROM sensor"

getReadings :: Connection -> T.Text -> T.Text -> IO [Reading]
getReadings conn source' sensor' =
  queryNamed conn "SELECT source, sensor, timestamp, value \
                  \FROM reading \
                  \WHERE source = :source AND sensor = :sensor"
  [":source" := source', ":sensor" := sensor']

routes :: Connection -> ST.ScottyT T.Text IO ()
routes conn = do
  get "/source" $ do
    sources <- ST.liftAndCatchIO $ getSources conn
    json sources
  get "/source/:name/:sensor" $ do
    source' <- param "name"
    sensor' <- param "sensor"
    readings <- ST.liftAndCatchIO $ getReadings conn source' sensor'
    json readings
  post "/source/:name/:sensor" $ do
    source' <- param "name"
    sensor' <- param "sensor"
    rawData <- body
    postToSensor conn source' sensor' rawData

tables :: [Query]
tables = [
  "CREATE TABLE IF NOT EXISTS \
  \sensor (source TEXT NOT NULL, name TEXT NOT NULL)"
  , "CREATE TABLE IF NOT EXISTS \
    \reading (source TEXT NOT NULL REFERENCES sensor(source), \
    \sensor TEXT NOT NULL REFERENCES sensor(name), \
    \value NUMBER NOT NULL, \
    \timestamp TIMESTAMP NOT NULL)"
  ]

main :: IO ()
main = do
  conn <- open "readings.db"
  mapM_ (execute_ conn) tables
  scotty 3000 $ routes conn
