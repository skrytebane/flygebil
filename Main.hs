{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson             (FromJSON, ToJSON, eitherDecode)
import qualified Data.ByteString.Lazy   as B
import           Data.Char              (isAlphaNum, isAscii)
import           Data.Maybe             (fromMaybe)
import qualified Data.Text.Lazy         as T
import           Data.Time.Clock
import           Database.SQLite.Simple
import           GHC.Generics
import           Web.Scotty
import qualified Web.Scotty.Trans       as ST

type Secret = T.Text

apiKey :: Secret
apiKey = "Jdzfu6CNmuHkL9KJL4dybQ19PUF9keSQqTbclZdK"

newtype Session = Session {
  connection :: Connection
  }

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
  sourceName     :: SourceName
  , sensorName   :: SensorName
  , timestamp    :: Maybe UTCTime
  , receivedTime :: Maybe UTCTime
  , value        :: Double
  } deriving (Eq, Show, Generic)

instance ToJSON Reading
instance FromJSON Reading
instance FromRow Reading where
  fromRow = Reading <$> field <*> field <*> field <*> field <*> field

insertReading :: Session -> Reading -> IO ()
insertReading (Session conn) (Reading source' sensor' timestamp' receivedTime' value') =
  executeNamed conn "INSERT INTO reading (source, sensor, timestamp, received_time, value) \
                    \VALUES (:source, :sensor, :timestamp, :received_time, :value) "
    [":source" := source'
    , ":sensor" := sensor'
    , ":value" := value'
    , ":timestamp" := timestamp'
    , ":received_time" := receivedTime'
    ]

decodeReading :: B.ByteString -> Either String Reading
decodeReading rawData = eitherDecode rawData >>= validateReading

validateReading :: Reading -> Either String Reading
validateReading r@(Reading source' sensor' _ _ _) =
  if validIdentifier source' && validIdentifier sensor'
  then Right r
  else Left "Invalid identifiers"
  where isAlphaAscii c = isAscii c && isAlphaNum c
        validIdentifier = T.all isAlphaAscii

postToSensor :: Session -> B.ByteString -> ActionM ()
postToSensor conn rawData = do
  time <- ST.liftAndCatchIO getCurrentTime
  case decodeReading rawData of
    Left s      -> raise $ T.pack $ "<p>In reader parsing: " ++ s ++ ".</p>"
    Right reading -> ST.liftAndCatchIO $
      insertReading conn $
      reading { receivedTime = Just time
              , timestamp = Just $ fromMaybe time $ timestamp reading
              }

getSources :: Session -> IO [Sensor]
getSources (Session conn) =
  query_ conn "SELECT DISTINCT source, sensor FROM reading"

getReadings :: Session -> T.Text -> T.Text -> IO [Reading]
getReadings (Session conn) source' sensor' =
  queryNamed conn "SELECT source, sensor, timestamp, received_time, CAST(value as FLOAT) \
                  \FROM reading \
                  \WHERE source = :source AND sensor = :sensor \
                  \ORDER BY timestamp DESC"
  [":source" := source', ":sensor" := sensor']

validateSecret :: Maybe Secret -> ActionM ()
validateSecret Nothing = raise "Invalid secret!"
validateSecret (Just providedSecret) | providedSecret == apiKey = next
                                     | otherwise = raise "Invalid secret!"

routes :: Session -> ST.ScottyT T.Text IO ()
routes session = do
  matchAny (regex ".*") $ do
    secret'' <- header "X-Secret"
    ST.liftAndCatchIO $ putStrLn $ "Got secret " ++ show secret''
    validateSecret secret''
  get "/source" $ do
    sources <- ST.liftAndCatchIO $ getSources session
    json sources
  post "/source" $ do
    rawData <- body
    postToSensor session rawData
  get "/source/:name/:sensor" $ do
    source' <- param "name"
    sensor' <- param "sensor"
    readings <- ST.liftAndCatchIO $ getReadings session source' sensor'
    json readings

tables :: [Query]
tables = [
  "CREATE TABLE IF NOT EXISTS reading \
  \(source TEXT NOT NULL REFERENCES sensor(source), \
  \sensor TEXT NOT NULL REFERENCES sensor(name), \
  \timestamp TIMESTAMP NOT NULL, \
  \received_time TIMESTAMP NOT NULL, \
  \value NUMBER NOT NULL)"
  ]

main :: IO ()
main =
  withConnection "readings.db" runServer
  where runServer conn = do
          mapM_ (execute_ conn) tables
          scotty 3000 $ routes $ Session conn
