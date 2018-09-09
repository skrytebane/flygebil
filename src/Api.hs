{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api (app) where

import           Control.Monad.Reader
import           Data.String.Conversions (convertString)
import           Data.Time.Clock         (getCurrentTime)
import           Servant

import           Storage
import           Types

type API = "sensor" :> Get '[JSON] [Sensor]
  :<|> "sensor" :> ReqBody '[JSON] Reading :> Post '[JSON] Reading
  :<|> "sensor" :> Capture "name" SensorName :> Get '[JSON] [Reading]

server :: Session -> Server API
server session =
  sensors session
  :<|> newReading session
  :<|> readings session

sensors :: Session -> Handler [Sensor]
sensors session = liftIO $ getSensors session

newReading :: Session -> Reading -> Handler Reading
newReading session reading = do
  r <- liftIO $ insertReading session reading
  case r of
    Left s         -> throwError err400 { errReasonPhrase = convertString s }
    Right reading' -> return reading'

readings :: Session -> SensorName -> Handler [Reading]
readings session name = liftIO $ getReadings session name

sensorAPI :: Proxy API
sensorAPI = Proxy

app :: Session -> Application
app session = serve sensorAPI $ server session
