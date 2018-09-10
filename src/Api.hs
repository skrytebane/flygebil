{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api (app) where

import           Control.Monad.Reader
import           Data.String.Conversions (convertString)
import qualified Data.Text.Lazy          as T
import           Data.Time.Clock         (getCurrentTime)
import           Servant


import           Storage
import           Types

type API = "sensor" :> Header "Authorization" T.Text :>
           (Get '[JSON] [Sensor]
            :<|> ReqBody '[JSON] Reading :> Post '[JSON] Reading
            :<|> Capture "name" SensorName :> Get '[JSON] [Reading])

server :: Session -> Server API
server session@(Session _ secret) auth =
  sensors auth :<|> newReading auth :<|> readings auth

  where authMe :: Handler ()
        authMe =
          case auth of
            Just s | s == secret -> return ()
            _                    -> throwError err401

        sensors :: Maybe T.Text -> Handler [Sensor]
        sensors _ = do
          authMe
          liftIO $ getSensors session

        newReading :: Maybe T.Text -> Reading -> Handler Reading
        newReading _ reading = do
          authMe
          r <- liftIO $ insertReading session reading
          case r of
            Left s         -> throwError err400 { errReasonPhrase = convertString s }
            Right reading' -> return reading'

        readings :: Maybe T.Text -> SensorName -> Handler [Reading]
        readings _ name = do
          authMe
          liftIO $ getReadings session name

sensorAPI :: Proxy API
sensorAPI = Proxy

app :: Session -> Application
app session = serve sensorAPI $ server session
