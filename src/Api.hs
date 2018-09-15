{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api (app) where

import           Control.Monad.Reader
import           Data.String.Conversions (convertString)
import qualified Data.Text.Lazy          as T
import           Data.Time.Clock         (getCurrentTime)
import           Servant


import           Storage
import           Types

type SensorAPI = "sensor" :> Header "Authorization" T.Text :>
           (Get '[JSON] [Sensor]
            :<|> ReqBody '[JSON] Reading :> Post '[JSON] Reading
            :<|> Capture "name" SensorName :> Get '[JSON] [Reading])

type DebugAPI = "debug" :> Header "Debug" T.Text :> Get '[JSON] T.Text

type API = SensorAPI :<|> DebugAPI

sensorServer :: Session -> Server SensorAPI
sensorServer session@(Session _ secret) auth =
  sensors auth :<|> newReading auth :<|> readings auth

  where checkAuthorization :: Handler ()
        checkAuthorization =
          case auth of
            Just s | s == secret -> return ()
            _                    -> throwError err403

        sensors :: Maybe T.Text -> Handler [Sensor]
        sensors _ = do
          checkAuthorization
          liftIO $ getSensors session

        newReading :: Maybe T.Text -> Reading -> Handler Reading
        newReading _ reading = do
          checkAuthorization
          r <- liftIO $ insertReading session reading
          case r of
            Left s         -> throwError err400 { errReasonPhrase = convertString s }
            Right reading' -> return reading'

        readings :: Maybe T.Text -> SensorName -> Handler [Reading]
        readings _ name = do
          checkAuthorization
          liftIO $ getReadings session name

debugServer :: Server DebugAPI
debugServer = debug

  where
    debug :: Maybe T.Text -> Handler T.Text
    debug t =
      case t of
        Just t' -> return $ "Heisann, " `T.append` t' `T.append` "!"
        Nothing -> return "Hoi!"

apiServer :: Session -> Server API
apiServer session = sensorServer session :<|> debugServer

serverAPI :: Proxy API
serverAPI = Proxy

app :: Session -> Application
app session = serve serverAPI $ apiServer session
