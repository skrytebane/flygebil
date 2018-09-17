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

type SensorAPI = BasicAuth "flygebil" User :> "sensor" :>
           (Get '[JSON] [Sensor]
            :<|> ReqBody '[JSON] Reading :> Post '[JSON] Reading
            :<|> Capture "name" SensorName :> Get '[JSON] [Reading])

type DebugAPI = "debug" :> Header "Debug" T.Text :> Get '[JSON] T.Text

type API = SensorAPI :<|> DebugAPI

sensorServer :: Session -> Server SensorAPI
sensorServer session@(Session _ secret) user =
  sensors user :<|> newReading user :<|> readings user

  where sensors :: User -> Handler [Sensor]
        sensors _ = liftIO $ getSensors session

        newReading :: User -> Reading -> Handler Reading
        newReading _ reading = do
          r <- liftIO $ insertReading session reading
          case r of
            Left s         -> throwError err400 { errReasonPhrase = convertString s }
            Right reading' -> return reading'

        readings :: User -> SensorName -> Handler [Reading]
        readings _ name = liftIO $ getReadings session name

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

checkBasicAuth :: UserDB -> BasicAuthCheck User
checkBasicAuth db = BasicAuthCheck $ \basicAuthData ->
  let username = convertString $ basicAuthUsername basicAuthData
      password = convertString $ basicAuthPassword basicAuthData
  in
    case userLookup username db of
      Nothing -> return NoSuchUser
      Just u -> if pass u == password
                then return (Authorized u)
                else return BadPassword

app :: UserDB -> Session -> Application
app db session = serveWithContext serverAPI ctx (apiServer session)
  where ctx = checkBasicAuth db :. EmptyContext
