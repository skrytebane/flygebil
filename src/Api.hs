{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api (app) where

import           Control.Monad.Reader
import           Data.Maybe              (fromMaybe)
import           Data.String.Conversions (convertString)
import qualified Data.Text.Lazy          as T
import           Servant

import           Storage
import           Types

type PlatformAPI = BasicAuth "flygebil" User :> "platform" :>
           (Get '[JSON] [Platform]
            :<|> ReqBody '[JSON] [Reading] :> Post '[JSON] [Reading]
            :<|> Capture "name" PlatformName :> QueryParam "limit" Int :> Get '[JSON] [Reading])

type DebugAPI = "debug" :> Header "Debug" T.Text :> Get '[JSON] T.Text

type API = PlatformAPI :<|> DebugAPI

sensorServer :: Session -> Server PlatformAPI
sensorServer session user' =
  sensors user' :<|> newReading user' :<|> readings user'

  where sensors :: User -> Handler [Platform]
        sensors _ = liftIO $ getPlatforms session

        newReading :: User -> [Reading] -> Handler [Reading]
        newReading _ readings' = do
          r <- liftIO $ insertReadings session readings'
          case r of
            Left s         -> throwError err400 { errReasonPhrase = convertString s }
            Right readings'' -> return readings''

        readings :: User -> PlatformName -> Maybe Int -> Handler [Reading]
        readings _ name' limit = liftIO $ getReadings session name' (fromMaybe 100 limit)

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
