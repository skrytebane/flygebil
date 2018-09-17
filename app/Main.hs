{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid              ((<>))
import           Data.String              (fromString)
import           Data.String.Conversions  (convertString)
import qualified Data.Text.Lazy           as T
import           Database.SQLite.Simple   (withConnection)
import           Network.Wai.Handler.Warp
import           Options.Applicative      ((<**>))
import qualified Options.Applicative      as P
import           System.FilePath          ((<.>), (</>))

import           Api                      (app)
import           Storage                  (initializeTables)
import           Types                    (Session (Session), User (User),
                                           createUserDB)

data CmdArgs = CmdArgs
  {
    secretFile :: String
  , host       :: String
  , port       :: Int
  , directory  :: String
  }

parseOpts :: IO CmdArgs
parseOpts = P.execParser opts
  where opts = P.info (optParser <**> P.helper)
          ( P.fullDesc
          <> P.progDesc "Run a REST-based sensor logging service"
          <> P.header "flygebil")

        optParser :: P.Parser CmdArgs
        optParser = CmdArgs
          <$> P.strOption (P.long "secret-file"
                           <> P.short 's'
                           <> P.metavar "FILENAME"
                           <> P.help "Read the secret to use from this file, or '-' for stdin")
          <*> P.strOption (P.long "host"
                           <> P.short 'H'
                           <> P.metavar "HOSTNAME"
                           <> P.showDefault
                           <> P.value "127.0.0.1"
                           <> P.help "Bind to this address")
          <*> P.option P.auto (P.long "port"
                               <> P.short 'p'
                               <> P.help "Port to bind to"
                               <> P.showDefault
                               <> P.value 3001
                               <> P.metavar "INTEGER")
          <*> P.strOption (P.long "database-directory"
                           <> P.short 'd'
                           <> P.metavar "DIRECTORY"
                           <> P.showDefault
                           <> P.value "/var/lib/flygebil"
                           <> P.help "Store database in this directory")

readSecret :: String -> IO T.Text
readSecret fileName = do
  contents <- case fileName of
                "-" -> getContents
                _   -> readFile fileName
  return $ T.strip $ convertString contents

main :: IO ()
main = do
  CmdArgs secret' host' port' directory' <- parseOpts
  secret'' <- readSecret secret'

  let settings = setPort port' $
        setHost (fromString host') $
        setBeforeMainLoop
        (putStrLn $ "Starting flygebil on http://" ++ host' ++ ":" ++ show port')
        defaultSettings

  let userDb = createUserDB [("flygebil", User "flygebil" secret'')]

  withConnection (directory' </> "readings" <.> "sqlite3")
    (\conn -> do
        let session = Session conn secret''
        initializeTables session
        runSettings settings $ app userDb session)
