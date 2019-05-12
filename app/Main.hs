{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Logging          as L
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
import           Migrate                  (importNarrowFile)
import           Storage                  (initializeTables, insertReadings)
import           Types                    (Session (Session), User (User),
                                           createUserDB)

data CmdArgs = CmdArgs
  {
    secretFile :: String
  , host       :: String
  , port       :: Int
  , importFile :: Maybe String
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
          <*> P.optional (P.strOption (P.long "import-file"
                                        <> P.short 'i'
                                        <> P.metavar "IMPORT-FILE"
                                        <> P.help "Import from this narrow format SQLite database"))
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

sublists :: Int -> [a] -> [[a]]
sublists n list =
  case list of
    [] -> []
    _  -> hd : sublists n tl
  where
    (hd, tl) = splitAt n list

main :: IO ()
main = L.withStdoutLogging $ do
  CmdArgs secret' host' port' importFile' directory' <- parseOpts
  secret'' <- readSecret secret'

  let settings = setPort port' $
        setHost (fromString host') $
        setBeforeMainLoop
        (L.log' $ convertString $
         T.concat ["Starting flygebil on http://", T.pack host', ":", T.pack $ show port'])
        defaultSettings

  let userDb = createUserDB [("flygebil", User "flygebil" secret'')]

  withConnection (directory' </> "readings" <.> "sqlite3")
    (\conn -> do
        let session = Session conn secret''
        initializeTables session
        case importFile' of
          Just path -> do
            readings <- L.timedLog' "Reading narrow table..." (importNarrowFile path)
            _ <- L.timedLog' "Importing narrow table into wide table..." $
                 mapM_ (insertReadings session) $ sublists 10000 readings
            return ()
          Nothing   -> return ()
        runSettings settings $ app userDb session)
