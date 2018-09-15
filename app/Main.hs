{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.String.Conversions  (convertString)
import qualified Data.Text.Lazy           as T
import           Database.SQLite.Simple
import           Network.Wai.Handler.Warp
import           System.Environment       (getArgs)
import           System.IO                (IOMode (ReadMode), hGetContents,
                                           withFile)

import           Api                      (app)
import           Storage                  (initializeTables)
import           Types                    (Session (..))

main :: IO ()
main =
  withConnection "readings.db"
  (\conn -> do
      secret <- getSecret "supersekrit"
      let session = Session conn secret
      initializeTables session
      run 8081 $ app session)

  where getSecret :: String -> IO T.Text
        getSecret default' = do
          args <- getArgs
          let contents =
                case args of
                  "-" : _      -> getContents
                  fileName : _ -> withFile fileName ReadMode hGetContents
                  _            -> return default'
          T.strip . convertString <$> contents
