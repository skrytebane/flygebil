{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Database.SQLite.Simple
import           Network.Wai.Handler.Warp

import           Api                      (app)
import           Storage                  (initializeTables)
import           Types                    (Session (..))

main :: IO ()
main =
  withConnection "readings.db"
  (\conn -> do
      let session = Session conn
      initializeTables session
      run 8081 $ app session)
