{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Bot                         (botStartup)
import           Schema                      (doMigration)

import           Control.Monad.Logger
import           Database.Persist.Postgresql
import           Data.Text                   (pack)
import           Data.Text.Encoding          (encodeUtf8)
import           System.Environment          (getEnv)

main :: IO ()
main = do
  envStr <- getEnv "CONN_STRING"
  let connStr = encodeUtf8 (pack envStr)
  _ <- runStdoutLoggingT . withPostgresqlPool connStr 1 . runSqlPool $ doMigration
  botStartup
