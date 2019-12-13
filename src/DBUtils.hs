module DBUtils where

import Schema

import Control.Monad.Logger (runStdoutLoggingT, LoggingT, filterLogger, LogLevel(..))
import Control.Monad.Reader (runReaderT)
import Database.Persist.Postgresql
import Database.Persist (get, insert, Entity(..), delete)
import System.Environment (getEnv)
import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack)


logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False


getUserById :: Int -> IO (Maybe User)
getUserById uid = do
      envStr <- getEnv "CONN_STRING"
      let connStr = encodeUtf8 (pack envStr)
      runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connStr $ runReaderT action
  where
    action :: SqlPersistT (LoggingT IO) (Maybe User)
    action = get $ UserKey (fromIntegral uid)


createUser :: User -> IO (Key User)
createUser user = do
    envStr <- getEnv "CONN_STRING"
    let connStr = encodeUtf8 (pack envStr)
    runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connStr $ runReaderT action
  where
    action :: SqlPersistT (LoggingT IO) (Key User)
    action = insert user


insertMsg :: Message -> IO (Key Message)
insertMsg msg = do
    envStr <- getEnv "CONN_STRING"
    let connStr = encodeUtf8 (pack envStr)
    runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connStr $ runReaderT action
  where
    action :: SqlPersistT (LoggingT IO) (Key Message)
    action = insert msg
