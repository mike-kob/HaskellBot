{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Schema where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Text
import           Data.Time
import           Database.Persist.Postgresql
import           Database.Persist.Sql
import           Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
      User
        userId Int
        username Text Maybe
        created UTCTime default=now()

        Primary userId
        deriving Show
      Message
        messageId Int
        userId UserId
        text Text
        sent UTCTime default=now()

        UniqueMsgUser messageId userId
        Primary messageId

    |]


doMigration :: MonadIO m => SqlPersistT m ()
doMigration = runMigration migrateAll
