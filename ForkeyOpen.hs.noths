module ForkeyOpen
    where

import qualified Database.Persist.Sqlite as PSqlite
import qualified Database.Sqlite as Sqlite
import qualified Database.Persist.Sql as Psql
import Control.Monad
import Data.Text
import Data.Int
import System.IO
import Control.Monad.IO.Class 
import Data.Function 

forKeyOpen :: Text -> IO PSqlite.Connection
forKeyOpen t = do 
  conn <- Sqlite.open t
  stmt <- Sqlite.prepare conn "PRAGMA foreign_keys = ON;"
  res <- Sqlite.step stmt 
  PSqlite.wrapConnection conn

forKeyCreatePoolConfig :: MonadIO m => PSqlite.SqliteConf -> m Psql.ConnectionPool
forKeyCreatePoolConfig (PSqlite.SqliteConf cs size) = forKeyCreateSqlitePool cs size

forKeyCreateSqlitePool :: MonadIO m => Text -> Int -> m PSqlite.ConnectionPool
forKeyCreateSqlitePool s = Psql.createSqlPool $ forKeyOpen s


