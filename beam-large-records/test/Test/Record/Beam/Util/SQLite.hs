module Test.Record.Beam.Util.SQLite (
    runInMemory
    -- * Re-exports
  , Sqlite
  ) where

import Control.Exception
import Database.Beam.Sqlite

import qualified Database.SQLite.Simple as SQLite

runInMemory :: (SQLite.Connection -> SqliteM a) -> IO a
runInMemory f =
    bracket (SQLite.open ":memory:") SQLite.close $ \conn ->
      runBeamSqlite conn $ f conn
