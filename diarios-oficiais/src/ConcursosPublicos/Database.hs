module ConcursosPublicos.Database (
      createDbPool
) where

import Data.Pool
import Database.PostgreSQL.Simple
import Control.Monad.IO.Class

myConnInfo :: ConnectInfo
myConnInfo = defaultConnectInfo { connectDatabase = "epassei", connectUser = "mzabani" }

createDbPool :: MonadIO m => m (Pool Connection)
createDbPool = liftIO $ createPool (connect myConnInfo) close 1 60 50