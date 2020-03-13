module DiariosOficiais.Database (
      createDbPool
) where

import Data.Pool
import Database.PostgreSQL.Simple
import Data.Time.Clock
import System.Environment
import Text.Read
import Data.Maybe
import RIO

getConnString :: MonadIO m => m ConnectInfo
getConnString = do
    user <- liftIO $ fromMaybe "diariosapp" <$> lookupEnv "PGAPPUSER"
    port <- liftIO $ fromMaybe 5433 . join <$> (fmap readMaybe <$> lookupEnv "PGPORT")
    db <- liftIO $ fromMaybe "diariosoficiais" <$> lookupEnv "PGDATABASE"
    return defaultConnectInfo { connectHost = "localhost", connectUser = user, connectDatabase = db, connectPort = port }

createDbPool :: MonadIO m => Int -> NominalDiffTime -> Int -> m (Pool Connection)
createDbPool numStripes keepUnusedConnFor maxConnsOpenPerStripe = liftIO $ do
      connString <- getConnString
      createPool (connect connString) close numStripes keepUnusedConnFor maxConnsOpenPerStripe