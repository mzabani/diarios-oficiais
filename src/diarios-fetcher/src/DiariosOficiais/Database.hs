module DiariosOficiais.Database (
      createDbPool, getDbVcsInfo
) where

import Data.Pool
import DbVcs (DbVcsInfo(..))
import Database.PostgreSQL.Simple
import Data.Time.Clock
import UnliftIO.Environment (getEnv)
import Text.Read
import Data.Maybe
import RIO

getConnString :: MonadIO m => m ConnectInfo
getConnString = do
    user <- getEnv "PGAPPUSER"
    port <- fromMaybe (error "No PGPORT defined") <$> (readMaybe <$> getEnv "PGPORT")
    db <- getEnv "PGDATABASE"
    host <- getEnv "PGHOST"
    return defaultConnectInfo { connectHost = host, connectUser = user, connectDatabase = db, connectPort = port }

getSqlMigrationsConnString :: MonadIO m => m ConnectInfo
getSqlMigrationsConnString = do
    user <- getEnv "PGUSER"
    port <- fromMaybe (error "No PGPORT defined") <$> (readMaybe <$> getEnv "PGPORT")
    host <- getEnv "PGHOST"
    return defaultConnectInfo { connectHost = host, connectUser = user, connectDatabase = "postgres", connectPort = port }

getDbVcsInfo :: MonadIO m => m DbVcsInfo
getDbVcsInfo = do
      superUserConnString <- getSqlMigrationsConnString
      appConnString <- getConnString
      sqlMigrationsDir <- getEnv "SQL_MIGRATIONS_DIR"
      return DbVcsInfo {
            superUserConnString = superUserConnString
            , dbName = connectDatabase appConnString
            , appUser = connectUser appConnString
            , sqlMigrationsDir = sqlMigrationsDir
      }

createDbPool :: MonadIO m => Int -> NominalDiffTime -> Int -> m (Pool Connection)
createDbPool numStripes keepUnusedConnFor maxConnsOpenPerStripe = liftIO $ do
      connString <- getConnString
      createPool (connect connString) close numStripes keepUnusedConnFor maxConnsOpenPerStripe