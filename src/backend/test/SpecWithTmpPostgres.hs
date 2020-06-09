import RIO
import qualified Database.Postgres.Temp as PostgresTemp
import Database.PostgreSQL.Simple (connectPostgreSQL, close)
import qualified Database.PostgreSQL.Simple as DB
import DbVcs (DbVcsInfo(..), bringDbUpToDate)
import Data.Monoid (Last(..))
import qualified RIO.Map as Map
import qualified System.IO as IO
import UnliftIO.Exception
import qualified Database.PostgreSQL.Simple.Options as PgOpts

tmpPostgresProcessConf :: PostgresTemp.ProcessConfig
tmpPostgresProcessConf = mempty { PostgresTemp.environmentVariables = mempty { PostgresTemp.specific = Map.fromList [("PGUSER", "postgres") ] } }

tmpPostgresConf :: PostgresTemp.Config
tmpPostgresConf = PostgresTemp.defaultConfig {
    PostgresTemp.connectionTimeout = Last (Just 20000000)
    -- , PostgresTemp.initDbConfig = PostgresTemp.initDbConfig PostgresTemp.defaultConfig <> PostgresTemp.Merge tmpPostgresProcessConf
    -- , PostgresTemp.createDbConfig = PostgresTemp.createDbConfig PostgresTemp.defaultConfig <> PostgresTemp.Merge tmpPostgresProcessConf
    }

main :: IO ()
main = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    IO.hSetBuffering IO.stderr IO.NoBuffering
    
    errorOrUnit <- PostgresTemp.withConfig tmpPostgresConf mainWithDb
    case errorOrUnit of
        Left e -> print e
        Right () -> return ()



mainWithDb :: PostgresTemp.DB -> IO ()
mainWithDb db = do
    let connOpts = PostgresTemp.toConnectionOptions db
        superUsrConnInfo = DB.defaultConnectInfo {
             DB.connectHost = fromMaybe "127.0.0.1" $ getLast $ PgOpts.host connOpts
             , DB.connectUser = fromMaybe "postgres" $ getLast $ PgOpts.user connOpts
             , DB.connectDatabase = fromMaybe "postgres" $ getLast $ PgOpts.dbname connOpts
             , DB.connectPort = fromIntegral $ fromMaybe 5432 $ getLast $ PgOpts.port connOpts
        }
    print superUsrConnInfo
    threadDelay (1000 * 1000 * 60)
    bringDbUpToDate DbVcsInfo {
        dbName = "postgres"
        , appUser = "app"
        , superUserConnString = superUsrConnInfo
        , sqlMigrationsDirs = [ "db-history" ]
    }