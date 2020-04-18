module DbVcs (DbVcsInfo(..), bringDbUpToDate) where

import Prelude hiding (readFile)
import Control.Monad (void, when, forM, forM_)
import Control.Exception (bracket, catch, SomeException)
import Data.String (fromString)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.Types as DB
import RIO.List (sort)
import RIO.Directory (listDirectory)
import RIO.ByteString (readFile)
import System.FilePath ((</>))

data DbVcsInfo = DbVcsInfo {
    dbName :: String
    -- ^ The name of the Database the Application will connect to
    , appUser :: String
    -- ^ The name of the User which will be created and authorized to access any assets created for the App's database.
    --   This is usually the App's User.
    , superUserConnString :: DB.ConnectInfo
    -- ^ A Connection String which has the power to create databases, grant privileges and a lot more.
    , sqlMigrationsDir :: FilePath
    -- ^ A directory with only .sql files. Any .sql file that must be applied in after another .sql file must be alphabetically
    --   greater than the first.
}

bringDbUpToDate :: MonadIO m => DbVcsInfo -> m ()
bringDbUpToDate DbVcsInfo { superUserConnString, dbName, appUser, sqlMigrationsDir } = liftIO $ do
    let
        unsafeDbName = fromString dbName
        unsafeAppUser = fromString appUser
    putStrLn "Going to apply sql migrations... "
    -- TODO: Database server might not be ready, retry with some sleeping in between!!
    bracket (DB.connect superUserConnString) DB.close $ \conn -> do
        dbExists <- isSingleTrue <$> DB.query conn "SELECT TRUE FROM pg_database WHERE datname = ?" (DB.Only dbName)
        userExists <- isSingleTrue <$> DB.query conn "SELECT TRUE FROM pg_catalog.pg_roles WHERE rolname = ?" (DB.Only appUser)
        -- TODO: Totally unsafe against SQL injections, but probably not a problem
        when (not dbExists) $ execvoid_ conn $ "CREATE DATABASE " <> unsafeDbName
        when (not userExists) $ do
            noErrors $ execvoid_ conn $ "CREATE USER " <> unsafeAppUser
            noErrors $ execvoid_ conn $ "GRANT CONNECT ON DATABASE " <> unsafeDbName <> " TO " <> unsafeAppUser
    
    let appDbConnString = superUserConnString { DB.connectDatabase = dbName }
    bracket (DB.connect appDbConnString) DB.close $ \conn -> do
        -- Now we assume a failed transaction means some other app is creating the table and won the race
        noErrors $ DB.withTransaction conn $ do
            -- TODO: Don't do anything in this transaction if the table already exists
            execvoid_ conn $ "CREATE TABLE sql_migrations ( " <>
                " applied_at timestamptz not null default now() " <>
                ", name text not null " <>
                ", unique (name))"
            execvoid_ conn $ "ALTER DEFAULT PRIVILEGES FOR USER postgres IN SCHEMA public GRANT SELECT, INSERT, UPDATE, DELETE, REFERENCES, TRIGGER ON TABLES TO " <> unsafeAppUser
            execvoid_ conn $ "ALTER DEFAULT PRIVILEGES FOR USER postgres IN SCHEMA public GRANT USAGE, SELECT, UPDATE ON SEQUENCES TO " <> unsafeAppUser
            execvoid_ conn $ "ALTER DEFAULT PRIVILEGES FOR USER postgres IN SCHEMA public GRANT EXECUTE ON ROUTINES TO " <> unsafeAppUser
        sqlMigrationFiles :: [FilePath] <- sort <$> listDirectory sqlMigrationsDir
        sqlMigrationsContents <- sqlMigrationFiles `forM` \fn -> (fn,) <$> readFile (sqlMigrationsDir </> fn)
        DB.withTransaction conn $ do
            execvoid_ conn "LOCK sql_migrations IN ACCESS EXCLUSIVE MODE"
            missing :: [FilePath] <- fmap DB.fromOnly <$> DB.returning conn "WITH allMigrations (name) AS (VALUES (?)) SELECT allMigrations.name FROM allMigrations LEFT JOIN sql_migrations applied USING (name) WHERE applied.name IS NULL" (fmap DB.Only sqlMigrationFiles)
            let missingInOrder = filter ((`elem` missing) . fst) sqlMigrationsContents
            putStrLn $ "[ " <> show (length missingInOrder) <> " still unapplied ]"
            forM_ missingInOrder $ \(fn, sql) -> do
                putStr $ "Applying " <> show fn
                execvoid_ conn (DB.Query sql)
                void $ DB.execute conn "INSERT INTO sql_migrations (name) VALUES (?)" (DB.Only fn)
                putStrLn " [ OK ]"
    putStrLn "All migrations applied successfully"

    where isSingleTrue v = v == [ DB.Only True ]
          -- execvoid conn q r = void $ DB.execute conn q r
          execvoid_ conn q = void $ DB.execute_ conn q
          noErrors f = catch @SomeException f (const (pure ()))