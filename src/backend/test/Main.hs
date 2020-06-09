module Main where

import RIO
import qualified Database.PostgreSQL.Simple as DB
import DbVcs (DbVcsInfo(..), withDbAndDrop)
import qualified System.IO as IO
import DiariosOficiais.Database (getDbVcsInfo, getAppConnString)
import UnliftIO.Exception
import UnliftIO.MVar
import System.FilePath ((</>))
import qualified AllSpecs
import qualified TestDb
import Test.Hspec

main :: IO ()
main = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    IO.hSetBuffering IO.stderr IO.NoBuffering
    
    dbInfo <- getDbVcsInfo
    let testDbInfo = dbInfo { dbName = "test-database", sqlMigrationsDirs = "../../db-history-test-db" : fmap ("../../" </>) (sqlMigrationsDirs dbInfo) }
    withDbAndDrop testDbInfo $ \testConnStr -> do
        TestDb.writeStrRef testConnStr
        hspec AllSpecs.spec