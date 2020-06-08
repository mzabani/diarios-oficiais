module TestDb (aroundWithConn, writeStrRef) where

import qualified Database.PostgreSQL.Simple as DB
import UnliftIO.Exception
import UnliftIO.MVar
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec

connStrRef :: MVar DB.ConnectInfo
connStrRef = unsafePerformIO newEmptyMVar

writeStrRef :: DB.ConnectInfo -> IO ()
writeStrRef cstr = putMVar connStrRef cstr

aroundWithConn :: SpecWith DB.Connection -> Spec
aroundWithConn = around $ \act -> do
    cstr <- readMVar connStrRef
    bracket (DB.connect cstr) DB.close act