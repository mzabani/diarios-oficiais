module BasicDataSpec (spec) where

import TestDb (aroundWithConn)
import Test.Hspec
import qualified Database.PostgreSQL.Simple as DB

spec :: Spec
spec = do
    aroundWithConn $ do
        describe "Dados básicos" $ do
            it "Múltiplas origens de diários" multiplasOrigensDeDiarios 

multiplasOrigensDeDiarios :: DB.Connection -> IO ()
multiplasOrigensDeDiarios conn = do
    [ DB.Only (cnt :: Int) ] <- DB.query conn "SELECT COUNT(*) FROM origemdiario" ()
    cnt `shouldSatisfy` (== 0)