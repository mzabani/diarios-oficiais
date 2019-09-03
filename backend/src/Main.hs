module Main where

import Data.Text
import Servant
import Servant.API
import Network.Wai
import ServantExtensions
import Network.Wai.Handler.Warp
import GHC.Generics
import Data.Aeson
import Database.PostgreSQL.Simple
import Data.Pool
import Data.Maybe
import Control.Monad.IO.Class
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Lazy.Encoding
import Data.Binary.Builder
import Control.Exception.Safe hiding (Handler)
import qualified Busca as Busca
import qualified Ler as Ler
import qualified Common as Common

type SinglePageAPI = "busca" :> ReqBody '[JSON] Common.FormBusca :> Post '[JSON] Common.ResultadoBusca
                :<|> "ler" :> Capture "conteudoDiarioId" Int :> Get '[PlainText] Text
                :<|> Raw

singlePageServer :: Pool Connection -> Server SinglePageAPI
singlePageServer connectionPool = 
                             Busca.buscaPost connectionPool
                        :<|> Ler.lerDiario connectionPool
                        :<|> serveDirectoryWebApp "html-SPA"

main :: IO ()
main = do
    -- TODO: chamar "createPool" dentro de "bracket"
    connectionPool <- createPool (connect connString) close 1 300 10
    let api = Proxy :: Proxy SinglePageAPI
    run 8080 $ serve api (singlePageServer connectionPool)
    destroyAllResources connectionPool

connString :: ConnectInfo
connString = defaultConnectInfo { connectHost = "localhost", connectUser = "diariosapp", connectDatabase = "diariosoficiais", connectPort = 5433 }