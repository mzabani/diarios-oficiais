module Main where

import Data.Text
import Servant
import Servant.API
import Servant.HTML.Blaze (HTML)
import Network.Wai
import ServantExtensions
import Network.Wai.Handler.Warp
import GHC.Generics
import DiariosOficiais.Database (createDbPool)
import Data.Aeson
import Database.PostgreSQL.Simple
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import Data.Pool
import Data.Maybe
import Control.Monad.IO.Class
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Text.Blaze.Html5 as Blaze
import Data.Text.Lazy.Encoding
import Data.Binary.Builder
import Text.Read (readMaybe)
import Control.Exception.Safe hiding (Handler)
import qualified Busca as Busca
import qualified Ler as Ler
import qualified Common as Common
import qualified System.IO as IO

type SinglePageAPI = "busca" :> ReqBody '[JSON] Common.FormBusca :> Post '[JSON] Common.ResultadoBusca
                :<|> "ler" :> Capture "conteudoDiarioId" Int :> Get '[HTML] Blaze.Html
                :<|> Raw

singlePageServer :: Pool Connection -> Server SinglePageAPI
singlePageServer connectionPool = 
                             Busca.buscaPost connectionPool
                        :<|> Ler.lerDiario connectionPool
                        :<|> serveDirectoryWebApp "html-SPA"

main :: IO ()
main = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    IO.hSetBuffering IO.stderr IO.NoBuffering

    let
        corsResourcePolicy = Just simpleCorsResourcePolicy {
            corsRequestHeaders = ["Content-Type"]
        }
    
    bracket (createDbPool 1 300 10) destroyAllResources $ \connectionPool -> do
        putStrLn "Inicializando servidor web na porta 8080"
        let api = Proxy :: Proxy SinglePageAPI
        run 8080 $ cors (const corsResourcePolicy) $ serve api (singlePageServer connectionPool)
        destroyAllResources connectionPool