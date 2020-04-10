module Main where

import RIO
import Servant
import Servant.API
import Servant.HTML.Blaze (HTML)
import Network.Wai
import ServantExtensions
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import GHC.Generics
import DiariosOficiais.Database (createDbPool)
import Data.Aeson
import Database.PostgreSQL.Simple
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import Data.Pool
import qualified Text.Blaze.Html5 as Blaze
import System.Environment (getEnv)
import System.Directory (getCurrentDirectory)
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
                        :<|> serveDirectoryWebApp "results/frontend/bin/frontend.jsexe/"

main :: IO ()
main = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    IO.hSetBuffering IO.stderr IO.NoBuffering

    let
        corsResourcePolicy = Just simpleCorsResourcePolicy {
            corsRequestHeaders = ["Content-Type"]
        }
    
    bracket (createDbPool 1 300 10) destroyAllResources $ \connectionPool -> do
        putStrLn "Lendo variáveis de ambiente com paths para chave e certificado TLS"
        certKey <- liftIO $ getEnv "CERTKEYPATH"
        cert <- liftIO $ getEnv "CERTPATH"
        cwd <- liftIO getCurrentDirectory
        putStrLn $ "Diretório atual: " <> cwd
        putStrLn "Inicializando servidor web na porta 8080"
        let api = Proxy :: Proxy SinglePageAPI
            tlss = Warp.tlsSettings cert certKey
            warps = Warp.setPort 8080 Warp.defaultSettings
        Warp.runTLS tlss warps $ cors (const corsResourcePolicy) $ serve api (singlePageServer connectionPool)
        destroyAllResources connectionPool