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
import UnliftIO.Environment (getEnv)
import System.Directory (getCurrentDirectory)
import UnliftIO.Directory (doesFileExist)
import System.FilePath ((</>))
import qualified Busca as Busca
import qualified Ler as Ler
import qualified Common as Common
import qualified System.IO as IO

type AcmeChallengeAPI = ".well-known" :> "acme-challenge" :> Raw

type SinglePageAPI = "busca" :> ReqBody '[JSON] Common.FormBusca :> Post '[JSON] Common.ResultadoBusca
                :<|> "ler" :> Capture "conteudoDiarioId" Int :> Get '[HTML] Blaze.Html
                :<|> AcmeChallengeAPI
                :<|> Raw

singlePageServer :: FilePath -> Pool Connection -> Server SinglePageAPI
singlePageServer acmeChallengePath connectionPool = 
                             Busca.buscaPost connectionPool
                        :<|> Ler.lerDiario connectionPool
                        :<|> serveDirectoryWebApp acmeChallengePath
                        :<|> serveDirectoryWebApp "results/frontend/bin/frontend.jsexe/"

acmeChallengeServer :: FilePath -> Server AcmeChallengeAPI
acmeChallengeServer acmeChallengePath = serveDirectoryWebApp acmeChallengePath

main :: IO ()
main = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    IO.hSetBuffering IO.stderr IO.NoBuffering

    let
        corsResourcePolicy = Just simpleCorsResourcePolicy {
            corsRequestHeaders = ["Content-Type"]
        }
    
    putStrLn "Lendo variáveis de ambiente com paths para chave e certificado TLS"
    certKey <- getEnv "CERTKEYPATH"
    cert <- getEnv "CERTPATH"
    certKeyExists <- doesFileExist certKey
    certExists <- doesFileExist cert
    staticFilesPath <- liftIO $ getEnv "BACKEND_STATIC_FILES_PATH"
    let
        acmeChallengePath = staticFilesPath </> ".well-known/acme-challenge"
    
    unless (certKeyExists && certExists) $ do
        -- Need to run certbot to fetch certificates!
        liftIO $ putStrLn $ "Serving ACME challenge on HTTP. Run certbot to fetch certificates and restart me! Acme challenge path: " <> acmeChallengePath
        liftIO $ putStrLn "If you're developing, run \"make run-certbot\" and restart the server once that finishes successfully"
        Warp.run 8080 $ cors (const corsResourcePolicy) $ serve (Proxy :: Proxy AcmeChallengeAPI) (acmeChallengeServer acmeChallengePath)
    
    bracket (createDbPool 1 300 10) destroyAllResources $ \connectionPool -> do
        putStrLn $ "Certificado e chave TLS encontrados. Inicializando servidor web na porta 8083"
        let tlss = Warp.tlsSettings cert certKey
            warps = Warp.setPort 8083 Warp.defaultSettings
        Warp.runTLS tlss warps $ cors (const corsResourcePolicy) $ serve (Proxy :: Proxy SinglePageAPI) (singlePageServer acmeChallengePath connectionPool)