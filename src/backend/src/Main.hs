module Main where

import Prelude hiding (readFile)
import DbVcs (bringDbUpToDate)
import RIO
import Servant
import Servant.HTML.Blaze (HTML)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (StaticSettings(..), unsafeToPiece)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import DiariosOficiais.Database (createDbPool, getDbVcsInfo)
import Database.PostgreSQL.Simple
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import Data.Pool
import qualified Text.Blaze.Html5 as Blaze
import UnliftIO.Environment (getEnv)
import RIO.Directory (doesFileExist)
import System.FilePath ((</>))
import qualified Busca as Busca
import qualified Ler as Ler
import qualified Common as Common
import qualified System.IO as IO

type AcmeChallengeAPI = ".well-known" :> "acme-challenge" :> Raw

type SinglePageAPI = "busca" :> QueryParam' '[Required] "q" Text :> QueryParam' '[Required] "p" Int :> Get '[JSON] Common.ResultadoBusca
                :<|> "ler" :> Capture "conteudoDiarioId" Int :> Get '[HTML] Blaze.Html
                :<|> Raw

staticFileSettings :: FilePath -> StaticSettings
staticFileSettings frontendDir = (defaultWebAppSettings frontendDir) { ssIndices = [ unsafeToPiece "index.html" ] }

singlePageServer :: FilePath -> Pool Connection -> Server SinglePageAPI
singlePageServer frontendDir connectionPool = 
                             Busca.buscaGet connectionPool
                        :<|> Ler.lerDiario connectionPool
                        :<|> serveDirectoryWith (staticFileSettings frontendDir)

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
    
    certKey <- getEnv "CERTKEYPATH"
    cert <- getEnv "CERTPATH"
    certKeyExists <- doesFileExist certKey
    certExists <- doesFileExist cert
    staticFilesPath <- getEnv "BACKEND_STATIC_FILES_PATH"
    frontendDir <- getEnv "FRONTEND_DIR"
    dbVcsInfo <- getDbVcsInfo
    let
        acmeChallengePath = staticFilesPath </> ".well-known/acme-challenge"
    
    unless (certKeyExists && certExists) $ do
        -- Needs to run certbot to fetch certificates!
        liftIO $ putStrLn $ "Serving ACME challenge on HTTP. Run certbot to fetch certificates and restart me! Acme challenge path: " <> acmeChallengePath
        liftIO $ putStrLn "If you're developing, run \"make run-certbot\" and restart the server once that finishes successfully"
        Warp.run 8080 $ cors (const corsResourcePolicy) $ serve (Proxy :: Proxy AcmeChallengeAPI) (acmeChallengeServer acmeChallengePath)
    
    putStrLn $ "TLS Certificate and key found."
    
    -- Apply DB migrations if necessary
    bringDbUpToDate dbVcsInfo
    bracket (createDbPool 1 300 10) destroyAllResources $ \connectionPool -> do
        let tlss = Warp.tlsSettings cert certKey
            warps = Warp.setPort 8083 Warp.defaultSettings
        putStrLn "All good. Starting Web Server"
        Warp.runTLS tlss warps $ cors (const corsResourcePolicy) $ serve (Proxy :: Proxy SinglePageAPI) (singlePageServer frontendDir connectionPool)