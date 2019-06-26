{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Text
import Servant
import Servant.API
import Servant.HTML.Blaze
import ServantExtensions
import Network.Wai
import Network.Wai.Handler.Warp
import GHC.Generics
import Data.Aeson
import Web.FormUrlEncoded
import Database.PostgreSQL.Simple
import Data.Pool
import Data.Maybe
import Control.Monad.IO.Class
import Database.Beam
import Database.Beam.Postgres
import Facebook
import Text.Blaze.Html
import Text.Hamlet
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Model.Aprovados
import Control.Lens
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Lazy.Encoding
import Data.Binary.Builder
import Data.String.Conv
import Control.Monad.Trans.Resource
import Control.Monad.IO.Unlift
import Control.Exception.Safe hiding (Handler)

data DadosCadastro = DadosCadastro { cadastroNomeCompleto :: Text, cadastroEmail :: Text, cadastroAccessToken :: Text } deriving Generic
data DadosFacebookLogin = DadosFacebookLogin { accessToken :: Text } deriving Generic
data DadosFacebook = DadosFacebook { fbAccessToken :: Text, fbNomeCompleto :: Text, fbEmail :: Text } deriving Generic

-- instance FromJSON DadosCadastro
instance FromForm DadosCadastro
instance FromForm DadosFacebookLogin
instance ToJSON DadosFacebook

type UserAPI = "cadastro" :> ReqBody '[FormUrlEncoded] DadosCadastro :> PostRedirect 301 String
                :<|> "cadastro" :> Get '[HTML] Html
                :<|> Get '[HTML] Html
                :<|> "fbLogin" :> ReqBody '[FormUrlEncoded] DadosFacebookLogin :> Post '[JSON] DadosFacebook
                :<|> Raw

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: Pool Connection -> Server UserAPI
server connectionPool = cadastroPost connectionPool :<|> cadastroGet :<|> Main.index :<|> Main.fbLogin :<|> serveDirectoryWebApp "html"

app :: Pool Connection -> Application
app connectionPool = serve userAPI (server connectionPool)

-- Ideias importantes:
-- Deixar usuários livres para visualizarem os últimos 7 diários de cada tipo (garantir que não acessem diários anteriores a isso e que as URLs sejam baseadas em hashes para evitar que alguém nos copie com facilidade)
-- Mostrar todos os diários que baixamos numa lista em algum lugar no site (para que os que querem se cadastrar possam vê-los)
-- Permitir que usuários cadastrados acompanhem CONVOCAÇÕES de alguns concursos
main :: IO ()
main = do
    connectionPool <- createPool (connect connString) close 1 300 10
    -- TODO: chamar "run" dentro de "bracket"
    run 8080 (app connectionPool)
    destroyAllResources connectionPool

cadastroGet :: Handler Html
cadastroGet = return (toHtml $(shamletFile "templates/cadastro.hamlet"))

cadastroPost :: Pool Connection -> DadosCadastro -> RedirectHandler String
cadastroPost connPool dc = do
    -- TODO: Pegar userID a partir do Access Token (que deve ser válido) e inserir no banco
    mgr <- newTlsManagerWith tlsManagerSettings
    fbIdMaybe <- fmap (idCode . userId) <$> getFbUserFromAccessToken mgr (cadastroAccessToken dc)
    liftIO $ withResource connPool $ \conn -> withTransaction conn $
        runBeamPostgresDebug putStrLn conn $ runInsert $ insert (_usuarios aprovadosDb) $
            insertExpressions [UsuarioT {
                _usuarioId = default_
                , _usuarioEmail = val_ (cadastroEmail dc)
                , _usuarioNomeCompleto = val_ (cadastroNomeCompleto dc)
                , _usuarioFacebookId = val_ fbIdMaybe
            }]
    redirect "/cadastro-realizado"    

index :: Handler Html
index = let name = ("Marcelo" :: String) in return (toHtml $(shamletFile "templates/index.hamlet"))

fbAppToken :: FacebookT Auth (ResourceT IO) AppAccessToken
fbAppToken = do
    -- TODO: Cachear o AppAccessToken por tempo
    accToken <- getAppAccessToken
    return accToken

getFbUserFromAccessToken :: MonadIO m => Manager -> Text -> m (Maybe User)
getFbUserFromAccessToken _ "" = return Nothing
getFbUserFromAccessToken mgr accessToken = liftIO . runResourceT $ runFacebookT Credentials {
    appName = "ePassei"
    , appId = "336251103621017"
    , appSecret = "28f29b85e3aed5501496e2e0acc7f183"
    } mgr $ do
        appAccToken <- fbAppToken
        dt <- debugToken appAccToken accessToken
        case (dtIsValid dt, dtScopes dt, dtAccessToken dt, dtUserId dt) of
            (Just True, Just perms, Just usrAccToken, Just userID) -> Just <$> getUser userID [] (Just usrAccToken)
            _ -> return Nothing

fbLogin :: DadosFacebookLogin -> Handler DadosFacebook
fbLogin DadosFacebookLogin{..} = do
    mgr <- newTlsManagerWith tlsManagerSettings
    fbUsrMaybe <- getFbUserFromAccessToken mgr accessToken
    case fbUsrMaybe of
        Nothing -> error "Oops!"
        Just fbUsr -> return DadosFacebook {
            fbNomeCompleto = fromMaybe "" (userName fbUsr)
            , fbEmail = fromMaybe "" (userEmail fbUsr)
            , fbAccessToken = accessToken
        }

connString :: ConnectInfo
connString = defaultConnectInfo { connectHost = "localhost", connectUser = "mzabani", connectDatabase = "epassei" }