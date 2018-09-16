{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.Text
import Servant
import Servant.API
import Network.Wai
import Network.Wai.Handler.Warp
import GHC.Generics
import Data.Aeson
import Web.FormUrlEncoded
import Database.PostgreSQL.Simple
import Data.Pool
import Control.Monad.IO.Class
import Database.Beam
import Database.Beam.Postgres
import Text.Blaze.Html
import Servant.HTML.Blaze
import Text.Hamlet
import Model.Aprovados

data DadosCadastro = DadosCadastro { nomeCompleto :: Text, email :: Text } deriving Generic

-- instance FromJSON DadosCadastro
instance FromForm DadosCadastro
--instance ToJSON DadosCadastro

type UserAPI = "cadastrar" :> ReqBody '[FormUrlEncoded] DadosCadastro :> Post '[JSON] Bool
                :<|> "listar" :> Get '[HTML] Html
                :<|> Raw

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: Pool Connection -> Server UserAPI
server connectionPool = cadastrar connectionPool :<|> listar :<|> serveDirectoryWebApp "html"

app :: Pool Connection -> Application
app connectionPool = serve userAPI (server connectionPool)

-- Ideias importantes:
-- Deixar usuários livres para visualizarem os últimos 7 diários de cada tipo (garantir que não acessem diários anteriores a isso e que as URLs sejam baseadas em hashes para evitar que alguém nos copie com facilidade)
-- Mostrar todos os diários que baixamos numa lista em algum lugar no site (para que os que querem se cadastrar possam vê-los)
-- Permitir que usuários cadastrados acompanhem CONVOCAÇÕES de alguns concursos
main :: IO ()
main = do
    connectionPool <- createPool (connect connString) close 1 300 10
    run 8080 (app connectionPool)
    destroyAllResources connectionPool

cadastrar :: Pool Connection -> DadosCadastro -> Handler Bool
cadastrar connPool dc =
    liftIO $ withResource connPool $ \conn -> withTransaction conn $ do
        runBeamPostgresDebug putStrLn conn $ runInsert $ insert (_usuarios aprovadosDb) $
            insertExpressions [UsuarioT {
                _usuarioId = default_
                , _usuarioEmail = val_ (email dc)
                , _usuarioNomeCompleto = val_ (nomeCompleto dc)
            }]
        return True

listar :: Handler Html
listar = return $ (toHtml [shamlet|<div>Hello, world!|])

connString :: ConnectInfo
connString = defaultConnectInfo { connectHost = "localhost", connectUser = "mzabani", connectDatabase = "epassei" }