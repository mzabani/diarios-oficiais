{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Model.Aprovados
import Heist
import Heist.Interpreted
import Control.Lens
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Lazy.Encoding
import Data.Binary.Builder
import Data.String.Conv

data DadosCadastro = DadosCadastro { nomeCompleto :: Text, email :: Text } deriving Generic

-- instance FromJSON DadosCadastro
instance FromForm DadosCadastro
--instance ToJSON DadosCadastro

type UserAPI = "cadastrar" :> ReqBody '[FormUrlEncoded] DadosCadastro :> Post '[JSON] Bool
                :<|> Get '[HTML] Html
                :<|> Raw

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: Pool Connection -> HeistState Handler -> Server UserAPI
server connectionPool heistState = cadastrar connectionPool :<|> Main.index heistState :<|> serveDirectoryWebApp "html"

app :: Pool Connection -> HeistState Handler -> Application
app connectionPool heistState = serve userAPI (server connectionPool heistState)

-- Ideias importantes:
-- Deixar usuários livres para visualizarem os últimos 7 diários de cada tipo (garantir que não acessem diários anteriores a isso e que as URLs sejam baseadas em hashes para evitar que alguém nos copie com facilidade)
-- Mostrar todos os diários que baixamos numa lista em algum lugar no site (para que os que querem se cadastrar possam vê-los)
-- Permitir que usuários cadastrados acompanhem CONVOCAÇÕES de alguns concursos
main :: IO ()
main = do
    connectionPool <- createPool (connect connString) close 1 300 10
    heistState <- billy
    -- TODO: chamar "run" dentro de "bracket"
    run 8080 (app connectionPool heistState)
    destroyAllResources connectionPool

billy :: IO (HeistState Handler)
billy = do
    heist <- initHeist $ emptyHeistConfig
        & hcTemplateLocations .~ [ loadTemplates "templates" ]
        & hcInterpretedSplices .~ defaultInterpretedSplices
        & hcNamespace .~ ""
    case heist of
        Left errs -> error $ Prelude.foldl (++) "Error on initHeist: " errs
        Right heistState -> do
            return heistState
            -- Just (output, _) <- renderTemplate heistState "index"
            -- liftIO . BS.putStrLn . toLazyByteString $ output

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

renderBlazeHtmlOrError :: HeistState Handler -> BS.ByteString -> Handler Html
renderBlazeHtmlOrError heistState templateName = do
    outputAndMime <- renderTemplate heistState templateName
    case outputAndMime of
        Nothing -> error $ "Could not render template " ++ toS templateName
        Just (binaryBuilder, mimeType) ->
            case mimeType of
                "text/html;charset=utf-8" -> do
                    let htmlMarkup = unsafeLazyByteString . toLazyByteString $ binaryBuilder
                    -- liftIO $ print htmlMarkup
                    return $ toHtml htmlMarkup
                _           -> error $ "Template " ++ toS templateName ++ " is of non-html mime type " ++ toS mimeType

index :: HeistState Handler -> Handler Html
-- index = return (toHtml $(shamletFile "html/index.html"))
index heistState = renderBlazeHtmlOrError heistState "index"

connString :: ConnectInfo
connString = defaultConnectInfo { connectHost = "localhost", connectUser = "mzabani", connectDatabase = "epassei" }