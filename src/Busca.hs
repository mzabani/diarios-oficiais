module Busca where

import RIO
import Database.Beam
import Text.Blaze.Html
import Data.Aeson
import Servant
import Servant.API
import Servant.HTML.Blaze
import Database.Beam.Backend.SQL.BeamExtensions
import qualified Database.Beam.Postgres as Pg
import ServantExtensions
import Network.Wai
import Network.Wai.Handler.Warp
import GHC.Generics
import Web.FormUrlEncoded
import Database.PostgreSQL.Simple
import Data.Pool
import Text.Hamlet
import Model.Diarios
import BeamUtils

data FormBusca = FormBusca {
    buscaTermo :: Text
} deriving Generic
instance FromForm FormBusca

data ResultadoBusca = ResultadoBusca {
    trecho :: Text
} deriving (Generic, ToJSON)

buscaGet :: Servant.Handler Html
buscaGet = return (toHtml $(shamletFile "templates/busca.hamlet"))

buscaPost :: Pool Connection -> FormBusca -> Servant.Handler [ ResultadoBusca ]
buscaPost connPool fb = withDbConnection connPool $ \conn -> do
    res <- liftIO $ Pg.runBeamPostgresDebug Prelude.putStrLn conn $
                runSelectReturningList $ select $ 
                    filter_ (\cd -> Pg.ilike_ (conteudodiarioConteudo cd) (val_ ("%" <> buscaTermo fb <> "%"))) $ limit_ 10 (all_ (conteudosDiarios diariosDb))
    return $ fmap (\cd -> ResultadoBusca {
        trecho = conteudodiarioMd5Sum cd
    }) res