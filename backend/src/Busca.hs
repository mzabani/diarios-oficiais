module Busca where

import RIO
import qualified Data.Text as Text
import Database.Beam
import Text.Blaze.Html
import Data.Aeson
import Data.Time
import Data.Char (isSpace)
import RIO.Map as Map
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
import Database.PostgreSQL.Simple.Types
import Data.Pool
import Text.Hamlet
import Model.Diarios
import BeamUtils
import Common

buscaGet :: Servant.Handler Html
buscaGet = return (toHtml $(shamletFile "templates/busca.hamlet"))

-- O cérebro da aplicação.
data EstruturaConsulta a b = EstruturaConsulta {
    _select :: (Text, [Text], [a], (b -> [Text])),
    _where :: (Text, [a]) -- TODO: Where deveria ser uma expressão (e.g. "valor > 5000 E valor < 10000")
}

-- TODO: Usar beam o quanto antes..
parseQueryBusca :: FormBusca -> Maybe (EstruturaConsulta a b)
parseQueryBusca fb =
    let 
        t = buscaTermo fb
        (antesSendo, aposSendo) = Text.breakOn "sendo:" t
    in
    if t == "" || Text.all isSpace t
        then Nothing
    else if aposSendo == "" then
        Just EstruturaConsulta {
            _select = ("diario.data, regexp_matches(conteudo, '(.{0,100})(' || ? || ')(.{0,100})', 'gi')", ["Data", "Conteúdo"], [ antesSendo ], \(dataDiario, PGArray matchList) -> [Text.pack (showGregorian dataDiario), Text.concat matchList]),
            _where = ("conteudo ILIKE '%' || ? || '%'", [ antesSendo ] )
        }
    else
        -- TODO: Mapa de strings do usuário -> strings seguras (e.g ("dataDiario" -> "diario.data"))
        -- Aqui não inserimos parâmetros de consulta..
        Just EstruturaConsulta {
            _select = ("STRING SEGURA A PARTIR DE Map", [], undefined, undefined),
            _where = ("", [])
        }

buscaPost :: Pool Connection -> FormBusca -> Servant.Handler ResultadoBusca
buscaPost connPool fb =
    -- TODO: Usar o mesmo Parser de documentos e procurar em tokens numéricos ou de texto
    -- por diários que tenham todos os termos buscados
    -- Para SQL: escapar buscaTermo antes de embutir na regex para evitar regexes complicadas vindo de usuários
    case parseQueryBusca fb of
        Nothing -> ErroBusca "Expressão de busca inválida"
        Just consulta ->
            withDbConnection connPool $ \conn -> do
                let (selectStr, nomesColunas, pgRowSelect, pgFromRow) = _select consulta
                    (whereStr, pgRowWhere) = _where consulta
                res <- liftIO $ query conn ("select " <> selectStr <> 
                    " from conteudodiario join diarioabaixartoconteudodiario dbcd on dbcd.conteudodiarioid = conteudodiario.id " <>
                    " join diarioabaixar on diarioabaixar.id = dbcd.diarioabaixarid " <>
                    " join diario on diario.id=diarioabaixar.diarioid " <>
                    " join origemdiario on diario.origemdiarioid = origemdiario.id " <>
                    " where " <> whereStr <> " limit 100") (pgRowSelect <> pgRowWhere)
                let rowsResult = fmap pgFromRow res
                return $ Resultados (Resultado {
                    colunas = nomesColunas,
                    valores = rowsResult
                })