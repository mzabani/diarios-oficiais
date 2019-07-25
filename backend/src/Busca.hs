module Busca where

import RIO
import qualified Data.Text as Text
import Database.Beam
import Text.Blaze.Html
import Data.Aeson
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import Data.Time
import Data.Maybe (catMaybes)
import Data.Char (isSpace)
import RIO.Map as Map
import qualified Servant
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
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.FromRow
import qualified Database.PostgreSQL.Simple.Internal as PgInternal
import Data.Pool
import Text.Hamlet
import Model.Diarios
import BeamUtils
import Common

buscaGet :: Servant.Handler Html
buscaGet = return (toHtml $(shamletFile "templates/busca.hamlet"))

-- | Selectable armazena um FieldParser para extração dos resultados, uma expressão para dentro do SELECT e 
-- parâmetros para o caso de precisar fornecer parâmetros para a Query
data Selectable = forall a. (FromField a) => Selectable QueryFrag (a -> Valor) (FieldParser a)

data QueryFrag = forall a. ToRow a => QueryFrag Query a

instance IsString QueryFrag where
    fromString s = QueryFrag (fromString s) ()
instance Semigroup QueryFrag where
    QueryFrag q1 p1 <> QueryFrag q2 p2 = QueryFrag (q1 <> q2) (p1 :. p2)

runQueryWith :: MonadIO m => PgInternal.RowParser r -> Connection -> QueryFrag -> m [r]
runQueryWith rowParser conn (QueryFrag query params) = liftIO $ queryWith rowParser conn query params

createRowParser :: [Selectable] -> PgInternal.RowParser [Valor]
createRowParser s = do
    n <- numFieldsRemaining
    if RIO.length s /= n then
        error "oops"
    else
        sequenceA $ fmap (\(Selectable _ toValor parser) -> toValor <$> fieldWith parser) s

data Consulta = Consulta {
    _select :: [Text]
    , _where :: Text
    , _grupos :: [Text]
    -- TODO: Adicionar order by
}

parseConsulta :: FormBusca -> Consulta
parseConsulta fb =
    let 
        t = buscaTermo fb
        (antesGrupos, Text.strip . Text.drop 7 -> aposGrupos) = Text.breakOn "grupos:" t
        (antesSendo, Text.strip . Text.drop 6 -> aposSendo) = Text.breakOn "sendo:" antesGrupos
        colunasSelect = RIO.filter (/="") $ fmap Text.strip $ Text.split (== ',') antesSendo
        colunasGrupos = RIO.filter (/="") $ fmap Text.strip $ Text.split (== ',') aposGrupos
    in Consulta {
        _select = colunasSelect,
        _where = aposSendo,
        _grupos = colunasGrupos
    }

-- pgArrayToValorMatch :: PGArray Text -> Valor
-- pgArrayToValorMatch (fromPGArray -> l) = case l of
--     [antes, match, depois] -> ValorMatch antes match depois
--     _                      -> ValorTexto (Text.concat l)

pgArrayToValor :: PGArray Text -> Valor
pgArrayToValor (fromPGArray -> l) = ValorTexto (Text.concat l)

queryGrupos :: MonadIO m => FormBusca -> Connection -> m ResultadoBusca
queryGrupos fb conn =
            let 
                consulta = parseConsulta fb
                filtroConteudo = _where consulta
                comGroupBy = _grupos consulta /= []
                -- TODO: Unaccent
                pegarSelectable = \case
                    "data"     -> Just $ Selectable "diario.data" (ValorTexto . Text.pack . showGregorian) fromField
                    "diario"   -> Just $ Selectable "case when origemdiario.cidade is null then origemdiario.nomecompleto else origemdiario.cidade || '/' || origemdiario.estado end" ValorTexto fromField
                    "conteudo" ->
                        if filtroConteudo == "" then Nothing
                        -- ^ Não mostramos todo o conteúdo de uma seção, naturalmente
                        -- else if comGroupBy then Just $ Selectable "regexp_split_to_array(ts_headline('portuguese', secaodiario.conteudo, plainto_tsquery('portuguese', ?), 'MinWords=1,MaxWords=15,MaxFragments=99999,FragmentDelimiter=~@~'), '~@~')" (ValorLista . fmap pgArrayToValorMatch . fromPGArray) fromField
                        -- ^ Queremos mostrar todos os matches se houver agrupamento e filtro de conteúdo
                        else Just $ Selectable (QueryFrag "regexp_split_to_array(ts_headline('portuguese', secaodiario.conteudo, plainto_tsquery('portuguese', ?), 'MinWords=1,MaxWords=15,MaxFragments=99999,FragmentDelimiter=~@~'), '~@~')" (Only filtroConteudo)) pgArrayToValor fromField
                        -- ^ Se não houver group by mostramos um match por resultado

                    -- Colunas para quando há agrupamento
                    "qtd"      -> Just $ Selectable "count(*)" (ValorTexto . Text.pack . show) (fromField :: FieldParser Integer)
                    _          -> Nothing
                pegarFilterable = \case
                    "" -> Just "true"
                    s  -> Just $ QueryFrag "to_tsvector('portuguese', secaodiario.conteudo) @@ plainto_tsquery('portuguese', ?)" (Only s)
                pegarGroupable = \case
                    "data"       -> Just "data"
                    "diario"     -> Just "origemdiario.id, origemdiario.nomecompleto, origemdiario.cidade, origemdiario.estado"
                    _            -> Nothing
                selectablesMay  = sequenceA $ fmap pegarSelectable (_select consulta)
                filterablesMay  = sequenceA $ fmap pegarFilterable [filtroConteudo]
                groupablesMay   = sequenceA $ fmap pegarGroupable (_grupos consulta)
                intercalateQuery _ [] = ""
                intercalateQuery _ (x:[]) = x
                intercalateQuery separador (x:xs) = x <> separador <> intercalateQuery separador xs
            in
            case (selectablesMay, filterablesMay, groupablesMay) of
                -- TODO: Retornar status http erro apropriado em caso de query inválida
                (Nothing, _, _) -> return $ ErroBusca "Lembre-se de selecionar alguma coluna (e.g. \"data\", \"conteudo\") e de usar um filtro como \"sendo:conteudo~habite\""
                (_, Nothing, _) -> return $ ErroBusca "Lembre-se de selecionar filtrar colunas corretamente (e.g. \"data=2010-12-25\", \"conteudo~exonerado\")"
                (_, _, Nothing) -> return $ ErroBusca "Apenas algumas colunas podem ser agrupadas (e.g. data)"
                (Just selectables, Just filterables, Just groupables) -> do
                    let
                        selectClause = intercalateQuery ", " $ fmap (\(Selectable col _ _) -> col) selectables
                        whereClause = if RIO.null filterables then "" else "where " <> (intercalateQuery " AND " filterables)
                        groupByClause = if RIO.null groupables then "" else "group by " <> intercalateQuery ", " groupables

                        customRowParser = createRowParser selectables
                        pquery@(QueryFrag query _) = "select " <> selectClause 
                                    <> " from secaodiario"
                                    <> " join conteudodiario on conteudodiario.id = secaodiario.conteudodiarioid"
                                    <> " join diarioabaixartoconteudodiario dbcd on dbcd.conteudodiarioid = conteudodiario.id "
                                    <> " join diarioabaixar on diarioabaixar.id = dbcd.diarioabaixarid "
                                    <> " join diario on diario.id=diarioabaixar.diarioid "
                                    <> " join origemdiario on diario.origemdiarioid = origemdiario.id "
                                    <> whereClause
                                    <> " "
                                    <> groupByClause
                                    <> " order by diario.data desc limit 100"
                                    -- TODO: Pensar em paginação
                    liftIO $ Prelude.print query
                    --res <- liftIO $ queryWith customRowParser conn query valuesToInject
                    res <- runQueryWith customRowParser conn pquery
                    return $ Resultados (Resultado {
                        colunas = _select consulta,
                        resultados = res
                    })

buscaPost :: Pool Connection -> FormBusca -> Servant.Handler ResultadoBusca
buscaPost connPool fb =
    -- TODO: Usar o mesmo Parser de documentos e procurar em tokens numéricos ou de texto
    -- por diários que tenham todos os termos buscados
    withDbConnection connPool $ \conn -> queryGrupos fb conn