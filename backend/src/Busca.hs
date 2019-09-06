module Busca where

import RIO
import qualified Data.Text as Text
import Database.Beam
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
import Model.Diarios
import BeamUtils
import Common
import qualified Control.Exception as UnsafeException

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
    if RIO.length s > n then
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

pgArrayToValor :: PGArray Text -> Valor
pgArrayToValor (fromPGArray -> l) = ValorLista $ fmap txtToValor l
    where txtToValor s = case Text.splitOn "~#~" s of
                            [] -> ValorTexto ""
                            ss -> ValorMatch $ alternarMatches False ss
          alternarMatches _ [] = []
          alternarMatches negrito (x : xs) = (x, negrito) : alternarMatches (not negrito) xs

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
                    "paragrafo" ->
                        if filtroConteudo == "" then Nothing
                        -- ^ Não mostramos todo o conteúdo de uma seção, naturalmente
                        -- else if comGroupBy then Just $ Selectable "regexp_split_to_array(ts_headline('portuguese', paragrafodiario.conteudo, plainto_tsquery('portuguese', ?), 'MinWords=1,MaxWords=15,MaxFragments=99999,FragmentDelimiter=~@~'), '~@~')" (ValorLista . fmap pgArrayToValorMatch . fromPGArray) fromField
                        -- ^ Queremos mostrar todos os matches se houver agrupamento e filtro de conteúdo
                        -- else Just $ Selectable (QueryFrag "regexp_split_to_array(ts_headline('portuguese', paragrafodiario.conteudo, plainto_tsquery('portuguese', ?), 'MinWords=1,MaxWords=15,MaxFragments=99999,FragmentDelimiter=~@~,StartSel=~#~,StopSel=~#~'), '~@~')" (Only filtroConteudo)) pgArrayToValor fromField
                        else Just $ Selectable "paragrafodiario.conteudo" ValorTexto fromField
                        -- ^ Se não houver group by mostramos um match por resultado

                    -- Colunas para quando há agrupamento
                    "qtd"      -> Just $ Selectable "count(*)" (ValorTexto . Text.pack . show) (fromField :: FieldParser Integer)
                    _          -> Nothing
                pegarFilterable = \case
                    "" -> Just "true"
                    s  -> Just $ QueryFrag "paragrafodiario.portuguese_conteudo_tsvector @@ plainto_tsquery('portuguese', ?)" (Only s)
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
                (Nothing, _, _) -> return $ ErroBusca "Lembre-se de selecionar alguma coluna (e.g. \"data\", \"paragrafo\") e de usar um filtro como \"sendo:paragrafo~habite\""
                (_, Nothing, _) -> return $ ErroBusca "Lembre-se de selecionar filtrar colunas corretamente (e.g. \"data=2010-12-25\", \"paragrafo~exonerado\")"
                (_, _, Nothing) -> return $ ErroBusca "Apenas algumas colunas podem ser agrupadas (e.g. data)"
                (Just selectables, Just filterables, Just groupables) -> do
                    let
                        selectClause = intercalateQuery ", " $ fmap (\(Selectable col _ _) -> col) selectables
                        whereClause = if RIO.null filterables then "" else "where " <> intercalateQuery " AND " filterables
                        groupByClause = if RIO.null groupables then "" else "group by " <> intercalateQuery ", " groupables

                        -- TODO
                        -- 1. Pensar em paginação
                        -- 2. Cada linha retornada pela query (a não ser que haja agrupamento) deve ter apenas um match. As seções de diário não devem transparecer ao usuário (são artifício interno de indexação)
                        -- 3. Matches com regex são bem mais úteis para fins de pesquisa. No entanto, regexp_matches é uma função que pesa MUITO até mesmo
                        --    para a regex '(.{0,100})(termos buscados)(.{0,100})'. Acho que uma ideia inicial é limitar a freq. de buscas por IP..
                        --    Ou contar ocorrências de palavras buscadas
                        -- 4. Linguagem simplificada apenas com condição WHERE para o usuário. Automaticamente incluir "data, diario, paragrafo" no SELECT
                        -- 5. De forma mais abrangente: como indexar texto para permitir buscas regex eficientes?

                        customRowParser = createRowParser $ selectables
                        rowParserComOrdemEQtd = (\valores ordem qtd -> (valores, ordem, qtd)) <$> customRowParser <*> (field :: PgInternal.RowParser Int) <*> (field :: PgInternal.RowParser Int)
                        -- <> [Selectable "QUALQUERCOISA" (ValorTexto . Text.pack . show) (fromField :: FieldParser Integer), Selectable "QUALQUERCOISA" (ValorTexto . Text.pack . show) (fromField :: FieldParser Integer)]
                        pquery@(QueryFrag query _) = "with resultados as (select " <> selectClause <> ", paragrafodiario.ordem as ordem"
                                    <> " from paragrafodiario"
                                    <> " join conteudodiario on conteudodiario.id = paragrafodiario.conteudodiarioid"
                                    <> " join diarioabaixartoconteudodiario dbcd on dbcd.conteudodiarioid = conteudodiario.id"
                                    <> " join diarioabaixar on diarioabaixar.id = dbcd.diarioabaixarid"
                                    <> " join diario on diario.id=diarioabaixar.diarioid"
                                    <> " join origemdiario on diario.origemdiarioid = origemdiario.id"
                                    <> " "
                                    <> whereClause
                                    <> " "
                                    <> groupByClause
                                    <> "), qtdResultados (qtd) as (select cast(count(*) as int) from resultados)"
                                    <> " select resultados.*, qtdResultados.qtd from resultados, qtdResultados order by data desc, ordem limit 20"
                                    
                    liftIO $ Prelude.print query
                    liftIO $ (do
                        res <- runQueryWith rowParserComOrdemEQtd conn pquery
                        let qtdRes = case res of
                                        [] -> 0
                                        ((_, _, x) : _) -> x
                        return $ Resultados (Resultado {
                            colunas = _select consulta,
                            resultados = fmap (\(r, _, _) -> r) res
                        }))
                        `UnsafeException.catch`
                        (\(e :: IOException) -> return $ ErroBusca "Há algo de errado com a consulta. Por favor modifique/corrija a consulta e tente novamente.")

buscaPost :: Pool Connection -> FormBusca -> Servant.Handler ResultadoBusca
buscaPost connPool fb =
    withDbConnection connPool $ \conn -> queryGrupos fb conn