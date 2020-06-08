module Busca where

import RIO
import qualified RIO.Char as Char
import qualified Data.Text as Text
import Data.Time
import qualified Servant
import qualified Data.Attoparsec.Text as Parsec
import qualified Data.Attoparsec.Combinator as Parsec (lookAhead)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import qualified Database.PostgreSQL.Simple.Internal as PgInternal
import Data.Pool
import DbUtils
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
runQueryWith rowParser conn (QueryFrag qry params) = liftIO $ queryWith rowParser conn qry params

createRowParser :: [Selectable] -> PgInternal.RowParser [Valor]
createRowParser s = do
    n <- numFieldsRemaining
    if RIO.length s > n then
        error "campos insuficientes para createRowParser"
    else
        sequenceA $ fmap (\(Selectable _ toValor parser) -> toValor <$> fieldWith parser) s

data Consulta = Consulta {
    _select :: [Text]
    , _where :: [FiltroBusca]
    , _grupos :: [Text]
    , _pagina :: Int
}

data OperadorSql = Igual | Menor | Maior | MenorIgual | MaiorIgual deriving Show
operadorQueryFrag :: OperadorSql -> QueryFrag
operadorQueryFrag = \case
        Igual -> "="
        Menor -> "<"
        Maior -> ">"
        MenorIgual -> "<="
        MaiorIgual -> ">="

data FiltroBusca = FiltroConteudo Text | FiltroDiario Text | FiltroData OperadorSql Text | AgruparPor [Text] deriving Show

charDePalavra :: Char -> Bool
charDePalavra c = Char.isAlpha c || Char.isDigit c || c /= '\\' && c /= '>' && c /= ':' && c /= '<' && c /= '=' && c /= ' '

parserPalavra :: Parsec.Parser Text
parserPalavra = do
    void Parsec.skipSpace
    acabou <- Parsec.atEnd
    if acabou
        then pure ""
        else do
            palavra <- Parsec.takeWhile1 charDePalavra
            void Parsec.skipSpace
            pure palavra

fimDoInputOuFiltroEspecial :: Parsec.Parser ()
fimDoInputOuFiltroEspecial = void parserFiltroDiario <|> void parserFiltroData <|> void parserGrupos <|> Parsec.endOfInput

parserFiltrosEspeciaisQualquerQtd :: Parsec.Parser [FiltroBusca]
parserFiltrosEspeciaisQualquerQtd = Parsec.many' (parserFiltroDiario <|> parserFiltroData <|> parserGrupos)

-- | Parser de 'diario:palavras filtro de diário'
parserFiltroDiario :: Parsec.Parser FiltroBusca
parserFiltroDiario = do
    Parsec.skipSpace
    void $ Parsec.asciiCI "di"
    void $ Parsec.string "á" <|> Parsec.string "Á" <|> Parsec.asciiCI "a"
    void $ Parsec.asciiCI "rio"
    Parsec.skipSpace
    void $ Parsec.char ':' <|> Parsec.char '='
    Parsec.skipSpace
    palavras <- Parsec.manyTill' parserPalavra (Parsec.lookAhead fimDoInputOuFiltroEspecial)
    when (palavras == [] || all (== "") palavras) $ fail "Digite pelo menos uma palavra para filtrar pelo nome de diário, e.g. \"diario: Campinas\""
    pure $ FiltroDiario $ Text.intercalate " " palavras

parserOperadorSql :: Parsec.Parser OperadorSql
parserOperadorSql = ((Parsec.string ":" <|> Parsec.string "=") >> pure Igual)
    <|> (Parsec.string "<=" >> pure MenorIgual)
    <|> (Parsec.string ">=" >> pure MaiorIgual)
    <|> (Parsec.string "<" >> pure Menor)
    <|> (Parsec.string ">" >> pure Maior)
    

-- | Parser de 'data:2019-02-21', 'data>2019-01-01' e similares
parserFiltroData :: Parsec.Parser FiltroBusca
parserFiltroData = do
    Parsec.skipSpace
    void $ Parsec.asciiCI "data"
    Parsec.skipSpace
    op <- parserOperadorSql
    Parsec.skipSpace
    -- TODO: Trocar por parser ISO8601
    dataStr <- parserPalavra
    when (dataStr == "") $ fail "Digite a data para filtrar no formato yyyy-mm-dd"
    pure $ FiltroData op dataStr

-- | Parser de 'grupos: coluna1 coluna2'
parserGrupos :: Parsec.Parser FiltroBusca
parserGrupos = do
    Parsec.skipSpace
    void $ Parsec.asciiCI "grupos"
    Parsec.skipSpace
    void $ Parsec.char ':' <|> Parsec.char '='
    Parsec.skipSpace
    colunas <- Parsec.manyTill' parserPalavra (Parsec.lookAhead fimDoInputOuFiltroEspecial)
    -- TODO: Falhar para qualquer coisa diferente de "data" e "diario"
    pure $ AgruparPor colunas

parserFiltroConteudo :: Parsec.Parser [FiltroBusca]
parserFiltroConteudo = do
    palavras <- Parsec.manyTill' parserPalavra (Parsec.lookAhead fimDoInputOuFiltroEspecial)
    when (palavras == [] || palavras == [""]) $ fail "É necessário digitar pelo menos uma palavra para buscar"
    filtrosEspeciais <- parserFiltrosEspeciaisQualquerQtd
    pure $ FiltroConteudo (Text.intercalate " " palavras) : filtrosEspeciais

parseConsulta :: FormBusca -> Either String Consulta
parseConsulta fb = do
    consulta <- Parsec.parseOnly parserFiltroConteudo (buscaTermo fb)
    let        
        colunasGrupos = concatMap (\case
                                        AgruparPor cols -> cols
                                        _ -> []) consulta
        filtros = RIO.filter (\case
                                        AgruparPor _ -> False
                                        _ -> True) consulta
    return $ if RIO.length colunasGrupos == 0 then
                Consulta {
                    _select = ["data", "diario", "paragrafo"],
                    _where = filtros,
                    _grupos = [],
                    _pagina = buscaPagina fb
                }
        else
                Consulta {
                    _select = colunasGrupos <> ["quantidade"],
                    _where = filtros,
                    _grupos = colunasGrupos,
                    _pagina = buscaPagina fb
                }

-- pgArrayToValor :: PGArray Text -> Valor
-- pgArrayToValor (fromPGArray -> l) = ValorLista $ fmap txtToValor l
--     where txtToValor s = case Text.splitOn "~#~" s of
--                             [] -> ValorTexto ""
--                             ss -> ValorMatch $ alternarMatches False ss
--           alternarMatches _ [] = []
--           alternarMatches negrito (x : xs) = (x, negrito) : alternarMatches (not negrito) xs

queryGrupos :: MonadIO m => FormBusca -> Connection -> m ResultadoBusca
queryGrupos fb conn = case parseConsulta fb of
    Left err -> return $ ErroBusca $ "Erro na consulta. " <> Text.drop (Text.length "Failed reading: ") (Text.pack err)
    Right consulta ->
        let 
            pegarSelectable = \case
                "data"     -> Just $ Selectable "dataDiario" (ValorTexto . Text.pack . showGregorian) fromField
                "diario"   -> Just $ Selectable "nomeDiario" ValorTexto fromField
                "paragrafo" -> Just $ Selectable "paragrafodiario.conteudo" ValorTexto fromField
                "quantidade" -> Just $ Selectable "cast(count(*) as int)" (ValorTexto . Text.pack . show) (fromField :: FieldParser Int)
                _          -> Nothing
            pegarFilterable = \case
                FiltroConteudo s -> Just $ QueryFrag "paragrafodiario.portuguese_conteudo_tsvector @@ websearch_to_tsquery('portuguese', ?)" (Only s)
                FiltroDiario   s -> Just $ QueryFrag "UNACCENT(nomeDiario) ILIKE ('%' || UNACCENT(?) || '%')" (Only s)
                FiltroData  op s -> Just $ "dataDiario " <> operadorQueryFrag op <> QueryFrag "?" (Only s)
                AgruparPor _     -> Just "true" -- TODO: GADT evitaria esse caso?
            pegarGroupable = \case
                "data"       -> Just "dataDiario"
                "diario"     -> Just "nomeDiario"
                _            -> Nothing
            
            selectablesMay  = sequenceA $ fmap pegarSelectable (_select consulta)
            filterablesMay  = sequenceA $ fmap pegarFilterable (_where consulta)
            groupablesMay   = sequenceA $ fmap pegarGroupable (_grupos consulta)
            intercalateQuery _ [] = ""
            intercalateQuery _ (x:[]) = x
            intercalateQuery separador (x:xs) = x <> separador <> intercalateQuery separador xs
        in
        case (selectablesMay, filterablesMay, groupablesMay) of
            (Nothing, _, _) -> return $ ErroBusca "Um erro interno aconteceu. Por favor reporte isso como um bug com a palavra \"selectablesMay\""
            (_, Nothing, _) -> return $ ErroBusca "Escolha algumas palavras para fazer seus filtros."
            (_, _, Nothing) -> return $ ErroBusca "Apenas algumas colunas podem ser agrupadas (e.g. \"data\" e \"diario\")"
            (Just selectables, Just filterables, Just groupables) -> do
                let
                    possuiAgrupamento = not (RIO.null groupables)
                    selectClause = intercalateQuery ", " $ fmap (\(Selectable col _ _) -> col) selectables
                    whereClause = if RIO.null filterables then "" else "WHERE " <> intercalateQuery " AND " filterables
                    groupByClause = if not possuiAgrupamento then "" else "GROUP BY " <> intercalateQuery ", " groupables
                    orderByEmAgrupado = if not possuiAgrupamento then "" else "ORDER BY " <> (intercalateQuery ", " $ fmap (\(Selectable qf@(QueryFrag col _) _ _) -> if col == "dataDiario" then "dataDiario DESC" else qf) selectables)

                    limitClause = QueryFrag " LIMIT ? " (Only (20::Int))
                    offsetClause = QueryFrag " OFFSET ? " $ Only ((_pagina consulta - 1) * 20)

                    customRowParser = createRowParser selectables
                    rowParserComAuxiliares = if not possuiAgrupamento then (\valores paragrafoId qtd -> (valores, paragrafoId, qtd)) <$> customRowParser <*> (field :: PgInternal.RowParser (Maybe Int)) <*> (field :: PgInternal.RowParser Int)
                        else (\valores _ qtdTotal -> (valores, Nothing, qtdTotal)) <$> customRowParser <*> (field :: PgInternal.RowParser (Maybe Int)) <*> (field :: PgInternal.RowParser Int)

                    conteudosCte = "conteudosDiarios (origemDiarioId, diarioId, dataDiario, nomeDiario, conteudoDiarioId, rankingRecencia) as ("
                                <> "    select diario.origemdiarioid, diario.id, diario.data, case when origemdiario.cidade is null then origemdiario.nomecompleto else origemdiario.cidade || '/' || origemdiario.estado end, dbcd.conteudodiarioid, rank() over (partition by diario.id, diario.data order by momentotermino desc) "
                                <> "       from diario join diarioabaixar on diario.id=diarioabaixar.diarioid "
                                <> "       join diarioabaixartoconteudodiario dbcd on dbcd.diarioabaixarid=diarioabaixar.id "
                                <> "       join statusdownloaddiario sdd on sdd.diarioabaixarid=diarioabaixar.id "
                                <> "       join downloadterminado on downloadterminado.statusdownloaddiarioid=sdd.id "
                                <> "       join origemdiario on diario.origemdiarioid = origemdiario.id "
                                <> "       order by diarioabaixar.diarioid, downloadterminado.momentotermino desc)"
                    
                    pquery@(QueryFrag finalQuery _) = if not possuiAgrupamento then
                                    "with " <> conteudosCte
                                <> " , resultados as (select " <> selectClause <> ", paragrafodiario.id"
                                <> "       from paragrafodiario"
                                <> "       join conteudosDiarios on conteudosDiarios.conteudoDiarioId=paragrafodiario.conteudoDiarioId and conteudosDiarios.rankingRecencia=1"
                                <> " "
                                <> whereClause
                                <> " "
                                <> "order by dataDiario desc, paragrafodiario.ordem "
                                <> "), qtdResultados (qtd) as (select cast(count(*) as int) from resultados)"
                                <> " select resultados.*, qtdResultados.qtd from resultados, qtdResultados "
                                <> offsetClause
                                <> limitClause
                        else
                                "with " <> conteudosCte
                            <> " , resultados as (select " <> selectClause <> ", cast(count(*) as int) as qtdParagrafos"
                            <> "       from paragrafodiario"
                            <> "       join conteudosDiarios on conteudosDiarios.conteudoDiarioId=paragrafodiario.conteudoDiarioId and conteudosDiarios.rankingRecencia=1"
                            <> " "
                            <> whereClause
                            <> " "
                            <> groupByClause
                            <> " "
                            <> orderByEmAgrupado
                            <> "), qtdResultados (qtd) as (select cast(sum(qtdParagrafos) as int) from resultados)"
                            <> " select resultados.*, qtdResultados.qtd from resultados, qtdResultados "
                            <> offsetClause
                            <> limitClause
                                
                liftIO $ Prelude.print finalQuery
                liftIO $ (do
                    res <- runQueryWith rowParserComAuxiliares conn pquery
                    let qtdRes = case res of
                                    [] -> 0
                                    ((_, _, x) : _) -> x
                    return $ Resultados qtdRes (Resultado {
                        colunas = _select consulta
                        , resultados = fmap (\(r, cid, _) -> (cid, r)) res
                    }))
                    `UnsafeException.catch`
                    (\(_ :: IOException) -> return $ ErroBusca "Há algo de errado com a consulta. Por favor modifique/corrija a consulta e tente novamente.")

buscaGet :: Pool Connection -> Maybe Text -> Maybe Int -> Servant.Handler ResultadoBusca
buscaGet _ Nothing _ = error "Oops"
buscaGet _ _ Nothing = error "Oops"
buscaGet connPool (Just q) (Just p) =
    withDbConnection connPool $ \conn -> queryGrupos (FormBusca q p) conn