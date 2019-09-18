module PdfParser.Estruturas where

import RIO
import qualified RIO.Text as Text
import qualified RIO.List as List
import Data.Char (isDigit)
import qualified Text.RE.TDFA.Text as RE
import qualified Data.Text as Text (splitOn, breakOn)
import Data.Bifunctor
import Data.String.Conv
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Aeson (ToJSON, FromJSON)

-- PARTE 1: Tipos que representam blocos, elementos inline e seus tamanhos de fonte/posição left e top

-- | Extrai o atributo "style" dos kvps forneidos e retorna as diferentes variáveis dentro de "style" (e.g. font-size, left, top)
styleAttrs :: (Eq s, IsString s, IsString v, StringConv v Text) => [(s, v)] -> [(Text, Text)]
styleAttrs kvps = bimap Text.strip (Text.drop 1 . Text.strip) . Text.breakOn ":" <$> (Text.splitOn ";" $ toS $ fromMaybe "" $ RIO.lookup "style" kvps)

class Attrs a where
    fontSize :: Num b => a -> Maybe b
    emptyAttrs :: a
    mixAttrs :: a -> a -> a
    makeAttrs :: forall s v. (Eq s, IsString s, IsString v, StringConv v Text) => [(s, v)] -> a

data AttrsBloco = AttrsBloco {
    fontSizeBloco :: Maybe Int,
    left :: Maybe Int,
    top :: Maybe Int
} deriving (Show, Eq)
instance Attrs AttrsBloco where
    fontSize = fmap fromIntegral . fontSizeBloco
    emptyAttrs = AttrsBloco Nothing Nothing Nothing
    makeAttrs kvps =
        let
            sa = styleAttrs kvps
        in AttrsBloco {
            fontSizeBloco = join $ fmap readPx $ RIO.lookup "font-size" sa,
            left = join $ fmap readPx $ RIO.lookup "left" sa,
            top = join $ fmap readPx $ RIO.lookup "top" sa
        }
    mixAttrs lowerPrecedenceAttrs higherPrecedenceAttrs = AttrsBloco {
        fontSizeBloco = fontSize higherPrecedenceAttrs <|> fontSize lowerPrecedenceAttrs,
        left = left higherPrecedenceAttrs <|> left lowerPrecedenceAttrs,
        top = top higherPrecedenceAttrs <|> top lowerPrecedenceAttrs
    }

data AttrsInline = AttrsInline {
    fontSizeInline :: Maybe Int
} deriving (Show, Eq)
instance Attrs AttrsInline where
    fontSize = fmap fromIntegral . fontSizeInline
    emptyAttrs = AttrsInline Nothing
    makeAttrs kvps =
        let
            sa = styleAttrs kvps
        in AttrsInline {
            fontSizeInline = join $ fmap readPx $ RIO.lookup "font-size" sa
        }
    mixAttrs lowerPrecedenceAttrs higherPrecedenceAttrs = AttrsInline {
        fontSizeInline = fontSize higherPrecedenceAttrs <|> fontSize lowerPrecedenceAttrs
    }

-- | Parses "38px" into (Just 38)
readPx :: Text -> Maybe Int
readPx = readMaybe . takeWhile isDigit . Text.unpack

data Page = Page {
    pageBlocos :: [Bloco]
} deriving Show

data Bloco = Bloco AttrsBloco (Either [TextEl] [Bloco]) deriving Show

atributosBloco :: Bloco -> AttrsBloco
atributosBloco (Bloco a _) = a

data TextEl = TextEl {
    atributos :: AttrsInline,
    texto :: Either Text [TextEl]
} deriving Show

textoTel :: TextEl -> Text
textoTel tel = case texto tel of
    Left t -> t
    Right tels -> Text.concat $ fmap textoTel tels

textoBloco :: Bloco -> Text
textoBloco (Bloco _ telsOuBlocos) =
    case telsOuBlocos of
        Left tels -> (Text.concat $ fmap textoTel tels) <> "\n"
        Right blcs -> Text.concat (fmap ((<> "\n") . textoBloco) blcs) <> "\n"

corpoPagina :: Page -> Text
corpoPagina (pageBlocos -> txts) = Text.concat $ fmap textoBloco txts

-- | Mostra o conteúdo com meta-dados apenas das fontes, substituindo "left: *px" por espaçamento e ignorando "top: *px"
corpoPaginaDebugFontes :: Page -> Text
corpoPaginaDebugFontes (pageBlocos -> blocos) =
    let
        printBlocoWithAttrs (Bloco (AttrsBloco { fontSizeBloco, left }) telsOrBlocos) = 
            case telsOrBlocos of
                Left tels ->
                    "[F" <> maybe "" tshow fontSizeBloco <> "]{" <> Text.replicate (maybe 0 (`div` 8) left) " " <> Text.concat (fmap printTxtElDebug tels) <> "}\n"
                Right blcs ->
                    "[F" <> maybe "" tshow fontSizeBloco <> "]{" <> Text.replicate (maybe 0 (`div` 8) left) " " <> Text.concat (fmap printBlocoWithAttrs blcs) <> "}\n"
    in Text.concat $ fmap printBlocoWithAttrs blocos

-- | Retorna o conteúdo de um TextEl com informações de tamanho de fonte (ignorando todo o resto)
printTxtElDebug :: TextEl -> Text
printTxtElDebug (TextEl { texto, atributos }) = "[F" <> maybe "" tshow (fontSizeInline atributos) <> "]" <> case texto of
    Left t -> "{" <> t <> "}"
    Right tels -> "{" <> (Text.concat $ fmap printTxtElDebug tels) <> "}"

-- | Retorna o conteúdo puro de um TextEl
printTxtEl :: TextEl -> Text
printTxtEl (TextEl { texto }) = case texto of
    Left t -> t
    Right tels -> Text.concat $ fmap printTxtEl tels

printBlocoDebug :: Bloco -> Text
printBlocoDebug (Bloco (AttrsBloco { fontSizeBloco, left, top }) telsOrBlocos) =
    case telsOrBlocos of
        Left tels ->
            "[>" <> maybe "" tshow left <> ", ^" <> maybe "" tshow top <> ", F" <> maybe "" tshow fontSizeBloco <> "]{" <> Text.concat (fmap printTxtElDebug tels) <> "}\n"
        Right blcs ->
            "[>" <> maybe "" tshow left <> ", ^" <> maybe "" tshow top <> ", F" <> maybe "" tshow fontSizeBloco <> "]{" <> Text.concat (fmap printBlocoDebug blcs) <> "}\n"

-- | Mostra o conteúdo com meta-dados dos `Attrs` todos antes de cada elemento
corpoPaginaDebug :: Page -> Text
corpoPaginaDebug (pageBlocos -> blocos) = Text.concat $ fmap printBlocoDebug blocos

-- PARTE 2: Tipos que representam o documento após processamento dos dados de altura e posição horizontal para obter informações
--          mais processadas como alinhamento de texto e se onde começam e terminam parágrafos
data AlinhamentoTexto = AEsquerda | Centralizado deriving (Show, Eq)
newtype Paragrafo = Paragrafo [TextEl] deriving Show

todosTextElsDoTextEl :: TextEl -> [TextEl]
todosTextElsDoTextEl tel = case texto tel of
    Left _ -> [tel]
    Right tels -> concatMap todosTextElsDoTextEl tels

tamanhoMedioFonteParagrafo :: Paragrafo -> Maybe Float
tamanhoMedioFonteParagrafo (Paragrafo tels) = avgFilter (fontSize . atributos) (const True) tels

printParagrafoDebug :: Paragrafo -> Text
printParagrafoDebug (Paragrafo tels) = "\n                      " <> Text.concat (fmap printTxtElDebug tels)

printParagrafo :: Paragrafo -> Text
printParagrafo (Paragrafo tels) = Text.concat (fmap printTxtEl tels)

avgFilter :: (Fractional a, Foldable t, Functor t) => (b -> Maybe a) -> (a -> Bool) -> t b -> Maybe a
avgFilter f cond l = case filter cond (catMaybes $ toList (fmap f l)) of
    [] -> Nothing
    (x:xs) -> Just $ uncurry (/) $ List.foldl' (\(num, den) n -> (num + n, den + 1)) (x,1) xs

avg :: (Real a, Fractional b, Foldable t) => t a -> Maybe b
avg l = case toList l of
    [] -> Nothing
    (x:xs) -> Just $ uncurry (\n d -> realToFrac n / d) $ List.foldl' (\(num, den) n -> (num + n, den + 1)) (x,1) xs

data BlocoEInfo = BlocoEInfo {
    infoBloco :: Bloco
    , infoTexto :: Text
    , infoTerminaComHifen :: Bool
    , infoTerminaComPontoOuPontoEVirgula :: Bool
    , infoPagina :: Int
    , infoIndexNaPagina :: Int
    , infoColuna :: Int
} deriving Show

data InfoDocumento = InfoDocumento {
    docTamanhoMedioFonte :: Float
    , docMediaAlturaLinha :: Float
} deriving Show

-- | Retorna os parágrafos todos separadinhos a partir das páginas
detalharDocumento :: [Page] -> [Paragrafo]
detalharDocumento pgs = 
    let 
        (docInfo, binfos) = produzirDadosCompletos pgs
        matchings = mkMatching (docInfo, binfos)
    in mkParagrafos binfos matchings
    
produzirDadosCompletos :: [Page] -> (InfoDocumento, [BlocoEInfo])
produzirDadosCompletos pgs = 
    let
        blocosEPgs = concatMap (\(pg, ordemPg) -> fmap (, ordemPg) (pageBlocos pg)) (zip pgs [0..])
    in
    produzirBlocosEInfo blocosEPgs

-- | Retorna os parágrafos da página de forma aproximada recebendo blocos e suas páginas
produzirBlocosEInfo :: [(Bloco, Int)] -> (InfoDocumento, [BlocoEInfo])
produzirBlocosEInfo blocosEPgs = 
    let
        todosTextElsDoBloco :: Bloco -> [TextEl]
        todosTextElsDoBloco (Bloco _ (Left tels)) = concatMap todosTextElsDoTextEl tels
        todosTextElsDoBloco (Bloco _ (Right blcs)) = concatMap todosTextElsDoBloco blcs

        todosBlocosDoBloco :: Bloco -> [Bloco]
        todosBlocosDoBloco (Bloco _ (Left _)) = []
        todosBlocosDoBloco (Bloco _ (Right blcs)) = concatMap todosBlocosDoBloco blcs

        todosTextElsDaPagina :: [TextEl]
        todosTextElsDaPagina = concatMap todosTextElsDoBloco blocos

        todosBlocosDaPagina :: [Bloco]
        todosBlocosDaPagina = concatMap todosBlocosDoBloco blocos

        blocos = fmap fst blocosEPgs
        
        -- TODO: Ideal seria analisar distribuição de valores e pegar média daqueles entre +- 2 sigma ao invés de fazer filtros arbitrários (e.g. <= 24)
        --       Nosso algoritmo para "tamanhoMedioFonte" falha se a fonte for muito grande, por exemplo (e aí todo o resto falha junto)
        tamanhoMedioFonte :: Float
        tamanhoMedioFonte = fromMaybe 9 $ avgFilter (fontSize . atributos) (<= 24) todosTextElsDaPagina
        mediaComprimentoLinha :: Float
        mediaComprimentoLinha = fromMaybe 800 $ avgFilter (Just . (*) tamanhoMedioFonte . fromIntegral . Text.length . textoBloco) (const True) todosBlocosDaPagina
        quatroQuintosCompLinha :: Float
        quatroQuintosCompLinha = (4 / 5) * mediaComprimentoLinha
        mediaAlturaLinha :: Float
        mediaAlturaLinha = fromMaybe (2 * tamanhoMedioFonte) $ avgFilter (\(Bloco a1 _, Bloco a2 _) -> realToFrac <$> ((-) <$> top a2 <*> top a1)) (\t -> t > 0 && t <= 2 * tamanhoMedioFonte) $ List.zip blocos (List.drop 1 blocos)
        
        -- Construímos BlocoEInfo numa única passada
        mkBlocoEInfo :: Bloco -> Int -> Int -> Int -> BlocoEInfo
        mkBlocoEInfo b pg pgIdx col = BlocoEInfo {
            infoBloco = b
            , infoTexto = textoBloco b
            , infoTerminaComHifen = RE.anyMatches (textoBloco b RE.*=~ [RE.re|- *\n?$|])
            , infoTerminaComPontoOuPontoEVirgula = RE.anyMatches (textoBloco b RE.*=~ [RE.re|(\.|;) *\n?$|])
            , infoPagina = pg
            , infoIndexNaPagina = pgIdx
            , infoColuna = col
        }
        blocosInfos :: [BlocoEInfo]
        blocosInfos = case blocosEPgs of
            [] -> []
            ((b1, _):xs) -> snd $
                List.foldl' (\(ultimoBInfo, todosBInfos) (bAtual, pgAtual) ->
                    let
                        a2 = atributosBloco bAtual
                        a1 = atributosBloco (infoBloco ultimoBInfo)                        
                        mesmaPagina = pgAtual == infoPagina ultimoBInfo
                        
                        diferencaAltura = realToFrac <$> ((-) <$> top a2 <*> top a1)
                        diferencaLateral :: Maybe Float = realToFrac <$> ((-) <$> left a2 <*> left a1)
                        provavelmenteAbaixoDoAnterior = ((negate mediaAlturaLinha <) <$> diferencaAltura) == Just True
                        provavelmenteMesmaColuna = ((quatroQuintosCompLinha >) . realToFrac <$> diferencaLateral) == Just True
                        mesmaColuna = provavelmenteAbaixoDoAnterior && provavelmenteMesmaColuna && pgAtual == infoIndexNaPagina ultimoBInfo
                        colIdx = if not mesmaPagina then 0
                                   else if mesmaColuna then infoColuna ultimoBInfo
                                   else infoColuna ultimoBInfo + 1
                        pgIdx = if mesmaPagina then infoIndexNaPagina ultimoBInfo + 1
                                else 0
                        bAtualInfo = mkBlocoEInfo bAtual pgAtual pgIdx colIdx
                    in
                        (bAtualInfo, todosBInfos <> [bAtualInfo])
                    )
                    (mkBlocoEInfo b1 0 0 0, [ mkBlocoEInfo b1 0 0 0 ])
                    xs

    in
        (InfoDocumento {
            docTamanhoMedioFonte = tamanhoMedioFonte
            , docMediaAlturaLinha = mediaAlturaLinha
        }, blocosInfos)

-- Nosso algoritmo compara pares de blocos consecutivos, e para cada comparação retorna um `Matching`
data Matching = DoMesmoParagrafo | Cabecalho | IniciaOutroParagrafo deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkMatching :: (InfoDocumento, [BlocoEInfo]) -> [Matching]
mkMatching (InfoDocumento {..}, binfos) =
    let
        -- Pegamos offset left médio a partir apenas os 1/3 menores (em offset) blocos para evitar os centralizados de nos atrapalharem
        offsetLeftMedioPorColuna :: [(Int, Float)]
        offsetLeftMedioPorColuna = fmap (\l@((col, _) :| _) ->
            let 
                todos = fmap snd l
                umTercoMenores = NE.take (ceiling @Float (realToFrac (NE.length todos) / 3)) todos
            in (col, fromMaybe 0 $ avg umTercoMenores)) $ NE.groupWith fst $ catMaybes $ fmap (\bi -> (,) (infoColuna bi) <$> left (atributosBloco (infoBloco bi))) binfos
        
        alinhamentoBloco :: BlocoEInfo -> AlinhamentoTexto
        alinhamentoBloco b = 
            let
                offsetLeftColunaMedio = lookup (infoColuna b) offsetLeftMedioPorColuna
                attrs = atributosBloco (infoBloco b)
                offsetLateralBloco = (-) <$> fmap realToFrac (left attrs) <*> offsetLeftColunaMedio
                maisQue2CharsAfastado = ((2 * docTamanhoMedioFonte <) <$> offsetLateralBloco) == Just True
                alig = if maisQue2CharsAfastado then Centralizado else AEsquerda
            in
                --if RE.anyMatches (infoTexto b RE.*=~[RE.re| *JONAS DONIZETTE *\n?$|]) then traceShow (offsetLateralBloco, offsetLeftColunaMedio, infoTexto b) alig else alig
                alig

    in case binfos of
            [] -> []
            [ _ ] -> [ IniciaOutroParagrafo ] -- Caso bizarro..
            _ -> fmap (\(bAnteriorInfo, bAtualInfo) ->
                        let
                            a1 = atributosBloco (infoBloco bAnteriorInfo)
                            a2 = atributosBloco (infoBloco bAtualInfo)
                            
                            
                            diferencaAltura = realToFrac <$> ((-) <$> top a2 <*> top a1)
                            separadosPorMuitaAltura = ((2 * docMediaAlturaLinha <) <$> diferencaAltura) == Just True
                            -- comprimentoLinhaAnterior = realToFrac (Text.length textoBlocoAnterior) * tamanhoMedioFonte
                            anteriorTerminaComPonto = infoTerminaComPontoOuPontoEVirgula bAnteriorInfo
                            anteriorTerminaComHifen = infoTerminaComHifen bAnteriorInfo
                            -- linhaAnteriorCurta = mediaComprimentoLinha - comprimentoLinhaAnterior >= tamanhoMedioFonte * 60
                            alinhamentoTexto = alinhamentoBloco bAtualInfo
                            alinhamentoAnterior = alinhamentoBloco bAnteriorInfo

                            -- anteriorTerminaComPonto não é bom critério. Exemplo: "Dr. Mário Gatti" com "Dr." no final de uma linha..
                            -- Parece que lidar com n-grams é inevitável
                        in
                            if not anteriorTerminaComHifen && (separadosPorMuitaAltura || anteriorTerminaComPonto || alinhamentoAnterior /= alinhamentoTexto) then
                                IniciaOutroParagrafo
                            else
                                DoMesmoParagrafo
                        )
                    (zip binfos (drop 1 binfos))

-- | Conta, comparando apenas os matchings disponíveis em ambas as listas (e desconsiderando excessos)
--   a quantidade de Blocos que pertencem aos mesmos parágrafos dividido pelo total de blocos.
--   Em caso de alguma lista de matchings ser vazia, retorna 0.
matchingAccuracy :: [Matching] -> [Matching] -> Float
matchingAccuracy ms1 ms2 =
    let (_, _, tb, tdiff) = foldl' (\(idxPar1 :: Int, idxPar2, totalBlocos :: Int, totalBlocosEmParasDiferentes :: Int) (m1, m2) ->
                                        let
                                            novoIdxPar1 = if m1 == DoMesmoParagrafo then idxPar1 else idxPar1 + 1
                                            novoIdxPar2 = if m2 == DoMesmoParagrafo then idxPar2 else idxPar2 + 1
                                        in
                                        (novoIdxPar1, novoIdxPar2, totalBlocos + 1, if novoIdxPar1 == novoIdxPar2 then totalBlocosEmParasDiferentes else totalBlocosEmParasDiferentes + 1))
                                    (-1, -1, 0, 0)
                                    (zip ms1 ms2)
    in if tb == 0 then 0 else 1 - realToFrac tdiff / realToFrac tb

-- | Retorna os parágrafos da página de forma aproximada
mkParagrafos :: [BlocoEInfo] -> [Matching] -> [Paragrafo]
mkParagrafos binfos matchings =
    if length binfos /= length matchings
        then error "Comprimentos dos matchings e binfos não bate! Queria que essa verificação fosse em tempo de compilação mas deixa pra outra hora.."
        else 
            case zip binfos matchings of
                [] -> []
                ((b1, _):xs) -> 
                    let
                        adicionarLineBreakAoUltimoTel :: [TextEl] -> [TextEl]
                        adicionarLineBreakAoUltimoTel [] = []
                        adicionarLineBreakAoUltimoTel (ultimoTel : []) = case ultimoTel of
                            TextEl attrs (Left t) ->
                                if RE.anyMatches (t RE.*=~ [RE.re|- *\n?$|]) 
                                    then [ TextEl attrs $ Left (t RE.?=~/ [RE.ed|- *\n?$///|]) ]
                                    else [ TextEl attrs $ Left (t <> " ") ]
                            TextEl attrs (Right tels) -> [ TextEl attrs $ Right (adicionarLineBreakAoUltimoTel tels) ]
                        adicionarLineBreakAoUltimoTel (t:ts) = t : adicionarLineBreakAoUltimoTel ts
    
                        textElsRaizDoBlocoComLineBreak :: Bloco -> [TextEl]
                        textElsRaizDoBlocoComLineBreak (Bloco _ (Left tels)) = adicionarLineBreakAoUltimoTel tels
                        textElsRaizDoBlocoComLineBreak (Bloco _ (Right blcs)) = concatMap textElsRaizDoBlocoComLineBreak blcs
                        
                        (ultimoParagrafo, paragrafosAnteriores) =
                            List.foldl' (\(pEmConstrucao@(Paragrafo telsPConstr), todosPs) (bAtualInfo, matching) ->
                                let bAtual = infoBloco bAtualInfo
                                in
                                if matching == Cabecalho || matching == IniciaOutroParagrafo then
                                        (Paragrafo (textElsRaizDoBlocoComLineBreak bAtual), todosPs <> [pEmConstrucao])
                                else
                                    (Paragrafo (telsPConstr <> textElsRaizDoBlocoComLineBreak bAtual), todosPs))
                                (Paragrafo (textElsRaizDoBlocoComLineBreak (infoBloco b1)), [])
                                xs
                        in
                            paragrafosAnteriores <> [ultimoParagrafo]