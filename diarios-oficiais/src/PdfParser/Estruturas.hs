module PdfParser.Estruturas where

import RIO
import qualified RIO.Text as Text
import qualified RIO.List as List
import Data.Char (isDigit)
import qualified Text.RE.TDFA.Text as RE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as Text (splitOn, breakOn)
import Data.Bifunctor
import Data.String.Conv

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
data DocumentoDetalhado = DocumentoDetalhado {
    documentoTamanhoMedioFonte :: Float,
    documentoPaginas :: [PaginaDetalhada]
}
data PaginaDetalhada = PaginaDetalhada {
    paginaTamanhoMedioFonte :: Float,
    paginaParagrafos :: [Paragrafo]
}
data AlinhamentoTexto = AEsquerda | Centralizado deriving (Show, Eq)
data Paragrafo = Paragrafo AlinhamentoTexto [TextEl] deriving Show

alinhamentoParagrafo :: Paragrafo -> AlinhamentoTexto
alinhamentoParagrafo (Paragrafo al _) = al

todosTextElsDoTextEl :: TextEl -> [TextEl]
todosTextElsDoTextEl tel = case texto tel of
    Left _ -> [tel]
    Right tels -> concatMap todosTextElsDoTextEl tels

tamanhoMedioFonteParagrafo :: Paragrafo -> Maybe Float
tamanhoMedioFonteParagrafo (Paragrafo _ tels) = avgFilter (fontSize . atributos) (const True) tels

printParagrafoDebug :: Paragrafo -> Text
printParagrafoDebug (Paragrafo align tels) = "\n                      " <> Text.pack (show align) <> "\n" <> Text.concat (fmap printTxtElDebug tels)

printParagrafo :: Paragrafo -> Text
printParagrafo (Paragrafo _ tels) = Text.concat (fmap printTxtEl tels)

avgFilter :: (Fractional a, Foldable t, Functor t) => (b -> Maybe a) -> (a -> Bool) -> t b -> Maybe a
avgFilter f cond l = case filter cond (catMaybes $ toList (fmap f l)) of
    [] -> Nothing
    (x:xs) -> Just $ uncurry (/) $ List.foldl' (\(num, den) n -> (num + n, den + 1)) (x,1) xs

avg :: (Real a, Fractional b, Foldable t) => t a -> Maybe b
avg l = case toList l of
    [] -> Nothing
    (x:xs) -> Just $ uncurry (\n d -> realToFrac n / d) $ List.foldl' (\(num, den) n -> (num + n, den + 1)) (x,1) xs

-- | Retorna um documento detalhado a partir de suas páginas
detalharDocumento :: [Page] -> DocumentoDetalhado
detalharDocumento pgs = 
    let
        -- TODO: Por hora calculamos uma média das médias de cada página para a fonte e não uma média geral do documento..
        pgsDetalhadas = fmap detalharPagina pgs
    in
    DocumentoDetalhado {
        documentoTamanhoMedioFonte = fromMaybe 9 $ avgFilter (Just . paginaTamanhoMedioFonte) (const True) pgsDetalhadas,
        documentoPaginas = pgsDetalhadas
    }


-- | Retorna os parágrafos da página de forma aproximada
detalharPagina :: Page -> PaginaDetalhada
detalharPagina (pageBlocos -> blocos) = 
    -- -- Assim que a altura cair por mais de 5 elementos consecutivos e subir novamente para um bloco posicionado o suficiente à esquerda, temos a mudança de coluna
    -- Colunas são formadas por blocos consecutivos cujo "left" é próximo o suficiente comparado ao tamanho médio das linhas da página
    -- IMPORTANTE: O posicionamento suficiente à esquerda é importante por conta de tabelas, já que a ordem dos blocos em tabelas é por coluna e não por linha.. cuidado!
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
        
        snoc :: a -> NonEmpty a -> NonEmpty a
        snoc v (x :| xs) = x :| xs ++ [v]
        
        -- -- 1. Primeiro separamos em colunas. Isso é útil por conta de detecção de larguras
        blocosPorColuna :: [NonEmpty Bloco]
        blocosPorColuna = case blocos of
            [] -> []
            (x:xs) ->
                let
                    (_, colunaFinal, outrasColunas) =
                        List.foldl' (\(Bloco a1 _, ultimaColuna, demaisColunas) bAtual@(Bloco a2 _) ->
                            let
                                diferencaAltura = realToFrac <$> ((-) <$> top a2 <*> top a1)
                                diferencaLateral :: Maybe Float = realToFrac <$> ((-) <$> left a2 <*> left a1)
                                provavelmenteAbaixoDoAnterior = ((negate mediaAlturaLinha <) <$> diferencaAltura) == Just True
                                provavelmenteMesmaColuna = ((quatroQuintosCompLinha >) . realToFrac <$> diferencaLateral) == Just True
                            in
                                if provavelmenteAbaixoDoAnterior && provavelmenteMesmaColuna then
                                    (bAtual, snoc bAtual ultimaColuna, demaisColunas)
                                else
                                    (bAtual, bAtual :| [], demaisColunas <> [ultimaColuna])
                                )
                            (x, x :| [], [])
                            xs
                in
                    outrasColunas <> [colunaFinal]

        -- 2. Queremos a separação por parágrafos
        paragrafos :: [[Paragrafo]]
        paragrafos = fmap mp blocosPorColuna
            where
                -- TODO: Unir hífens no fim de TextEl com o TextEl da próxima linha
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

                mp :: NonEmpty Bloco -> [Paragrafo]
                mp bs@(x :| xs) =
                    let
                        -- Pegamos offset left médio a partir apenas de 1/3 dos blocos para evitar os centralizados de nos atrapalharem
                        offsetLeftColunaMedio :: Maybe Float
                        offsetLeftColunaMedio = avg $ List.take (floor ((realToFrac (RIO.length bs) :: Float) / 3)) $ List.sort $ catMaybes $ toList $ fmap (left . atributosBloco) bs
                        alinhamentoBloco :: Bloco -> AlinhamentoTexto
                        alinhamentoBloco b@(Bloco attrs _) = 
                            let
                                offsetLateralBloco = (-) <$> fmap realToFrac (left attrs) <*> offsetLeftColunaMedio
                                maisQue2CharsAfastado = ((2 * tamanhoMedioFonte <) <$> offsetLateralBloco) == Just True
                                alig = if maisQue2CharsAfastado then Centralizado else AEsquerda
                            in
                                if RE.anyMatches (textoBloco b RE.*=~[RE.re| *JONAS DONIZETTE *\n?$|]) then traceShow (offsetLateralBloco, offsetLeftColunaMedio, textoBloco b) alig else alig
                        
                        (_, ultimoParagrafo, paragrafosAnteriores) =
                            List.foldl' (\(bAnterior@(Bloco a1 _), pEmConstrucao@(Paragrafo alPConstr telsPConstr), todosPs) bAtual@(Bloco a2 _) ->
                                let
                                    textoBlocoAnterior = textoBloco bAnterior
                                    diferencaAltura = realToFrac <$> ((-) <$> top a2 <*> top a1)
                                    separadosPorMuitaAltura = ((2 * mediaAlturaLinha <) <$> diferencaAltura) == Just True
                                    -- comprimentoLinhaAnterior = realToFrac (Text.length textoBlocoAnterior) * tamanhoMedioFonte
                                    anteriorTerminaComPonto = RE.anyMatches (textoBlocoAnterior RE.*=~ [RE.re|(\.|;) *\n?$|])
                                    anteriorTerminaComHifen = RE.anyMatches (textoBlocoAnterior RE.*=~ [RE.re|- *\n?$|])
                                    -- linhaAnteriorCurta = mediaComprimentoLinha - comprimentoLinhaAnterior >= tamanhoMedioFonte * 60
                                    alinhamentoTexto = alinhamentoBloco bAtual

                                    -- anteriorTerminaComPonto não é bom critério. Exemplo: "Dr. Mário Gatti" com "Dr." no final de uma linha..
                                    -- Parece que lidar com n-grams é inevitável
                                in
                                    if not anteriorTerminaComHifen && (separadosPorMuitaAltura || anteriorTerminaComPonto || alPConstr /= alinhamentoTexto) then
                                        (bAtual, Paragrafo alinhamentoTexto (textElsRaizDoBlocoComLineBreak bAtual), todosPs <> [pEmConstrucao])
                                    else
                                        (bAtual, Paragrafo alPConstr (telsPConstr <> textElsRaizDoBlocoComLineBreak bAtual), todosPs))
                                (x, Paragrafo (alinhamentoBloco x) (textElsRaizDoBlocoComLineBreak x), [])
                                xs
                    in
                        paragrafosAnteriores <> [ultimoParagrafo]
    in
        PaginaDetalhada { paginaTamanhoMedioFonte = tamanhoMedioFonte, paginaParagrafos = mconcat paragrafos }

-- PARTE 3: Separação em Seções. A ideia é que cada Seção seja a menor unidade possível que trate de um tema específico, mas o mais importante agora
--          é apenas que Seções diferentes não possuam conteúdo do mesmo tópico e que sejam pequenas o suficiente para caberem num TSVECTOR do postgres
-- NOTA: A SEPARAÇÃO EM SEÇÕES JÁ NÃO É IMPORTANTE. A BUSCA ACONTECE NOS PARÁGRAFOS. AQUI PODERÍAMOS

data Secao = Secao { secaoConteudo :: [Paragrafo] }
printSecao :: Secao -> Text
printSecao (secaoConteudo -> ps) = Text.concat $ fmap printParagrafo ps

obterSecoes :: DocumentoDetalhado -> [Secao]
obterSecoes doc = 
    let
        todosPs = concatMap paginaParagrafos (documentoPaginas doc)
        tamanhoFonteSecao = 1.5 * (documentoTamanhoMedioFonte doc)
        -- TODO: Parágrafos podem dobrar páginas
    in
        case todosPs of
            [] -> []
            (x:xs) ->
                let
                    (ultimaSecao, outrasSecoes) =
                        List.foldl' (\(secaoEmConstrucao@(Secao psConstrucao), demaisSecoes) p ->
                            let
                                qtdPsSecao = List.length psConstrucao
                                tamanhoFonteP = fromMaybe 9 $ tamanhoMedioFonteParagrafo p
                                tamanhoFonteUltimoP = fromMaybe 9 $ avgFilter tamanhoMedioFonteParagrafo (< tamanhoFonteSecao) psConstrucao
                                fonteMuitoMaiorQueUltimoP = tamanhoFonteP - tamanhoFonteUltimoP >= 1
                                fonteGigante = tamanhoFonteP > 4 * tamanhoFonteSecao -- Fonte gigante provavelmente significa cabeçalho de página, não início de seção
                            in
                                if qtdPsSecao == 1 || tamanhoFonteP < tamanhoFonteSecao || not fonteMuitoMaiorQueUltimoP || fonteGigante then
                                    (Secao (psConstrucao <> [p]), demaisSecoes)
                                else
                                    (Secao [p], demaisSecoes <> [secaoEmConstrucao]))
                        (Secao [x], [])
                        xs
                in
                    outrasSecoes <> [ultimaSecao]