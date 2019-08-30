module PdfParser where

import RIO
import qualified Data.Text as T
import Data.List
import System.Process.Typed
import Data.String.Conv
import qualified PdfParser.HtmlParser as PP
import qualified PdfParser.Estruturas as PP
import qualified Data.Char as C

parsePdfAntigo :: [FilePath] -> IO T.Text
parsePdfAntigo pdfFilePaths = do
    conteudoPaginas <- forM pdfFilePaths $ \path -> do
        (_, texto, _) <- readProcess (shell $ "pdftotext -enc UTF-8 " ++ path ++ " -")
        -- TODO: Se exitCode errado throw!
        return texto
    let linhasDasPaginas = concatMap (T.lines . toS) conteudoPaginas
    return $ unirLinhas linhasDasPaginas

parsePdfEmSecoes :: (MonadUnliftIO m, MonadIO m) => [FilePath] -> m [T.Text]
parsePdfEmSecoes pdfFilePaths = do
  secoesE <- PP.parsePdf pdfFilePaths
  case secoesE of
    Left e -> error $ show e
    Right secoes -> return $ fmap PP.printSecao secoes

-- Com a geração de uma página em modo table e sem modo pelo pdftotext, é preciso ler o modo table
-- e descobrir a largura da coluna da esquerda (e de outras se houver). Após descobrir a largura das colunas,
-- é possível numerar as linhas sequencialmente (onde o número das linhas das colunas à direita é maior que as da coluna à esquerda)
-- e unir palavras quebradas por hífen em linhas consecutivas.
-- A REALIDADE É BEM MAIS SIMPLES: BASTA PEGAR O NOARGS E UNIR LINHAS TERMINADAS COM HÍFEN À PRÓXIMA LINHA
-- EXCEÇÃO 1: Já vi CNPJs com hífen separando números entre uma linha e outra (onde o hífen já era esperado..), assim, se antes do hífen houver número não o removeremos
unirLinhas :: [T.Text] -> T.Text
unirLinhas linhas = fromMaybe "" $ listToMaybe $ unirHifens $ filter (not . emptyOrWhiteSpace) $ fmap T.strip linhas
  where emptyOrWhiteSpace s = T.null s || T.all C.isSpace s
        -- TODO: Bangs em unirHifens!
        unirHifens :: [T.Text] -> [T.Text]
        unirHifens [] = []
        unirHifens (l:[]) = [l]
        unirHifens (l1:l2:ls)
          | T.last l1 == '-' && not (C.isDigit (T.last l1)) = unirHifens $ T.concat [T.dropEnd 1 l1, "\n", l2] : ls
          | otherwise        = unirHifens $ T.concat [l1, "\n", l2] : ls -- TODO: E se o hífen for de separação de palavras compostas?
-- entenderPagina :: Text -> Text -> [Text]
-- entenderPagina textoDireto textoTable = linhasFinal
--   linhasTable = lines textoTable
--   linhasDireto = lines textoDireto
--   espacosContinuosTable :: Text -> [(Int, Int)]
--   espacosContinuosTable linhaTexto = let (_, xs, x) = foldl' (\(i, xs, maybeLast) c -> case maybeLast of
--                                                                         Nothing -> if c == ' ' then (i + 1, xs, Just (i, 1)) else (i + 1, xs, Nothing)
--                                                                         Just s@(firstIdx, len) -> if c == ' ' then (i + 1, xs, Just (firstIdx, len + 1)) else (i + 1, s : xs, Nothing)) (0, [], Nothing) linhaTexto
--                                      in maybeToList x ++ xs
--   espacosPorLinha = zipWith [1..] $ fmap espacosContinuosTable linhasTable
--   maioresEspacosPorLinha :: [(Int, (Int, Int))]
--   maioresEspacosPorLinha = catMaybes $ fmap (withMax snd . snd) espacosPorLinha
--   primeiraLetraSegundaColuna = median $ fmap (\(_, (i, len)) -> i + len - 1) maioresEspacosPorLinha
--   linhasFinal = fmap (\l -> AQUI PROCURAMOS UMA LINHA ) linhasDireto


withMax :: Ord a => (b -> a) -> [b] -> Maybe b
withMax f xs = snd <$> maybeRes
  where maybeRes = foldl' (\acc el -> case acc of
                                        Just (maxVal, _) -> let cmp = f el in if cmp > maxVal then Just (cmp, el) else acc
                                        Nothing              -> Just (f el, el)) Nothing xs

median :: Ord a => [a] -> Maybe a
median = fmap head . withMax length . group . sort