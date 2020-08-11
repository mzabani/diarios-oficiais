module Treinar where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import DiariosOficiais.Crawling (ProcessadorConteudoLink(..))
import qualified PdfParser.HtmlParser as PP
import qualified PdfParser.Estruturas as PP
import RIO.FilePath ((</>), (<.>))
import qualified RIO.Text as T
import qualified System.Process.Typed as Process
import UnliftIO.Directory (doesFileExist)

treinar :: FilePath -> IO ()
treinar pdfFile = do
    let fullPath = "data/diarios-oficiais" </> pdfFile
        resultadoPath = "treinamento" </> pdfFile <.> "json"
    (infoDoc, binfos) <- either id (error "Não foi possível ler o arquivo pdf") <$> PP.parseLinkBaixadoDetalhado ProcessarPdf [fullPath]
    whenM (doesFileExist resultadoPath) $ do
        resultadoEsperadoAtual <- fromMaybe (error "JSON inválido!") <$> Aeson.decodeFileStrict resultadoPath
        let matchingsAlgo = PP.mkMatching (infoDoc, binfos)
            acuracia = PP.matchingAccuracy matchingsAlgo resultadoEsperadoAtual
        putStrLn "Matching de algoritmos (atual, desejado):"
        print $ RIO.zip matchingsAlgo resultadoEsperadoAtual
        putStrLn ""
        putStrLn $ "Acurácia de matching do algoritmo atual: " <> show acuracia

    putStrLn "Para cada bloco exibido, digite:"
    putStrLn "\"m\" se este bloco pertence ao Mesmo parágrafo do anterior"
    putStrLn "\"i\" para IniciaOutroParagrafo"
    putStrLn "\"c\" para início de Cabeçalho"
    putStrLn "\"s\" para sair (a opção de salvar ou não aparecerá em seguida)"
    putStrLn ""
    matchingEsperado <- Process.withProcessWait (Process.shell $ "xdg-open \"" ++ fullPath ++ "\"") $ \_ ->
        forMUntilNothing binfos $ \binfo -> do
        let readMatch = liftIO getLine >>= \case
                                                    "m" -> return $ Just PP.DoMesmoParagrafo
                                                    "i" -> return $ Just PP.IniciaOutroParagrafo
                                                    "c" -> return $ Just PP.Cabecalho
                                                    "s" -> return Nothing
                                                    _   -> readMatch
        
        putStrLn $ T.unpack $ PP.infoTexto binfo
        
        readMatch
    
    putStrLn $ "Digite \"salvar\" para escrever este arquivo em disco em " <> resultadoPath
    cmd <- liftIO getLine
    when (cmd == "salvar") $ do
        RIO.writeFileBinary resultadoPath $ LBS.toStrict $ Aeson.encode matchingEsperado
        putStrLn $ "Arquivo com resultado esperado escrito em " <> resultadoPath
    return ()

-- | Não achei um loop como esse nem em monad-loops... :(
forMUntilNothing :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
forMUntilNothing [] _ = return []
forMUntilNothing (x:xs) f = f x >>= \case
                                      Nothing -> return []
                                      Just v  -> (v :) <$> forMUntilNothing xs f