module DiariosOficiais (start) where

import Model.Diarios
import Brdocs
import BeamUtils
import DiariosOficiais.Crawling
import DiariosOficiais.Database
import RIO
import UnliftIO.Environment
import DbVcs (bringDbUpToDate)
import Data.Time
import Data.Pool
import qualified Database.PostgreSQL.Simple as PGS
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.Trans.Resource
import System.Directory
import Crypto.Hash
import Crypto.Hash.Conduit
import qualified System.Process.Typed as Process
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import System.FilePath
import Data.Conduit
import Data.Conduit.Binary
import Data.String.Conv
import Database.Beam
import Network.HTTP.Conduit
import qualified PdfParser.HtmlParser as PP
import qualified PdfParser.Estruturas as PP
import Buscador
import qualified Data.Foldable as Fold
import qualified Crawlers.Campinas as CrawlerCampinas
import qualified Data.Text as T
import qualified Data.Aeson as Aeson

allCrawlers :: [Crawler]
-- Só Campinas funciona por enquanto
--allCrawlers = [toCrawler CrawlerSumare.SumareCrawler, toCrawler CrawlerCampinas.CampinasCrawler, toCrawler CrawlersDOU.DOU1Crawler]
allCrawlers = [toCrawler CrawlerCampinas.CampinasCrawler]

hoje :: MonadIO m => m Day
hoje = liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime

utcNow :: MonadIO m => m UTCTime
utcNow = liftIO $ zonedTimeToUTC <$> getZonedTime

-- ^ Downloads the URL supplied and saves into the supplied directory, returning the full path of the saved file, whose
-- name is the file's MD5 hash
downloadToAsMd5 :: Text -> FilePath -> Manager -> IO (FilePath, Digest MD5)
downloadToAsMd5 url dir mgr = do
  req <- parseRequest $ toS url
  runResourceT $ do
    resSource <- responseBody <$> http req mgr
    RIO.withSystemTempFile "concpub.file" $ \fp fileHandle -> do
      (fileHash, tempPath) <- sealConduitT resSource $$+- (getZipSink $ (,) <$> ZipSink sinkHash <*> ZipSink (sinkHandleAndClose fileHandle >> return fp))
      let finalPath = dir </> show (fileHash :: Digest MD5)
      -- Importante: renameFile pode falhar caso destino esteja em outro disco. Por isso usamos copyFile!
      liftIO $ print tempPath
      liftIO $ print finalPath
      liftIO $ copyFile tempPath finalPath
      liftIO $ removeFile tempPath
      return (finalPath, fileHash)
  where sinkHandleAndClose handle' = sinkIOHandle (return handle')


-- | Não achei um loop como esse nem em monad-loops... :(
forMUntilNothing :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
forMUntilNothing [] _ = return []
forMUntilNothing (x:xs) f = f x >>= \case
                                      Nothing -> return []
                                      Just v  -> (v :) <$> forMUntilNothing xs f

start :: IO ()
start = do
  putStrLn "Passe a opção \"fetch\" para baixar todos os diários dos últimos 365 dias e não passe opção nenhuma para baixar continuamente diários"
  let mgrSettings = Http.tlsManagerSettings { Http.managerModifyRequest = \req -> return req { requestHeaders = [("User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/73.0.3683.86 Safari/537.36")] } }
  mgr <- newManager mgrSettings
  basePath <- fromMaybe "./data/diarios-oficiais/" <$> lookupEnv "DIARIOSDIR"
  dbVcsInfo <- getDbVcsInfo
  -- Apply DB migrations if necessary
  bringDbUpToDate dbVcsInfo
  bracket (createDbPool 1 60 50) destroyAllResources $ \dbPool -> do
    createDirectoryIfMissing False basePath
    -- awsConfig <- createAwsConfiguration
    let ctx = AppContext {
      mgr = mgr,
      dbPool = dbPool,
      basePath = basePath
      -- awsConfig = awsConfig
    }
    hj <- hoje
    args <- getArgs
    case args of
      [ "fetch" ] ->
        forM_ [0..365] $ \i -> do
          let dt = addDays ((-1) * i) hj
          Fold.forM_ allCrawlers $ \sub -> downloadEIndexar dt ctx sub

      [ "reindexar" ] ->
        error "Precisamos ser capazes de ler os PDFs originais e reinserir os parágrafos deles sem baixá-los dos sites!"
        
      [ "treinar", pdfFile ] -> do
        let fullPath = "data/diarios-oficiais" </> pdfFile
            resultadoPath = "treinamento" </> pdfFile <.> "json"
        (infoDoc, binfos) <- either (error "Oops") id <$> PP.parsePdfDetalhado [fullPath]
        whenM (liftIO $ doesFileExist resultadoPath) $ do
          resultadoEsperadoAtual <- fromMaybe (error "JSON inválido!") <$> Aeson.decodeFileStrict resultadoPath
          let matchingsAlgo = PP.mkMatching (infoDoc, binfos)
              acuracia = PP.matchingAccuracy matchingsAlgo resultadoEsperadoAtual
          liftIO $ putStrLn "Matching de algoritmos (atual, desejado):"
          liftIO $ print $ RIO.zip matchingsAlgo resultadoEsperadoAtual
          liftIO $ putStrLn ""
          liftIO $ putStrLn $ "Acurácia de matching do algoritmo atual: " <> show acuracia

        liftIO $ putStrLn "Para cada bloco exibido, digite:"
        liftIO $ putStrLn "\"m\" se este bloco pertence ao Mesmo parágrafo do anterior"
        liftIO $ putStrLn "\"i\" para IniciaOutroParagrafo"
        liftIO $ putStrLn "\"c\" para início de Cabeçalho"
        liftIO $ putStrLn "\"s\" para sair (a opção de salvar ou não aparecerá em seguida)"
        liftIO $ putStrLn ""
        matchingEsperado <- Process.withProcessWait (Process.shell $ "xdg-open \"" ++ fullPath ++ "\"") $ \_ ->
          forMUntilNothing binfos $ \binfo -> do
            let readMatch = liftIO getLine >>= \case
                                                        "m" -> return $ Just PP.DoMesmoParagrafo
                                                        "i" -> return $ Just PP.IniciaOutroParagrafo
                                                        "c" -> return $ Just PP.Cabecalho
                                                        "s" -> return Nothing
                                                        _   -> readMatch
            
            liftIO $ putStrLn $ T.unpack $ PP.infoTexto binfo
            
            readMatch
        
        liftIO $ putStrLn $ "Digite \"salvar\" para escrever este arquivo em disco em " <> resultadoPath
        cmd <- liftIO getLine
        when (cmd == "salvar") $ do
          RIO.writeFileBinary resultadoPath $ LBS.toStrict $ Aeson.encode matchingEsperado
          liftIO $ putStrLn $ "Arquivo com resultado esperado escrito em " <> resultadoPath
        return ()
      _ ->
        forever $ do
          forM_ allCrawlers $ downloadEIndexar hj ctx
          putStrLn "Diários baixados. Esperando 1 hora para baixar novamente."
          threadDelay (1000 * 1000 * 60 * 60) -- Espera 1 hora até tentar de novo

data AppContext = AppContext {
  mgr :: Manager,
  dbPool :: Pool PGS.Connection,
  basePath :: String
  -- awsConfig :: Aws.Configuration
}

-- | Baixa o diário e atualiza o banco de dados para torná-lo buscável
downloadEIndexar :: (MonadThrow m, MonadUnliftIO m, MonadIO m) => Day -> AppContext -> Crawler -> m ()
downloadEIndexar hj AppContext{..} sub = do
  liftIO $ Prelude.putStrLn $ "Baixando " ++ show sub ++ " da data " ++ show hj
  crawlRes <- findLinks sub hj mgr
  case crawlRes of
    CrawlDiarios links -> do
      liftIO $ withDbConnection dbPool $ \conn -> do
        diarioABaixar <- withDbTransaction conn $ do
          diarioExistenteMaybe <- getDiarioNaData (crawlerOrigemDiarioId sub) hj conn
          diarioExistente <- case diarioExistenteMaybe of
                                Just d  -> return d
                                Nothing -> beamInsertReturningOrThrow conn (diarios diariosDb) Diario {
                                            diarioId = default_,
                                            diarioOrigemDiarioId = val_ $ crawlerOrigemDiarioId sub,
                                            diarioNome = val_ "",
                                            diarioData = val_ hj
                                          }
          rightNow <- utcNow
          beamInsertReturningOrThrow conn (diariosABaixar diariosDb) DiarioABaixar {
            diarioabaixarId = default_,
            diarioabaixarDiarioId = val_ $ pk diarioExistente,
            diarioabaixarInicioDownload = val_ rightNow
          }

        downloads <- forM (Prelude.zip [1..] links) $ \(ordem, l) -> do
          downloadStart <- utcNow
          statusDownload <- withDbTransaction conn $
                              beamInsertReturningOrThrow conn (statusDownloads diariosDb)
                                              StatusDownloadDiario {
                                                  statusdownloaddiarioId = default_,
                                                  statusdownloaddiarioDiarioABaixarId = val_ $ primaryKey diarioABaixar,
                                                  statusdownloaddiarioUrl = val_ l,
                                                  statusdownloaddiarioOrdem = val_ ordem,
                                                  statusdownloaddiarioInicioDownload = val_ downloadStart
                                                }
          -- TODO: Erro de download -> atualizar banco com erro e disparar email
          (pdfFilePath, md5) <- downloadToAsMd5 l basePath mgr
          downloadEnd <- utcNow
          withDbTransaction conn $
            beamInsertOrThrow conn (downloadsTerminados diariosDb) DownloadTerminado {
              downloadterminadoId = default_,
              downloadterminadoStatusDownloadDiarioId = val_ $ primaryKey statusDownload,
              downloadterminadoMomentoTermino = val_ downloadEnd,
              downloadterminadoMd5Sum = val_ $ toS (show md5),
              downloadterminadoFilePath = val_ $ toS pdfFilePath
            }
          return pdfFilePath

        paragrafosDiario <- either (error "Erro de parsing!") id <$> PP.parsePdf downloads

        -- Aqui pegamos um md5sum de todo o conteúdo. Podemos pegar diretamente de "conteudoTextoPdf" pois aí mudanças
        -- no conversor pdf -> texto irão ativar o código de mudança de conteúdo que vem depois
        let conteudoTextoPdf = T.concat $ fmap PP.printParagrafo paragrafosDiario
            conteudoMd5Sum = hash ((toS conteudoTextoPdf) :: ByteString) :: Digest MD5
            md5sumString   = show conteudoMd5Sum
        -- writeFileUtf8 (basePath </> md5sumString <.> ".txt") conteudoTextoPdf
        conteudoDiario <- withDbTransaction conn $ do
          conteudoDiario <- beamInsertOrGet_ conn (conteudosDiarios diariosDb) ConteudoDiario {
            conteudodiarioId = default_,
            conteudodiarioMd5Sum = val_ $ toS md5sumString
          } (\t -> conteudodiarioMd5Sum t ==. val_ (toS md5sumString))
          beamInsertOrThrow conn (diariosABaixarToConteudosDiarios diariosDb) DiarioABaixarToConteudoDiario {
            diarioabaixartoconteudodiarioId = default_,
            diarioabaixartoconteudodiarioDiarioABaixarId = val_ $ pk diarioABaixar,
            diarioabaixartoconteudodiarioConteudoDiarioId = val_ $ pk conteudoDiario
          }
          return conteudoDiario

        -- TODO: Tudo daqui pra baixo deveria ser uma função separada para que possa ser executada
        -- para diários antigos quando introduzimos novidades (e.g. mais tokens buscáveis)
        
        withDbTransaction_ conn $ do
          -- TODO: insertOnConflictUpdate ao invés de beamInsertOnNoConflict e apagar apenas parágrafos com ordem maior que o último (código comentado mais abaixo)
          beamDelete conn (paragrafosDiarios diariosDb) (\pd -> paragrafodiarioConteudoDiarioId pd ==. val_ (pk conteudoDiario))
          forM_ (RIO.zip [0..] paragrafosDiario) $ \(i, paragrafo) -> do
            beamInsertOnNoConflict conn (paragrafosDiarios diariosDb) ParagrafoDiario {
              paragrafodiarioId = default_,
              paragrafodiarioConteudoDiarioId = val_ $ pk conteudoDiario,
              paragrafodiarioOrdem = val_ i,
              paragrafodiarioConteudo = val_ $ PP.printParagrafo paragrafo
            }
          -- let ordemUltimoParagrafo = RIO.length paragrafosDiario - 1
          -- runDelete $ delete (paragrafosDiarios diariosDb) (\pd -> paragrafodiarioOrdem pd >. ordemUltimoParagrafo &&. paragrafodiarioConteudoDiarioId ==. pk conteudoDiario)

          let (DocumentoDiario tokensEPosicoes) = parseConteudoDiario conteudoTextoPdf
        
          -- Inserção de tokens buscáveis de todos tipos que nos interessarem
          beamDelete conn (tokensTextoTbl diariosDb) (\tt -> tokentextoConteudoDiarioId tt ==. val_ (pk conteudoDiario))
          forM_ tokensEPosicoes $ \(pos, token) ->
            case token of
              TokenCpf (cpfTexto, cpf) ->
                beamInsertOnNoConflict conn (tokensTextoTbl diariosDb) Model.Diarios.TokenTexto {
                  tokentextoId = default_,
                  tokentextoValorTexto = val_ (printCpfSoNumeros cpf),
                  tokentextoConteudoDiarioId = val_ $ pk conteudoDiario,
                  tokentextoTipo = val_ "CPF",
                  tokentextoInicio = val_ pos,
                  tokentextoComprimento = val_ (T.length cpfTexto)
                }

              TokenCnpj (cnpjTexto, cnpj) ->
                beamInsertOnNoConflict conn (tokensTextoTbl diariosDb) Model.Diarios.TokenTexto {
                  tokentextoId = default_,
                  tokentextoValorTexto = val_ (printCnpjSoNumeros cnpj),
                  tokentextoConteudoDiarioId = val_ $ pk conteudoDiario,
                  tokentextoTipo = val_ "CNPJ",
                  tokentextoInicio = val_ pos,
                  tokentextoComprimento = val_ (T.length cnpjTexto)
                }
              
              _ -> return ()
    CrawlError e          -> liftIO $ Prelude.putStrLn ("Erro: " ++ show e) >> return ()
    CrawlArquivoNaoExiste -> liftIO $ Prelude.putStrLn ("Não há diário a baixar para a data " ++ show hj) >> return ()