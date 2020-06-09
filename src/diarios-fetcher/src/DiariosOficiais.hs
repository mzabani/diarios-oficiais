module DiariosOficiais (start) where

import Model.Diarios
import DbUtils
import DiariosOficiais.Crawling
import DiariosOficiais.Database
import RIO
import UnliftIO.Environment
import DbVcs (bringDbUpToDate)
import Data.Time
import Data.Pool
import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Query as PQ
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
import Control.Monad.Logger (runNoLoggingT)
import Database.Beam
import Network.HTTP.Conduit
import UnliftIO.Async (pooledMapConcurrentlyN)
import UnliftIO.Exception (catchAny)
import qualified PdfParser.HtmlParser as PP
import qualified PdfParser.Estruturas as PP
import qualified Data.Foldable as Fold
import qualified Crawlers.Campinas as CrawlerCampinas
import qualified Crawlers.DOU
import qualified Data.Text as T
import qualified Data.Aeson as Aeson

allCrawlers :: [Crawler]
allCrawlers = [toCrawler CrawlerCampinas.CampinasCrawler, toCrawler Crawlers.DOU.DOU1Crawler, toCrawler Crawlers.DOU.DOU2Crawler, toCrawler Crawlers.DOU.DOU3Crawler]

hoje :: MonadIO m => m Day
hoje = liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime

utcNow :: MonadIO m => m UTCTime
utcNow = liftIO $ zonedTimeToUTC <$> getZonedTime

-- | Downloads the URL supplied and saves into the supplied directory, returning the full path of the saved file, whose
-- name is the file's MD5 hash
downloadToAsMd5 :: (MonadThrow m, MonadUnliftIO m, MonadIO m) => Text -> FilePath -> Manager -> m (FilePath, Digest MD5)
downloadToAsMd5 url dir mgr = do
  req <- parseRequest $ toS url
  runResourceT $ do
    resSource <- responseBody <$> http req mgr
    RIO.withSystemTempFile "concpub.file" $ \fp fileHandle -> do
      (fileHash, tempPath) <- sealConduitT resSource $$+- (getZipSink $ (,) <$> ZipSink sinkHash <*> ZipSink (sinkHandleAndClose fileHandle >> return fp))
      let finalPath = dir </> show (fileHash :: Digest MD5)
      -- Importante: renameFile pode falhar caso destino esteja em outro disco. Por isso usamos copyFile!
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
  putStrLn "Passe a opção \"fetch yyyy-mm-dd yyyy-mm-dd\" para baixar todos os diários ainda não baixados entre as duas datas fornecidas (inclusive) e não passe opção nenhuma para baixar continuamente diários"
  let mgrSettings = Http.tlsManagerSettings { Http.managerModifyRequest = \req -> return req { requestHeaders = [("User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.138 Safari/537.36")] } }
  mgr <- newManager mgrSettings
  basePath <- fromMaybe "./data/diarios-oficiais/" <$> lookupEnv "DIARIOSDIR"
  dbVcsInfo <- getDbVcsInfo
  -- Apply DB migrations if necessary
  void $ bringDbUpToDate dbVcsInfo
  bracket (createDbPool 1 60 50) (destroyAllResources) $ \dbPool -> do
    createDirectoryIfMissing False basePath
    -- awsConfig <- createAwsConfiguration
    let ctx = AppContext {
      mgr = mgr,
      dbPool = dbPool,
      basePath = basePath
      -- awsConfig = awsConfig
    }
    args <- getArgs
    case args of
      [ "fetch", sd1, sd2 ] -> do
        let
          md1 :: Maybe Day = parseTimeM True defaultTimeLocale "%F" sd1
          md2 = parseTimeM True defaultTimeLocale "%F" sd2
        case (md1, md2) of
          (Just d1, Just d2) -> do
            let
              de = min d1 d2
              ate = max d1 d2
            datasEDiariosFaltando :: [(Day, OrigemDiarioId)] <- withDbConnection dbPool $ \conn ->
              runNoLoggingT $ PQ.runPgMonadT conn $
                PQ.pgQuery [PQ.sqlExp|
                  with todasDatas (data) as (select (#{de}::date + (n || ' days')::interval)::date from generate_series(0, #{diffDays ate de}) n)
                      , datasEOrigens (data, origemDiarioId) as (select data, id from todasDatas cross join origemdiario)
                      , datasEOrigensBaixados (data, origemDiarioId) as (
                              select distinct diario.data, diario.origemdiarioid
                              from diario
                              join diarioabaixar on diarioabaixar.diarioid=diario.id
                              join statusdownloaddiario on statusdownloaddiario.diarioabaixarid=diarioabaixar.id
                              join downloadterminado on downloadterminado.statusdownloaddiarioid=statusdownloaddiario.id
                              join diarioabaixartoconteudodiario dbctcb on dbctcb.diarioabaixarid = diarioabaixar.id
                              where exists (select 1 from paragrafodiario where paragrafodiario.conteudodiarioid = dbctcb.conteudodiarioid))
                      select datasEOrigens.data, datasEOrigens.origemdiarioid
                        from datasEOrigens
                        left join datasEOrigensBaixados using (data, origemDiarioId)
                        where datasEOrigensBaixados.data is null|]
            let datasECrawlersFaltando :: [(Day, Crawler)] = mapMaybe (\(dt, origemId) -> (dt,) <$> Fold.find ((== origemId) . crawlerOrigemDiarioId) allCrawlers) datasEDiariosFaltando
            forM_ datasECrawlersFaltando $ \(dt, cr) -> downloadEIndexar dt ctx cr
          _ -> liftIO $ putStrLn "Ao usar o fetch, certifique-se de fornecer duas datas em formato yyyy-mm-dd"

      [ "reindexar" ] ->
        error "Precisamos ser capazes de ler os PDFs originais e reinserir os parágrafos deles sem baixá-los dos sites!"
        
      [ "treinar", pdfFile ] -> do
        let fullPath = "data/diarios-oficiais" </> pdfFile
            resultadoPath = "treinamento" </> pdfFile <.> "json"
        (infoDoc, binfos) <- either id (error "Não foi possível ler o arquivo pdf") <$> PP.parseLinkBaixadoDetalhado ProcessarPdf [fullPath]
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
          hj <- hoje
          forM_ allCrawlers $ \sub -> do
            catchAny (downloadEIndexar hj ctx sub) $ const $ do
              liftIO $ putStrLn $ "Falha ao baixar diário " ++ toS (crawlerNome sub) ++ ", data " ++ show hj
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
    CrawlDiarios links procConteudo -> do
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

        downloadStart <- utcNow
        statusDownloads <- withDbTransaction conn $ forM (Prelude.zip [1..] links) $ \(ordem, l) -> beamInsertReturningOrThrow conn (statusDownloads diariosDb)
                                              StatusDownloadDiario {
                                                  statusdownloaddiarioId = default_,
                                                  statusdownloaddiarioDiarioABaixarId = val_ $ primaryKey diarioABaixar,
                                                  statusdownloaddiarioUrl = val_ l,
                                                  statusdownloaddiarioOrdem = val_ ordem,
                                                  statusdownloaddiarioInicioDownload = val_ downloadStart
                                                }
        
        downloads <- pooledMapConcurrentlyN 20 (\sd -> do
          (contentsFilePath, md5) <- downloadToAsMd5 (statusdownloaddiarioUrl sd) basePath mgr
          downloadEnd <- utcNow
          -- Estamos em threads concorrentes => não use a mesma DB connection!
          withDbConnection dbPool $ \newConn -> withDbTransaction newConn $
            beamInsertOrThrow conn (downloadsTerminados diariosDb) DownloadTerminado {
              downloadterminadoId = default_,
              downloadterminadoStatusDownloadDiarioId = val_ $ primaryKey sd,
              downloadterminadoMomentoTermino = val_ downloadEnd,
              downloadterminadoMd5Sum = val_ $ toS (show md5),
              downloadterminadoFilePath = val_ $ toS contentsFilePath
            }
          return contentsFilePath
          ) statusDownloads

        liftIO $ Prelude.putStrLn $ show sub ++ " da data " ++ show hj ++ " baixado com " ++ show (length downloads) ++ " URLs baixadas. Persistindo hash do conteúdo do diário"
        paragrafosDiario :: [Text] <- either (fmap PP.printParagrafo) id <$> PP.parseLinkBaixado procConteudo downloads

        -- Aqui pegamos um md5sum de todo o conteúdo.
        let conteudoMd5Sum = hashFinalize $ hashUpdates (hashInit @MD5) $ fmap encodeUtf8 paragrafosDiario
            md5sumString   = show conteudoMd5Sum
        (conteudoDiario, alreadyExisted) <- withDbTransaction conn $ do
          (conteudoDiario, alreadyExisted) <- beamInsertOrGet conn (conteudosDiarios diariosDb) ConteudoDiario {
            conteudodiarioId = default_,
            conteudodiarioMd5Sum = val_ $ toS md5sumString
          } (\t -> conteudodiarioMd5Sum t ==. val_ (toS md5sumString))
          beamInsertOrThrow conn (diariosABaixarToConteudosDiarios diariosDb) DiarioABaixarToConteudoDiario {
            diarioabaixartoconteudodiarioId = default_,
            diarioabaixartoconteudodiarioDiarioABaixarId = val_ $ pk diarioABaixar,
            diarioabaixartoconteudodiarioConteudoDiarioId = val_ $ pk conteudoDiario
          }
          return (conteudoDiario, alreadyExisted)

        jaPossuiParagrafos <- diarioPossuiParagrafos (pk conteudoDiario) conn

        case (alreadyExisted, jaPossuiParagrafos) of
          (RowExisted, True) -> liftIO $ putStrLn $ show sub ++ " da data " ++ show hj ++ ": já existia e nada mudou."
          _ -> do
            -- TODO: Tudo daqui pra baixo deveria ser uma função separada para que possa ser executada
            -- para diários antigos quando introduzimos novidades (e.g. mais tokens buscáveis)
            
            liftIO $ Prelude.putStrLn $ show sub ++ " da data " ++ show hj ++ ": persistindo " ++ show (length paragrafosDiario) ++ " parágrafos"
            withDbTransaction_ conn $ do
              -- TODO: insertOnConflictUpdate ao invés de beamInsertOnNoConflict e apagar apenas parágrafos com ordem maior que o último (código comentado mais abaixo)
              beamDelete conn (paragrafosDiarios diariosDb) (\pd -> paragrafodiarioConteudoDiarioId pd ==. val_ (pk conteudoDiario))
              forM_ (RIO.zip [0..] paragrafosDiario) $ \(i, htmlParagrafo) -> do
                beamInsertOnNoConflict conn (paragrafosDiarios diariosDb) ParagrafoDiario {
                  paragrafodiarioId = default_,
                  paragrafodiarioConteudoDiarioId = val_ $ pk conteudoDiario,
                  paragrafodiarioOrdem = val_ i,
                  paragrafodiarioConteudo = val_ htmlParagrafo
                }

            liftIO $ Prelude.putStrLn $ show sub ++ " da data " ++ show hj ++ " pronto."
    
    CrawlError e          -> liftIO $ Prelude.putStrLn ("Erro: " ++ show e) >> return ()
    CrawlArquivoNaoExiste -> liftIO $ Prelude.putStrLn ("Não há diário a baixar para a data " ++ show hj) >> return ()