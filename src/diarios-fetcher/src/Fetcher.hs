module Fetcher (AppContext(..), FetchArgs(..), fetch, fetchContinuamente) where

import RIO

import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Resource
import qualified Crawlers.Campinas as CrawlerCampinas
import qualified Crawlers.DOU
import Crypto.Hash
import Crypto.Hash.Conduit
import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Foldable as Fold
import Data.Pool
import Data.String.Conv (toS)
import Data.Time
import Database.Beam
import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Query as PQ
import DbUtils
import DiariosOficiais.Crawling
import Model.Diarios
import Network.HTTP.Conduit
import qualified PdfParser.HtmlParser as PP
import qualified PdfParser.Estruturas as PP
import RIO.FilePath ((</>))
import UnliftIO.Directory (copyFile, removeFile)

data AppContext = AppContext {
  mgr :: Manager,
  dbPool :: Pool PGS.Connection,
  basePath :: String
  -- awsConfig :: Aws.Configuration
}

data FetchArgs = FetchArgs {
  fetchDesde :: Day
  , fetchAte :: Day
  , manterArquivos :: Bool
}

allCrawlers :: [Crawler]
allCrawlers = [toCrawler CrawlerCampinas.CampinasCrawler, toCrawler Crawlers.DOU.DOU1Crawler, toCrawler Crawlers.DOU.DOU2Crawler, toCrawler Crawlers.DOU.DOU3Crawler]

utcNow :: MonadIO m => m UTCTime
utcNow = liftIO $ zonedTimeToUTC <$> getZonedTime

hoje :: MonadIO m => m Day
hoje = liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime

fetchContinuamente :: AppContext -> IO ()
fetchContinuamente ctx = forever $ do
    hj <- hoje
    forM_ allCrawlers $ \sub -> do
        catchAny (downloadEIndexar hj False ctx sub) $ const $ do
            liftIO $ putStrLn $ "Falha ao baixar diário " ++ toS (crawlerNome sub) ++ ", data " ++ show hj
        putStrLn "Diários baixados. Esperando 1 hora para baixar novamente."
        threadDelay (1000 * 1000 * 60 * 60) -- Espera 1 hora até tentar de novo

fetch :: AppContext -> FetchArgs -> IO ()
fetch ctx@(AppContext { dbPool }) (FetchArgs de ate manterArquivos) = do
    datasEDiariosFaltando :: [(Day, OrigemDiarioId)] <- withDbConnection dbPool $ \conn ->
        runNoLoggingT $ PQ.runPgMonadT conn $
        PQ.pgQuery [PQ.sqlExp|
            with todasDatas (data) as (select (#{de}::date + (n || ' days')::interval)::date from generate_series(0, #{diffDays ate de}) n)
                , datasEOrigens (data, origemDiarioId) as (select data, id from todasDatas cross join origemdiario)
                , datasEOrigensBaixados (data, origemDiarioId) as (
                        select distinct diario.data, diario.origemdiarioid
                        from diario
                        join diarioabaixar on diarioabaixar.diarioid=diario.id
                        join diarioabaixartoconteudodiario dbctcb on dbctcb.diarioabaixarid = diarioabaixar.id
                        join conteudodiario on conteudodiario.id=dbctcb.conteudodiarioid
                        where not conteudodiario.diario_existe
                            or exists (select 1 from paragrafodiario where paragrafodiario.conteudodiarioid = dbctcb.conteudodiarioid))
                select datasEOrigens.data, datasEOrigens.origemdiarioid
                from datasEOrigens
                left join datasEOrigensBaixados using (data, origemDiarioId)
                where datasEOrigensBaixados.data is null
                order by datasEOrigens.data, datasEOrigens.origemdiarioid|]
    let datasECrawlersFaltando :: [(Day, Crawler)] = mapMaybe (\(dt, origemId) -> (dt,) <$> Fold.find ((== origemId) . crawlerOrigemDiarioId) allCrawlers) datasEDiariosFaltando
    forM_ datasECrawlersFaltando $ \(dt, cr) -> 
      downloadEIndexar dt manterArquivos ctx cr `catchAny` \e -> do
        print e
        putStrLn "Ignorando exceção e continuando em 10 segundos..."
        threadDelay (1000 * 1000 * 10)

-- | Baixa o diário e atualiza o banco de dados para torná-lo buscável
downloadEIndexar :: (MonadThrow m, MonadUnliftIO m, MonadIO m) => Day -> Bool -> AppContext -> Crawler -> m ()
downloadEIndexar hj manterArquivos AppContext{..} sub = do
  liftIO $ Prelude.putStrLn $ "Baixando " ++ show sub ++ " da data " ++ show hj
  crawlRes <- findLinks sub hj mgr
  case crawlRes of
    CrawlDiarios links procConteudo -> registrarDiario links procConteudo
    CrawlArquivoNaoExiste -> do
      liftIO $ Prelude.putStrLn "Diário inexistente"
      registrarDiario [] ProcessarPdf -- Tanto faz se é PDF ou HTML aqui
    CrawlError e          -> liftIO $ Prelude.putStrLn ("Erro: " ++ show e)

  where
    registrarDiario links procConteudo =
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
              downloadterminadoFilePath = if manterArquivos then val_ (Just $ toS contentsFilePath)
                                          else val_ Nothing
            }
          return contentsFilePath
          ) statusDownloads

        liftIO $ Prelude.putStrLn $ show (length downloads) ++ " URLs baixadas. Transformando conteúdo dos arquivos em parágrafos..."
        !(paragrafosDiario :: [Text]) <- either (fmap PP.printParagrafo) id <$> PP.parseLinkBaixado procConteudo downloads
        liftIO $ Prelude.putStrLn $ "Parágrafos obtidos. Persistindo hash do conteúdo do diário..."
        

        -- Aqui pegamos um md5sum de todo o conteúdo.
        let conteudoMd5Sum = hashFinalize $ hashUpdates (hashInit @MD5) $ fmap encodeUtf8 paragrafosDiario
            md5sumString   = show conteudoMd5Sum
        (conteudoDiario, alreadyExisted) <- withDbTransaction conn $ do
          (conteudoDiario, alreadyExisted) <- beamInsertOrGet conn (conteudosDiarios diariosDb) ConteudoDiario {
            conteudodiarioId = default_,
            conteudodiarioMd5Sum = val_ $ toS md5sumString,
            conteudodiarioDiarioExiste = val_ $ paragrafosDiario /= []
          } (\t -> conteudodiarioMd5Sum t ==. val_ (toS md5sumString))
          beamInsertOrThrow conn (diariosABaixarToConteudosDiarios diariosDb) DiarioABaixarToConteudoDiario {
            diarioabaixartoconteudodiarioId = default_,
            diarioabaixartoconteudodiarioDiarioABaixarId = val_ $ pk diarioABaixar,
            diarioabaixartoconteudodiarioConteudoDiarioId = val_ $ pk conteudoDiario
          }
          return (conteudoDiario, alreadyExisted)

        jaPossuiParagrafos <- diarioPossuiParagrafos (pk conteudoDiario) conn

        case (alreadyExisted, jaPossuiParagrafos) of
          (RowExisted, True) -> liftIO $ putStrLn $ "Diário já existia e nada mudou."
          _ -> do
            -- TODO: Tudo daqui pra baixo deveria ser uma função separada para que possa ser executada
            -- para diários antigos quando introduzimos novidades (e.g. mais tokens buscáveis)
            
            liftIO $ Prelude.putStrLn $ "Persistindo " ++ show (length paragrafosDiario) ++ " parágrafos..."
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

            unless manterArquivos $ forM_ downloads removeFile
            liftIO $ Prelude.putStrLn "Tudo pronto."

        

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