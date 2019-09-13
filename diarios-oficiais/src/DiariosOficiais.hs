module DiariosOficiais (start) where

import Model.Diarios
import Brdocs
import BeamUtils
import DiariosOficiais.Crawling
import DiariosOficiais.Database
import RIO
import UnliftIO.Environment
import Data.Time
import Data.Pool
import qualified Database.PostgreSQL.Simple as PGS
import Control.Monad.Trans.Resource
import System.Directory
import Crypto.Hash
import Crypto.Hash.Conduit
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import System.FilePath
import Data.Conduit
import Data.Conduit.Binary
import Data.String.Conv
import Database.Beam
import Network.HTTP.Conduit
import qualified PdfParser as PP
import qualified PdfParser.Estruturas as PP
import Buscador
import AwsUtils
import qualified Aws
import qualified Data.Foldable as Fold
import qualified Crawlers.Campinas as CrawlerCampinas
import qualified Crawlers.Sumare as CrawlerSumare
import qualified Data.Text as T

allCrawlers :: [Crawler]
allCrawlers = [toCrawler CrawlerSumare.SumareCrawler, toCrawler CrawlerCampinas.CampinasCrawler]

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

start :: IO ()
start = do
  putStrLn "Passe a opção \"fetch\" para baixar todos os diários dos últimos 365 dias e não passe opção nenhuma para baixar continuamente diários"
  let mgrSettings = Http.tlsManagerSettings { Http.managerModifyRequest = \req -> return req { requestHeaders = [("User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/73.0.3683.86 Safari/537.36")] } }
  mgr <- newManager mgrSettings
  bracket (createDbPool 1 60 50) destroyAllResources $ \dbPool -> do
    let basePath = "./data/diarios-oficiais/"
    createDirectoryIfMissing False basePath
    awsConfig <- createAwsConfiguration
    let ctx = AppContext {
      mgr = mgr,
      dbPool = dbPool,
      basePath = basePath,
      awsConfig = awsConfig
    }
    hj <- hoje
    args <- getArgs
    case args of
      [ "fetch" ] ->
        forM_ [0..365] $ \i -> do
          let dt = addDays ((-1) * i) hj
          Fold.forM_ allCrawlers $ \sub -> downloadEIndexar dt ctx sub
      _ ->
        forever $ do
          forM_ allCrawlers $ downloadEIndexar hj ctx
          putStrLn "Diários baixados. Esperando 1 hora para baixar novamente."
          threadDelay (1000 * 1000 * 60 * 60) -- Wait for 1 hour (threadDelay takes microseconds)

data AppContext = AppContext {
  mgr :: Manager,
  dbPool :: Pool PGS.Connection,
  basePath :: String,
  awsConfig :: Aws.Configuration
}

-- | Baixa o diário e atualiza o banco de dados para torná-lo buscável
downloadEIndexar :: (MonadUnliftIO m, MonadIO m) => Day -> AppContext -> Crawler -> m ()
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

        secoesPdf <- PP.parsePdfEmSecoes downloads

        -- Aqui pegamos um md5sum de todo o conteúdo. Podemos pegar diretamente de "conteudoTextoPdf" pois aí mudanças
        -- no conversor pdf -> texto irão ativar o código de mudança de conteúdo que vem depois
        let conteudoTextoPdf = T.concat $ fmap PP.printSecao secoesPdf
            conteudoMd5Sum = hash ((toS conteudoTextoPdf) :: ByteString) :: Digest MD5
            md5sumString   = show conteudoMd5Sum
            paragrafosDiario = RIO.concatMap PP.secaoConteudo secoesPdf
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