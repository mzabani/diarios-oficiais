{-# LANGUAGE AllowAmbiguousTypes, RecordWildCards #-}
module ConcursosPublicos (start) where

import Model.Diarios
import Model.Aprovados
import Brdocs
import BeamUtils
import ConcursosPublicos.Crawling
import ConcursosPublicos.Database
import RIO
import UnliftIO.Environment
import Data.Time
import Data.Pool
import qualified Data.List.NonEmpty as NonEmpty
import qualified Database.PostgreSQL.Simple as PGS
import Control.Monad.Trans.Control
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
import Data.Text
import Database.Beam
import Network.HTTP.Conduit
import PdfParser
import Buscador
import AwsUtils
import Zlude
import qualified Aws
import qualified Data.Foldable as Fold
import qualified Crawlers.Campinas as CrawlerCampinas
import qualified Crawlers.Sumare as CrawlerSumare
import qualified Data.Text as T

allCrawlers :: [Crawler]
allCrawlers = [toCrawler CrawlerSumare.SumareCrawler, toCrawler CrawlerCampinas.CampinasCrawler]
-- allCrawlers = [toCrawler CrawlerCampinas.CampinasCrawler]

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
  --let mgrSettings = Http.defaultManagerSettings { Http.managerModifyRequest = \req -> return req { requestHeaders = [("User-Agent", "Chrome 50")] } }
  let mgrSettings = Http.tlsManagerSettings { Http.managerModifyRequest = \req -> return req { requestHeaders = [("User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/73.0.3683.86 Safari/537.36")] } }
  mgr <- newManager mgrSettings
  dbPool <- createDbPool
  homeDir <- getHomeDirectory
  let basePath = homeDir </> "Diarios/"
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
      forM_ [0..0365] $ \i -> do
      --forM_ [0..365] $ \i -> do
        let dt = addDays ((-1) * i) hj
        Fold.forM_ allCrawlers $ \sub -> downloadEBuscarNomesNaData dt ctx sub
    _ ->
      forever $ do
        crawlersElistasUsrsParaAvisar <- forM allCrawlers $ \sub -> do
          usrsQueApareceram <- downloadEBuscarNomesNaData hj ctx sub
          return $ fmap ((,) sub) usrsQueApareceram
        -- let usrsParaAvisarESeusCrawlers = safeGroupBy (_usuarioId . snd) $ Prelude.concat crawlersElistasUsrsParaAvisar
        -- forM_ usrsParaAvisarESeusCrawlers $ \(_, crawlersEUsrs) -> do
        --   let usr = snd $ Zlude.head crawlersEUsrs
        --       crawlers = fmap fst crawlersEUsrs
        --   let conteudoEmail = "Olá, " <> _usuarioPrimeiroNome usr <> ",<br/><br/>" <> "Parece que seu nome apareceu em um ou mais diários oficiais do dia de hoje (" <> toS (show hj) <> ")! Veja abaixo a lista completa:<br/><br/>"
        --                       <> "<ul>" <> Fold.foldr (\el acc -> "<li>" <> el <> "</li>" <> acc) "" (fmap crawlerNome crawlers)
        --   -- TODO: Trocar destinatário por (_usuarioEmail usr) e usar algum HTML templating engine pra gerar o HTML dos emails..
          -- sendMail awsConfig (Email { from = "mzabani@gmail.com", to = "mzabani@gmail.com", subject = "Seu nome apareceu no diário oficial!", body = conteudoEmail })
        putStrLn "Diários baixados. Esperando 1 hora para baixar novamente."
        threadDelay (1000 * 1000 * 60 * 60) -- Wait for 1 hour (threadDelay takes microseconds)
  


data AppContext = AppContext {
  mgr :: Manager,
  dbPool :: Pool PGS.Connection,
  basePath :: String,
  awsConfig :: Aws.Configuration
}

-- | Busca os nomes de todos usuários ativos na data fornecida para o Crawler fornecido atualizando o banco de dados apropriadamente
--   e retornando os usuários encontrados
downloadEBuscarNomesNaData :: (MonadUnliftIO m, MonadIO m) => Day -> AppContext -> Crawler -> m [Usuario]
downloadEBuscarNomesNaData hj AppContext{..} sub = do
  liftIO $ Prelude.putStrLn $ "Baixando " ++ show sub ++ " da data " ++ show hj
  crawlRes <- findLinks sub hj mgr
  case crawlRes of
    CrawlDiarios links -> do
      liftIO $ withDbConnection dbPool $ \conn -> do
        diarioABaixar <- withDbTransaction conn $ do
          -- diarioExistente <- insertOrGet_ conn (diarios diariosDb) Diario {
          --                                   diarioId = default_,
          --                                   diarioOrigemDiarioId = val_ $ crawlerOrigemDiarioId sub,
          --                                   diarioNome = val_ "",
          --                                   diarioData = val_ hj
          --                                 } (\d -> crawlerOrigemDiarioId d ==. val_ (crawlerOrigemDiarioId sub) &&. diarioData d ==. val_ hj)
          diarioExistenteMaybe <- getDiarioNaData (crawlerOrigemDiarioId sub) hj conn
          diarioExistente <- case diarioExistenteMaybe of
                                Just d  -> return d
                                Nothing -> insertReturningOrThrow conn (diarios diariosDb) Diario {
                                            diarioId = default_,
                                            diarioOrigemDiarioId = val_ $ crawlerOrigemDiarioId sub,
                                            diarioNome = val_ "",
                                            diarioData = val_ hj
                                          }
          rightNow <- utcNow
          insertReturningOrThrow conn (diariosABaixar diariosDb) DiarioABaixar {
            diarioabaixarId = default_,
            diarioabaixarDiarioId = val_ $ pk diarioExistente,
            diarioabaixarInicioDownload = val_ rightNow
          }

        downloads <- forM (Prelude.zip [1..] links) $ \(ordem, l) -> do
          downloadStart <- utcNow
          statusDownload <- withDbTransaction conn $
                              insertReturningOrThrow conn (statusDownloads diariosDb)
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
            insertOrThrow conn (downloadsTerminados diariosDb) DownloadTerminado {
              downloadterminadoId = default_,
              downloadterminadoStatusDownloadDiarioId = val_ $ primaryKey statusDownload,
              downloadterminadoMomentoTermino = val_ downloadEnd,
              downloadterminadoMd5Sum = val_ $ toS (show md5),
              downloadterminadoFilePath = val_ $ toS pdfFilePath
            }
          -- TODO: Renomear ConteudoDiario para DiarioABaixar e remover dele propriedade conteúdo.
          -- Daí criar entidade ConteudoDiario que referencie DiarioABaixar e que tenha o conteúdo
          liftIO $ print (ordem, l)
          return pdfFilePath

        secoesPdfEmTexto <- parsePdfEmSecoes downloads

        -- Aqui pegamos um md5sum de todo o conteúdo. Podemos pegar diretamente de "conteudoTextoPdf" pois aí mudanças
        -- no conversor pdf -> texto irão ativar o código de mudança de conteúdo que vem depois
        let conteudoTextoPdf = T.concat secoesPdfEmTexto
            conteudoMd5Sum = hash ((toS conteudoTextoPdf) :: ByteString) :: Digest MD5
            md5sumString   = show conteudoMd5Sum
        writeFileUtf8 (basePath </> md5sumString <.> ".txt") conteudoTextoPdf
        conteudoDiario <- withDbTransaction conn $ do
          conteudoDiario <- insertOrGet_ conn (conteudosDiarios diariosDb) ConteudoDiario {
            conteudodiarioId = default_,
            conteudodiarioConteudo = val_ conteudoTextoPdf,
            conteudodiarioMd5Sum = val_ $ toS md5sumString
          } (\t -> conteudodiarioMd5Sum t ==. val_ (toS md5sumString))
          insertOrThrow conn (diariosABaixarToConteudosDiarios diariosDb) DiarioABaixarToConteudoDiario {
            diarioabaixartoconteudodiarioId = default_,
            diarioabaixartoconteudodiarioDiarioABaixarId = val_ $ pk diarioABaixar,
            diarioabaixartoconteudodiarioConteudoDiarioId = val_ $ pk conteudoDiario
          }
          return conteudoDiario

        -- TODO: Se as Seções forem muito grandes.. o trecho abaixo jogará uma exceção. Precisamos
        -- capturar e precisamos de um Fallback (quem sabe páginas?)
        withDbTransaction conn $
          forM_ (RIO.zip [0..] secoesPdfEmTexto) $ \(i, textoSecao) -> do
            -- TODO: insertOnConflictUpdate
            liftIO $ putStrLn $ "INSERINDO SECAO " <> show (i, conteudodiarioId conteudoDiario)
            -- liftIO $ putStrLn (T.unpack textoSecao)
            insertOnNoConflict conn (secoesDiarios diariosDb) SecaoDiario {
              secaodiarioId = default_,
              secaodiarioConteudoDiarioId = val_ $ pk conteudoDiario,
              secaodiarioOrdem = val_ i,
              secaodiarioConteudo = val_ textoSecao
            }

        -- TODO: Tudo daqui pra baixo deveria ser uma função separada para que possa ser executada
        -- para diários antigos quando introduzimos novidades (e.g. mais tokens buscáveis)
        let docDiario@(DocumentoDiario tokensEPosicoes) = parseConteudoDiario conteudoTextoPdf
        
        -- 1. Inserção de tokens buscáveis de todos tipos que nos interessarem
        withDbTransaction conn $
          forM_ tokensEPosicoes $ \(pos, token) ->
            case token of
              TokenCpf (cpfTexto, cpf) ->
                insertOnNoConflict conn (tokensTextoTbl diariosDb) Model.Diarios.TokenTexto {
                  tokentextoId = default_,
                  tokentextoValorTexto = val_ (printCpfSoNumeros cpf),
                  tokentextoConteudoDiarioId = val_ $ pk conteudoDiario,
                  tokentextoTipo = val_ "CPF",
                  tokentextoInicio = val_ pos,
                  tokentextoComprimento = val_ (T.length cpfTexto)
                }

              TokenCnpj (cnpjTexto, cnpj) ->
                insertOnNoConflict conn (tokensTextoTbl diariosDb) Model.Diarios.TokenTexto {
                  tokentextoId = default_,
                  tokentextoValorTexto = val_ (printCnpjSoNumeros cnpj),
                  tokentextoConteudoDiarioId = val_ $ pk conteudoDiario,
                  tokentextoTipo = val_ "CNPJ",
                  tokentextoInicio = val_ pos,
                  tokentextoComprimento = val_ (T.length cnpjTexto)
                }
              
              _ -> return ()


        -- Agora fazemos a busca de todos os usuários ativos na data em questão que ainda não tiveram seus nomes buscados.
        -- Isso evita que deixemos cadastros pra trás.. (na verdade ao cadastrar-se faremos uma busca geral, mas isso aqui serve como redundância)
        usuariosNaoBuscados <- getUsuariosAtivosNaoBuscados conn hj (pk conteudoDiario)
        case usuariosNaoBuscados of
          [] -> return []
          _ -> do
            let usrsAgrupadosPorPrimeiroNome = agruparUsuariosPorPrimeiroNome usuariosNaoBuscados
            listasNomesEQtds <- forM usrsAgrupadosPorPrimeiroNome $ \(primeiroNome, usrs) -> do
              -- 1. Primeiro buscamos apenas por primeiros nomes, o que nos permite indexar operações de busca futuras
              let indicesNomesEncontrados = fmap fst $ buscarNomeApenasMatches primeiroNome docDiario
                  loweredNome = toLower primeiroNome
              liftIO $ Prelude.putStrLn $ "Buscando " ++ show (Prelude.length usrs) ++ " usuários com primeiro nome " ++ show primeiroNome
              withDbTransaction conn $ forM_ indicesNomesEncontrados $ \idx -> do
                insertOnNoConflict conn (nomesEncontrados diariosDb) NomeEncontrado {
                  nomeencontradoId = default_,
                  nomeencontradoLoweredNome = val_ loweredNome,
                  nomeencontradoConteudoDiarioId = val_ $ pk conteudoDiario,
                  nomeencontradoPosicao = val_ idx
                }
                liftIO $ Prelude.putStrLn $ "Nome " ++ show primeiroNome ++ " encontrado na posição " ++ show idx

              -- 2. Agora buscamos realmente por nome inteiro e inserimos matches (talvez isso possa ser muito otimizado a partir da busca anterior, mas isso não importa pra nós aqui ainda..)
              listasUsrsEncontrados <- withDbTransaction conn $ forM usrs (procurarUsuarioEAtualizarBanco conn (conteudoDiario, docDiario))
              return (primeiroNome, Prelude.length indicesNomesEncontrados, listasUsrsEncontrados)

            -- let conteudoEmailParaMim = T.concat $ T.concat ["<h2>Diário ", toS (show sub), " de ", toS (show hj), "</h2>"] : fmap (\(primeiroNome, qtd, _) -> T.concat ["Nome \"", primeiroNome, "\": ", toS (show qtd), "<br/>" ]) listasNomesEQtds
            -- _ <- sendMail awsConfig (Email { from = "mzabani@gmail.com", to = "mzabani@gmail.com", subject = "Relatório diário", body = conteudoEmailParaMim })

            -- Cruzamos usuariosNaoBuscados com listasUsrsEncontrados e retornar [Usuario] com todos Usuarios encontrados nesta busca!
            let todosUsrsEncontrados = Fold.concat $ Fold.concatMap (NonEmpty.toList . thd) listasNomesEQtds
                usrsEncontradosAgora = [u | u <- usuariosNaoBuscados, b <- todosUsrsEncontrados, _usuarioencontradoUsuarioId b == pk u]
            return $ distinctBy _usuarioId usrsEncontradosAgora
    CrawlError e          -> liftIO $ Prelude.putStrLn ("Erro: " ++ show e) >> return []
    CrawlArquivoNaoExiste -> liftIO $ Prelude.putStrLn ("Não há diário a baixar para a data " ++ show hj) >> return []

-- | Busca o usuário fornecido no ConteudoDiario fornecido junto de seu DocumentoDiario (já parseado) e insere no banco de dados os matches encontrados, além de retorná-los.
--   Insere também um registro indicando que o usuário foi buscado
procurarUsuarioEAtualizarBanco :: (MonadIO m, MonadBaseControl IO m, MonadIO m) => PGS.Connection -> (ConteudoDiario, DocumentoDiario) -> Usuario -> m [UsuarioEncontrado]
procurarUsuarioEAtualizarBanco conn (conteudoDiario, docDiario) usr = do
  let idxENomes = buscarNomeApenasMatches (_usuarioNomeCompleto usr) docDiario
  usrsEncontrados <- forM idxENomes $ \(idx, nomeEncontrado) ->
    insertOrGet_ conn (_usuariosencontrados aprovadosDb) UsuarioEncontrado {
      _usuarioencontradoId = default_,
      _usuarioencontradoConteudoDiarioId = val_ $ pk conteudoDiario,
      _usuarioencontradoUsuarioId = val_ $ pk usr,
      _usuarioencontradoIdx = val_ idx,
      _usuarioencontradoNomeEncontrado = val_ nomeEncontrado
    } (\ue -> _usuarioencontradoUsuarioId ue ==. val_ (pk usr) &&. _usuarioencontradoConteudoDiarioId ue ==. val_ (pk conteudoDiario) &&. _usuarioencontradoIdx ue ==. val_ idx)
  now <- utcNow
  insertOnNoConflict conn (_usuariosbuscados aprovadosDb) UsuarioBuscado {
    _usuariobuscadoId = default_
    , _usuariobuscadoConteudoDiarioId = val_ $ pk conteudoDiario
    , _usuariobuscadoUsuarioId = val_ $ pk usr
    , _usuariobuscadoBuscaTerminadaEm = val_ now
  }
  return usrsEncontrados
    -- TODO: Criar disparador de email que insira no banco de dados os emails disparados (para eventual conferência)