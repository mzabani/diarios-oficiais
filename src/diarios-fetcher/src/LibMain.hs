module LibMain (start) where

import RIO
import Data.Pool (destroyAllResources)
import Data.Time
import DiariosOficiais.Database (createDbPool, getDbVcsInfo)
import DbVcs (bringDbUpToDate)
import qualified Fetcher
import Options.Applicative
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified Treinar
import UnliftIO.Directory (createDirectoryIfMissing)
import UnliftIO.Environment

data FullArgs = Reindexar | Treinar String | FetchContinuamente | Fetch Fetcher.FetchArgs

argsParser :: Parser FullArgs
argsParser =
  flag' Reindexar (long "reindexar" <> help "Reindexar diários baixados - ainda não implementado")
  <|>
  Treinar <$> strOption (long "treinar" <> help "Permite treinar o algoritmo de classificação de parágrafos de forma assistida para um arquivo PDF específico" <> metavar "PDFFILE")
  <|>
  (flag' Fetch (long "fetch" <> help "Baixa diários oficiais da Internet para um período especificado e os indexa para busca")
    <*> fetchArgsParser)
  <|> flag' FetchContinuamente (long "fetch-continuamente" <> help "Verifica de tempos em tempos e baixa os diários do dia de forma contínua, para sempre")
  
  where
    fetchArgsParser =
      Fetcher.FetchArgs
        <$> argument dateReader (metavar "DATA-INICIAL" <> help "A data dos primeiros diários a serem baixados em formato yyyy-mm-dd")
        <*> argument dateReader (metavar "DATA-FINAL" <> help "A data dos últimos diários a serem baixados em formato yyyy-mm-dd")
        <*> switch (long "manter-arquivos" <> short 'm' <> help "Por padrão os arquivos baixados são apagados após indexados. Use esta flag para mantê-los em disco.")
    
    dateReader = maybeReader (parseTimeM True defaultTimeLocale "%F")

start :: IO ()
start = doWork =<< execParser opts
  where
    opts = info (argsParser <**> helper)
      ( fullDesc
      -- <> progDesc "Descrição"
      -- <> header "Cabeçalho"
      )

doWork :: FullArgs -> IO ()
doWork args = do
  let mgrSettings = Http.tlsManagerSettings { Http.managerModifyRequest = \req -> return req { Http.requestHeaders = [("User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.138 Safari/537.36")] } }
  mgr <- Http.newManager mgrSettings
  basePath <- fromMaybe "./data/diarios-oficiais/" <$> lookupEnv "DIARIOSDIR"
  dbVcsInfo <- getDbVcsInfo
  -- Apply DB migrations if necessary
  void $ bringDbUpToDate dbVcsInfo
  bracket (createDbPool 1 60 50) (destroyAllResources) $ \dbPool -> do
    createDirectoryIfMissing False basePath
    -- awsConfig <- createAwsConfiguration
    let ctx = Fetcher.AppContext {
      mgr = mgr,
      dbPool = dbPool,
      basePath = basePath
      -- awsConfig = awsConfig
    }
    case args of
      Fetch fargs -> Fetcher.fetch ctx fargs

      Reindexar ->
        error "Precisamos ser capazes de ler os PDFs originais e reinserir os parágrafos deles sem baixá-los dos sites!"
        
      Treinar pdfFile -> Treinar.treinar pdfFile
        
      FetchContinuamente -> Fetcher.fetchContinuamente ctx