module Crawlers.Campinas where

import Model.Diarios
import ConcursosPublicos.Crawling
import Database.Beam
import Data.Time
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client
import Text.HTML.DOM
import Data.String.Conv
import Text.XML.Cursor
import System.FilePath
import ConcursosPublicos.Xml

getPageLink :: MonadIO m => Day -> Manager -> m (Maybe T.Text)
getPageLink dt mgr = do
    let (ano, mes, dia) = toGregorian dt
        url             = "http://www.campinas.sp.gov.br/diario-oficial/index.php?mes=" ++ show mes ++ "&ano=" ++ show ano
    req <- liftIO $ parseRequest url
    resp <- liftIO $ httpLbs req mgr
    let docCursor  = fromDocument $ parseLBS $ responseBody resp
        todosLinks = docCursor $// element "td" &// element "a" >=> hasAttribute "href" &| node
        diaText    = toS (show dia)
        linkDoDia  = listToMaybe $ fmap (attr "href") $ filter ((==diaText) . nodeText) $ todosLinks
    case linkDoDia of
        Nothing -> return Nothing
        Just l  -> return . Just . toS $ "http://www.campinas.sp.gov.br/diario-oficial/" </> toS l

data CampinasCrawler = CampinasCrawler
instance IsCrawler CampinasCrawler where
    toCrawler _ = Crawler {
        crawlerNome = "Diário da cidade de Campinas",
        crawlerOrigemDiarioId = OrigemDiarioId 1,
        findLinks = \dt mgr -> do
            link <- getPageLink dt mgr
            case link of
                Nothing -> return CrawlArquivoNaoExiste
                Just l -> return $ CrawlDiarios [l]
    }