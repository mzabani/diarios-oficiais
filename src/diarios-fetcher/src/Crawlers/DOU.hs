module Crawlers.DOU (DOU1Crawler(..), DOU2Crawler(..), DOU3Crawler(..)) where

import RIO
import qualified RIO.List as List
import Model.Diarios
import DiariosOficiais.Crawling
import Data.Time
import Data.Aeson (withObject, decode, FromJSON(..), (.:))
import qualified Data.Text as T
import Network.HTTP.Client
import Text.HTML.DOM
import Data.String.Conv
import Text.XML.Cursor hiding (Cursor)
import Text.XML.Cursor.Generic (Cursor)
import Text.XML (Node)
import DiariosOficiais.Xml (nodeText)

getLinks :: (MonadThrow m, MonadIO m) => Int -> Day -> Manager -> m (Maybe [T.Text])
getLinks numeroDiarioOficial dt mgr = do
    let (ano, mes, dia) = toGregorian dt
        diaMesAno = toS (tshow dia) <> "-" <> toS (tshow mes) <> "-" <> toS (tshow ano)
        url             = "http://www.in.gov.br/leiturajornal?secao=do" <> show numeroDiarioOficial <> "&data=" <> diaMesAno
    req <- liftIO $ parseRequest url
    resp <- liftIO $ httpLbs req mgr
    let docCursor  = fromDocument $ parseLBS $ responseBody resp
        jsonSummaryNodes = mconcat $ docCursor $// element "script" &| attributeIs "id" "params" &| node
    case jsonSummaryNodes of
        [encodeUtf8 . nodeText -> jsonSummary] ->
            case decode (toS jsonSummary) of
                Nothing -> return Nothing
                Just TopLevelDOU1 { jsonArray = [] } -> return Nothing
                Just TopLevelDOU1 { jsonArray } -> return $ Just $ fmap (("http://www.in.gov.br/web/dou/-/" <>) . urlTitle) $ List.sortOn numberPage jsonArray
        _ -> return Nothing

data TopLevelDOU1 = TopLevelDOU1 {
    jsonArray :: ![UrlEPagina]
} deriving (Generic, FromJSON)

data UrlEPagina = UrlEPagina { urlTitle :: !Text, numberPage :: !Int }
instance FromJSON UrlEPagina where
    parseJSON = withObject "UrlEPagina" $ \o -> UrlEPagina <$> o .: "urlTitle" <*> (fromMaybe (error "numberPage não é inteiro") . readMaybe <$> o .: "numberPage")

douParagrafoAxis :: Cursor Node -> [Node]
douParagrafoAxis doc = mconcat $ doc $// element "div" &| attributeIs "class" "texto-dou" &// element "p" &| node

toCrawlerNum :: IsCrawler c => Int -> OrigemDiarioId -> c -> Crawler
toCrawlerNum numeroDiarioOficial origemId _ = Crawler {
        crawlerNome = "Diário Oficial da União - Seção " <> tshow numeroDiarioOficial,
        crawlerOrigemDiarioId = origemId,
        findLinks = \dt mgr -> do
            links <- getLinks numeroDiarioOficial dt mgr
            case links of
                Nothing -> return CrawlArquivoNaoExiste
                Just l -> return $ CrawlDiarios l $ ProcessarHtml douParagrafoAxis
    }

data DOU1Crawler = DOU1Crawler
instance IsCrawler DOU1Crawler where
    toCrawler = toCrawlerNum 1 (OrigemDiarioId 3)

data DOU2Crawler = DOU2Crawler
instance IsCrawler DOU2Crawler where
    toCrawler = toCrawlerNum 2 (OrigemDiarioId 4)

data DOU3Crawler = DOU3Crawler
instance IsCrawler DOU3Crawler where
    toCrawler = toCrawlerNum 3 (OrigemDiarioId 5)