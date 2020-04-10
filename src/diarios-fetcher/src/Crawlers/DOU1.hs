module Crawlers.DOU1 where

import RIO
import Model.Diarios
import DiariosOficiais.Crawling
import Data.Time
import qualified Data.Text as T
import Network.HTTP.Client
import Text.HTML.DOM
import Data.String.Conv
import Text.XML.Cursor
import System.FilePath
import DiariosOficiais.Xml

getPageLink :: (MonadThrow m, MonadIO m) => Day -> Manager -> m (Maybe T.Text)
getPageLink dt mgr = do
    let (ano, mes, dia) = toGregorian dt
        diaMes = toS (tshow dia) <> "/" <> toS (tshow mes)
        url             = "http://pesquisa.in.gov.br/imprensa/core/jornalList.action"
    initReq <- liftIO $ parseRequest url
    let req = urlEncodedBody [("jornal", "do1"), ("edicao.dtInicio", diaMes), ("edicao.dtFim", diaMes), ("edicao.ano", toS $ tshow ano)] initReq
    resp <- liftIO $ httpLbs req mgr
    liftIO $ print (responseBody resp)
    let docCursor  = fromDocument $ parseLBS $ responseBody resp
        todosLinks = docCursor $// element "td" &// element "a" >=> hasAttribute "href" &| node
        diaText    = toS (show dia)
        linkDoDia  = listToMaybe $ fmap (attr "href") $ filter ((==diaText) . nodeText) $ todosLinks
    case linkDoDia of
        Nothing -> return Nothing
        Just l  -> return . Just . toS $ "http://www.campinas.sp.gov.br/diario-oficial/" </> toS l

data DOU1Crawler = DOU1Crawler
instance IsCrawler DOU1Crawler where
    toCrawler _ = Crawler {
        crawlerNome = "Diário Oficial da União - Seção 1",
        crawlerOrigemDiarioId = OrigemDiarioId 3,
        findLinks = \dt mgr -> do
            pageLink <- getPageLink dt mgr
            case pageLink of
                Nothing -> return CrawlArquivoNaoExiste
                Just l -> return $ CrawlDiarios [l]
    }