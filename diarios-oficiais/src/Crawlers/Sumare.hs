
module Crawlers.Sumare where

import Model.Diarios
import DiariosOficiais.Crawling
import Database.Beam
import Data.Time
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client
import qualified Text.XML as XML
import Text.HTML.DOM
import Data.String.Conv
import Text.XML.Cursor
import System.FilePath
import DiariosOficiais.Xml
import qualified Data.Attoparsec.Text as AT

dataPtBrParser :: AT.Parser Day
dataPtBrParser = do
    dia <- AT.decimal
    _ <- AT.char '/'
    mes <- AT.decimal
    _ <- AT.char '/'
    ano <- AT.decimal
    case fromGregorianValid ano mes dia of
        Nothing -> fail "Data inválida"
        Just dt -> return dt

-- | Não encontrei uma função que extraia todo o conteúdo de um Cursor pronta..
conteudoCursor :: Cursor -> T.Text
conteudoCursor c = conteudo' (node c)
  where conteudo' (XML.NodeContent t) = t
        conteudo' (XML.NodeElement e) = T.concat $ fmap conteudo' $ XML.elementNodes e
        conteudo' _                   = ""

cursorComData :: Day -> Cursor -> Bool
cursorComData dt cursorNo = case AT.parseOnly (dataPtBrParser <* AT.endOfInput) (conteudoCursor cursorNo) of
                                    Left _  -> False
                                    Right d -> d == dt

getPageLink :: MonadIO m => Day -> Manager -> m (Maybe T.Text)
getPageLink dt mgr = do
    let url = "https://www.sumare.sp.gov.br/"
    req <- liftIO $ parseRequest url
    resp <- liftIO $ httpLbs req mgr
    let docCursor  = fromDocument $ parseLBS $ responseBody resp
        linksDoDia = docCursor $// (element "div" >=> attributeIs "id" "modalDiario") &// element "i" >=> check (cursorComData dt) &| parent &| node
        linkDoDia  = listToMaybe $ fmap (attr "href") $ concat linksDoDia
    case linkDoDia of
        Nothing -> return Nothing
        Just l  -> return . Just . toS $ url </> toS l

data SumareCrawler = SumareCrawler
instance IsCrawler SumareCrawler where
    toCrawler _ = Crawler {
        crawlerNome = "Diário da cidade de Sumaré",
        crawlerOrigemDiarioId = OrigemDiarioId 2,
        findLinks = \dt mgr -> do
            link <- getPageLink dt mgr
            case link of
                Nothing -> return CrawlArquivoNaoExiste
                Just l -> return $ CrawlDiarios [l]
    }