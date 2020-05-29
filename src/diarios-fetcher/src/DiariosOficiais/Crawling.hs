module DiariosOficiais.Crawling where

import RIO
import Model.Diarios
import Data.Time
import Data.Text
import Network.HTTP.Client
import Text.XML (Node)
import Text.XML.Cursor (Cursor)

class IsCrawler c where
    toCrawler :: c -> Crawler

data Crawler = Crawler {
    crawlerNome :: Text,
    crawlerOrigemDiarioId :: OrigemDiarioId,
    findLinks :: forall m. (MonadIO m, MonadThrow m) => Day -> Manager -> m CrawlResult
}
instance Show Crawler where
    show c = unpack $ crawlerNome c

data ProcessadorConteudoLink = ProcessarPdf | ProcessarHtml (Cursor -> [Node])

data CrawlResult = CrawlDiarios [Text] ProcessadorConteudoLink  | CrawlError Text | CrawlArquivoNaoExiste