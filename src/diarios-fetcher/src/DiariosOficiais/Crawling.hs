module DiariosOficiais.Crawling where

import RIO
import Model.Diarios
import Data.Time
import Data.Text
import Network.HTTP.Client

class IsCrawler c where
    toCrawler :: c -> Crawler

data Crawler = Crawler {
    crawlerNome :: Text,
    crawlerOrigemDiarioId :: OrigemDiarioId,
    findLinks :: forall m. (MonadIO m, MonadThrow m) => Day -> Manager -> m CrawlResult
}
instance Show Crawler where
    show c = unpack $ crawlerNome c

data CrawlResult = CrawlDiarios [Text] | CrawlError Text | CrawlArquivoNaoExiste