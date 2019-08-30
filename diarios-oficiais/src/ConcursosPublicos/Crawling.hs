{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module ConcursosPublicos.Crawling where

import Model.Diarios
import Data.Time
import Data.Text
import Network.HTTP.Client
import Control.Monad.IO.Class

class IsCrawler c where
    toCrawler :: c -> Crawler

data Crawler = Crawler {
    crawlerNome :: Text,
    crawlerOrigemDiarioId :: OrigemDiarioId,
    findLinks :: forall m. MonadIO m => Day -> Manager -> m CrawlResult
}
instance Show Crawler where
    show c = unpack $ crawlerNome c

data CrawlResult = CrawlDiarios [Text] | CrawlError Text | CrawlArquivoNaoExiste