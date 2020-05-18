module Ler where

import RIO
import Servant
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as Blaze
import Database.PostgreSQL.Simple
import Data.Pool
import BeamUtils


lerDiario :: Pool Connection -> Int -> Servant.Handler Blaze.Html
lerDiario connPool conteudoDiarioId =
    withDbConnection connPool $ \conn -> do
        res <- liftIO $ query conn "select conteudo from paragrafodiario where conteudodiarioid = ?" (Only conteudoDiarioId)
        case res of
            [] -> throwError err404
            _ -> return $ forM_ res $ (\(Only (t :: T.Text)) -> Blaze.p $ Blaze.toHtml t)