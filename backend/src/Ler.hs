module Ler where

import RIO
import Servant
import Servant.API
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as Blaze
-- import qualified Text.Blaze as Blaze
import Network.Wai
import Network.Wai.Handler.Warp
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