module Ler where

import RIO
import Servant
import Servant.API
import Network.Wai
import Network.Wai.Handler.Warp
import Database.PostgreSQL.Simple
import Data.Pool
import BeamUtils


lerDiario :: Pool Connection -> Int -> Servant.Handler Text
lerDiario connPool conteudoDiarioId =
    withDbConnection connPool $ \conn -> do
        res <- liftIO $ query conn "select conteudo from conteudodiario where id = ?" (Only conteudoDiarioId)
        case res of
            [] -> throwError err404
            [Only conteudo] -> return conteudo
            _ -> throwError err500