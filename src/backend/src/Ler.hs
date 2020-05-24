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

lerParagrafosAntes :: Pool Connection -> Int -> Servant.Handler (Int, [(Int, Text)])
lerParagrafosAntes connPool paragrafoId = fmap (paragrafoId,) $
    withDbConnection connPool $ \conn ->
        -- Pegamos os 2 parágrafos anteriores
        liftIO $ query conn "WITH p AS (select conteudodiarioid, ordem from paragrafodiario where id = ?) select paragrafodiario.id, paragrafodiario.conteudo FROM paragrafodiario JOIN p USING (conteudodiarioid) WHERE paragrafodiario.ordem BETWEEN p.ordem - 2 AND p.ordem - 1 ORDER BY paragrafodiario.ordem" (Only paragrafoId)

lerParagrafosDepois :: Pool Connection -> Int -> Servant.Handler (Int, [(Int, Text)])
lerParagrafosDepois connPool paragrafoId = fmap (paragrafoId,) $
    withDbConnection connPool $ \conn ->
        -- Pegamos os 2 parágrafos posteriores
        liftIO $ query conn "WITH p AS (select conteudodiarioid, ordem from paragrafodiario where id = ?) select paragrafodiario.id, paragrafodiario.conteudo FROM paragrafodiario JOIN p USING (conteudodiarioid) WHERE paragrafodiario.ordem BETWEEN p.ordem + 1 AND p.ordem + 2 ORDER BY paragrafodiario.ordem" (Only paragrafoId)