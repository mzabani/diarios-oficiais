module PdfParser.HtmlParser where

import RIO
import RIO.Directory
import RIO.FilePath
import qualified RIO.List as List
import qualified RIO.Text as Text
import qualified Data.Text as Text (replace)
import qualified RIO.Map as Map
import qualified Data.Attoparsec.Text as Parsec
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import System.Process.Typed
import PdfParser.Estruturas
import System.Random
import Text.XML.Cursor
import Text.XML
import qualified Text.HTML.DOM as HTML

parsePdf :: (MonadIO m, MonadUnliftIO m) => [FilePath] -> m (Either () [Secao])
parsePdf pdfFilePaths = do
    someRandomNumber :: Int <- liftIO randomIO
    allPages <-
        forM pdfFilePaths $ \pdfPath -> do
            bracket ((</> (show someRandomNumber <> "-html")) <$> getTemporaryDirectory) removeDirectoryRecursive $ \tempDir -> do
                liftIO $ print tempDir
                
                _ <- readProcess (shell $ "pdftohtml \"" ++ pdfPath ++ "\" \"" ++ tempDir ++ "\"")
                allHtmlFiles <- listDirectory tempDir
                let pgNumParser = Parsec.string "page" >> Parsec.decimal >>= \d -> Parsec.string ".html" >> return (d :: Int)
                    paginasEmOrdem = fmap fst $ List.sortOn snd $ rights $ fmap (\s -> (,) s <$> Parsec.parseOnly pgNumParser (Text.pack s)) allHtmlFiles
                forM paginasEmOrdem $ \nomePagina -> do
                    let htmlPath = tempDir </> nomePagina
                    pg <- liftIO $ HTML.readFile htmlPath
                    let docCursor  = fromDocument pg
                        todosDivs = docCursor $// element "div" >=> hasAttribute "class" &| node
                        divsEls = catMaybes $ fmap (\case NodeElement el -> Just el
                                                          _              -> Nothing) todosDivs
                    return $ Right (divListToPage divsEls)
        
    case sequenceA (mconcat allPages) of
        Left err -> return $ Left err
        Right pgs -> do
            let doc = detalharDocumento pgs
                secoesDoc = obterSecoes doc
            
            return (Right secoesDoc)

divListToPage :: [Element] -> Page
divListToPage blocks' = 
    let
        blocos = fmap (elementToBloco emptyAttrs) blocks'
    in Page { pageBlocos = fmap (simplificarBloco emptyAttrs) blocos }

    where
          elementToBloco :: AttrsBloco -> Element -> Bloco
          elementToBloco attrs (Element _ divAttrs inlineNodes) = Bloco (mixAttrs attrs (makeAttrs (Map.toList divAttrs))) $ Left $ makeText inlineNodes

          makeText :: [Node] -> [TextEl]
          makeText inls = RIO.concatMap inlineToText inls

          inlineToText :: Node -> [TextEl]
          inlineToText (NodeContent s) = [ TextEl { atributos = emptyAttrs, texto = Left s } ]
          inlineToText (NodeElement (Element _ elAttrs els)) = [ TextEl { atributos = makeAttrs (Map.toList elAttrs), texto = Right (makeText els) } ]
          inlineToText el = error $ "inlineToText " <> show el

          simplificarBloco :: AttrsBloco -> Bloco -> Bloco
          simplificarBloco attrsPai (Bloco attrsBloco telsOrBlocos) =
            let
                newAttrs = mixAttrs attrsPai attrsBloco
                bFinal =
                    case telsOrBlocos of
                        Left tels -> Bloco newAttrs (Left (simplificarTextElsList (AttrsInline { fontSizeInline = fontSize newAttrs }) tels))
                        Right [ blocoFilhoUnico@(Bloco attrsFilhoUnico _) ] -> simplificarBloco (mixAttrs newAttrs attrsFilhoUnico) blocoFilhoUnico
                        Right blocos -> Bloco newAttrs $ Right $ fmap (simplificarBloco newAttrs) blocos
            in
                bFinal

          simplificarTextEl :: AttrsInline -> TextEl -> TextEl
          simplificarTextEl attrsPai tel =
            let
                attrsTel = mixAttrs attrsPai (atributos tel)
                telQuaseFinal =
                    case texto tel of
                        Left t ->
                            case t of
                                "ﬁ" -> tel { texto = Left "fi", atributos = attrsPai } -- TODO: Não está funcionando
                                _ -> tel { texto = Left (Text.replace "ﬁ" "fi" t), atributos = attrsTel }
                        Right [ singleTel ] ->
                            -- Podemos substituir um Inline com um único filho pelo próprio filho
                            simplificarTextEl (mixAttrs attrsTel (atributos singleTel)) singleTel
                        Right tels ->
                            let
                                telsSimplificados = simplificarTextElsList attrsTel tels
                                telSimplificado = tel { texto = Right telsSimplificados, atributos = attrsTel }
                            in
                                if length telsSimplificados /= length tels then simplificarTextEl attrsPai telSimplificado
                                else telSimplificado
            in
                telQuaseFinal
          
          simplificarTextElsList :: AttrsInline -> [TextEl] -> [TextEl]
          simplificarTextElsList attrsPai tels = fmap concatTextEls $ NonEmpty.groupBy atributosIguais $ fmap (simplificarTextEl attrsPai) $ filter (not . telEmpty) tels

          telEmpty :: TextEl -> Bool
          telEmpty tel = case texto tel of
            Left "" -> True
            Right [] -> True
            _ -> False
        
          atributosIguais :: TextEl -> TextEl -> Bool
          atributosIguais tel1 tel2 = atributos tel1 == atributos tel2
  
          concatTextEls (telPrim :| tels) = foldl' (\tel1 tel2 -> TextEl { atributos = atributos tel1, texto = Left (textoTel tel1 <> textoTel tel2) }) telPrim tels