module PdfParser.PandocHtmlParser where

import RIO
import RIO.Directory
import RIO.FilePath
import qualified RIO.List as List
import qualified RIO.Text as Text
import qualified Data.Attoparsec.Text as Parsec
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import System.Process.Typed
import Text.Pandoc.Definition
import Text.Pandoc.Readers.HTML
import Text.Pandoc.Extensions
import Text.Pandoc.Options
import Text.Pandoc.Class
import Text.Pandoc.Error
import PdfParser.Estruturas
import System.Random
import Data.Default (def)

parsePdf :: (MonadIO m, MonadUnliftIO m) => [FilePath] -> m (Either PandocError [Secao])
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
                    pg <- readFileUtf8 htmlPath
                    let opts = def {
                        readerExtensions = pandocExtensions,
                        readerStripComments = True
                    }
                    case runPure (readHtml opts pg) of
                        Left err -> return $ Left err
                        Right pdoc -> return $ Right (pandocPageToDoc pdoc)

        
    case sequenceA (mconcat allPages) of
        Left err -> return $ Left err
        Right pgs -> do
            let doc = detalharDocumento pgs
                secoesDoc = obterSecoes doc
            -- forM_ (List.zip [1..30] secoesDoc) $ \(i, s) -> do
            --     let nomeArquivo = "./pdf-secao" <> show i <> ".txt"
            --     writeFileUtf8 nomeArquivo (printSecao s)
            --     readProcess (shell $ "code " <> nomeArquivo)
            
            return (Right secoesDoc)

pandocPageToDoc :: Pandoc -> Page
pandocPageToDoc (Pandoc _ blocks') = 
    let
        blocos = fmap (panBlockToBloco emptyAttrs) blocks'
    in Page { pageBlocos = fmap (simplificarBloco emptyAttrs) blocos }
    -- TODO: Melhor que pegar altura da imagem é procurar as linhas mais altas (e mais baixas) para ver linhas que se repetem em todas as páginas.
    --       Estas quase que certamento serão cabeçalhos e rodapés
    -- TODO: Linhas entre parágrafos devem ser inseridas calculando a diferença de altura entre blocos consecutivos para extrair a altura das linhas,
    --       e aí quando o salto for muito grande, temos um parágrafo
    where panBlockToBloco :: AttrsBloco -> Block -> Bloco
          panBlockToBloco attrs (Plain inls) = Bloco attrs (Left (makeText inls))
          panBlockToBloco attrs (Para inls) = Bloco attrs (Left (makeText inls))
          panBlockToBloco attrs (Div (_, _, divAttrs) blocks) = Bloco (mixAttrs attrs (makeAttrs divAttrs)) $ Right $ fmap (panBlockToBloco (mixAttrs attrs (makeAttrs divAttrs))) blocks
          panBlockToBloco _ el = error $ "panBlockToBloco " <> show el

          makeText :: [Inline] -> [TextEl]
          makeText inls = RIO.concatMap inlineToText inls

          inlineToText :: Inline -> [TextEl]
          inlineToText (Str s) = [ TextEl { atributos = emptyAttrs, texto = Left (Text.pack s) } ]
          inlineToText Space = [ TextEl { atributos = emptyAttrs, texto = Left " " } ]
          inlineToText LineBreak = [ TextEl { atributos = emptyAttrs, texto = Left "\n" } ]
          inlineToText SoftBreak = [ TextEl { atributos = emptyAttrs, texto = Left "\n" } ]
          inlineToText (Span (_, _, spanAttr) inls) = [ TextEl { atributos = makeAttrs spanAttr, texto = Right (makeText inls) } ]
          inlineToText (Image _ _ _) = [ ]
          inlineToText (Quoted quoteType inls) = let quoteTel = TextEl { atributos = emptyAttrs, texto = Left (if quoteType == SingleQuote then "'" else "\"") } in quoteTel : (makeText inls <> [quoteTel])
          inlineToText (Link (_, _, linkAttr) inls _) = [ TextEl { atributos = makeAttrs linkAttr, texto = Right (makeText inls) } ]
          inlineToText (Math InlineMath t) = [ TextEl { atributos = emptyAttrs, texto = Left (Text.pack t) } ]
          inlineToText (Math DisplayMath t) = [ TextEl { atributos = emptyAttrs, texto = Left ("\n" <> (Text.pack t) <> "\n") } ]
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
                            case (t, fontSizeInline (atributos tel)) of
                                ("ﬁ", Just _) -> tel { texto = Left "fi", atributos = attrsPai } -- TODO: Não está funcionando
                                _ -> tel { atributos = attrsTel }
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