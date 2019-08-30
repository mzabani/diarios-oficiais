module PdfParser.DocumentoParserInteligente where

import RIO
import qualified RIO.List as List
import qualified RIO.Text as Text
import PdfParser.Estruturas
import Text.EditDistance

retirarCabecalhos :: [Page] -> [Bloco]
retirarCabecalhos pgs =
    -- Procuramos blocos quase-idênticos no início de cada página, onde quase-idêntico significa:
    -- 1. Comparação um a um em ordem de documento
    -- 2. Tolerância para com posições ligeiramente diferentes (até 5 caracteres de diferença horizontal e vertical (50px) )
    -- 3. Tolerância na diferença de texto de até 4 caracteres (pense página 999 mudando para página 1000)
    let
        attrsEq :: AttrsBloco -> AttrsBloco -> Bool
        attrsEq a1 a2 = fontSizeBloco a1 == fontSizeBloco a2 
                        && isJust (left a1) == isJust (left a2) && maybe True ((<= 50) . abs) ((-) <$> left a1 <*> left a2)
                        && isJust (top a1) == isJust (top a2) && maybe True ((<= 50) . abs) ((-) <$> top a1 <*> top a2)

        textoEq :: Text -> Text -> Bool
        textoEq t1 t2 = t1 == t2 || levenshteinDistance defaultEditCosts (Text.unpack t1) (Text.unpack t2) <= 4

        blocosEq :: Bloco -> Bloco -> Bool
        blocosEq b1@(Bloco attrs1 telsOrBlocos1) b2@(Bloco attrs2 telsOrBlocos2) = attrsEq attrs1 attrs2 &&
            case (telsOrBlocos1, telsOrBlocos2) of
                (Left _, Left _) -> textoEq (textoBloco b1) (textoBloco b2)
                (Right blcs1, Right blcs2) -> length blcs1 == length blcs2 &&  all (\(ba, bb) -> blocosEq ba bb) (zip blcs1 blcs2)
                _ -> False
    
        blocosPorLinha :: [[Bloco]]
        blocosPorLinha = List.transpose $ fmap pageBlocos pgs

        primeiraLinhaDeConteudo = fromMaybe 0 $ List.findIndex (\blcs -> not $ all (uncurry blocosEq) $ zip blcs (drop 1 blcs)) blocosPorLinha
    in
        -- TODO: Muitas páginas não possuem o mesmo cabeçalho.. o negócio é encontrar subsequências contínuas mais-longas com >= 3 elementos (por exemplo)
        --       e retirar cabeçalhos destas..
        mconcat $ fmap (drop primeiraLinhaDeConteudo . pageBlocos) pgs

