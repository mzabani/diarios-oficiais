import Reflex.Dom
import Reflex.Dom.Xhr
import Data.Aeson
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Calendar
import Control.Monad
import Common

search queries = do
  responsesEv <- performRequestAsync $ toRequest <$> queries
  return $ decodeXhrResponse <$> responsesEv
  where toRequest termo = postJson "http://localhost:8080/busca" (FormBusca termo)

main = mainWidgetWithHead htmlHead htmlBody

htmlHead :: Widget x ()
htmlHead = 
    elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href", "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"), ("integrity", "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"), ("crossorigin", "anonymous")]) (pure ())

htmlBody :: Widget x ()
htmlBody = do
    el "div" $ text "Busque nos diários!"
    input <- textInput def
    let dynStr = _textInput_value input
        --queries = updated $ _element dynStr
        queries = tag (current dynStr) (keypress Enter input)
    resultadosEv <- search queries
    resultadosDyn <- holdDyn Nothing resultadosEv
    dyn_ $ ffor resultadosDyn $ \case
            Nothing -> el "div" $ text "Comece a digitar e aperte ENTER para buscar"
            Just (ErroBusca erroMsg) -> el "div" $ text erroMsg
            Just (Resultados r) -> el "table" $
                el "tr" $
                    forM_ (colunas r) $ \col -> el "th" $ text col
                forM_ (valores r) $ \res -> el "tr" $
                    forM_ res $ \v -> el "td" $ text v
    -- dynStr <- _textInput_value <$> textInput def
    -- dyn_ $ ffor dynStr $ \str ->
    --     el "ul" $
    --         forM_ [1..T.length str] $ \i -> do
    --             el "li" $ text (T.pack (show i))