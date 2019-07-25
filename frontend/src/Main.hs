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
htmlHead = do
    elAttr "meta" (Map.fromList [("charset", "utf-8")]) (pure ())
    elAttr "meta" (Map.fromList [("name", "viewport"), ("content", "width=device-width, initial-scale=1, shrink-to-fit=no")]) (pure ())
    elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href", "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"), ("integrity", "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"), ("crossorigin", "anonymous")]) (pure ())
    el "title" (text "Buscador de Diários Oficiais")

renderValor (ValorTexto t) = text t
renderValor (ValorLista l) = el "ul" $ forM_ l $ \v -> el "li" $ renderValor v
renderValor (ValorMatch antes match depois) = text antes >> el "strong" (text match) >> text depois

htmlBody :: Widget x ()
htmlBody = do
    elClass "div" "container" $ do
        elClass "div" "row" $ do
            elClass "div" "col-sm-12" $ do
                input <- elClass "div" "mx-auto col-sm-12 col-md-8 col-lg-4" $
                    elClass "div" "input-group" $ do
                        input <- textInput def
                        elClass "div" "input-group-append" $
                            elClass "button" "btn-outline-secondary" (text "Lupa")
                        elClass "small" "form-text text-muted" $ text "Tente por exemplo \"data, diario, conteudo sendo: multa\" e aperte ENTER"
                        return input
                let dynStr = _textInput_value input
                    queries = tag (current dynStr) (keypress Enter input)
                resultadosEv <- search queries
                resultadosDyn <- holdDyn Nothing resultadosEv
                dyn_ $ ffor resultadosDyn $ \case
                        Nothing -> pure ()
                        Just (ErroBusca erroMsg) -> el "div" $ text erroMsg
                        Just (Resultados r) -> elClass "table" "table table-bordered" $ do
                            el "tr" $
                                forM_ (colunas r) $ \col -> el "th" $ text col
                            forM_ (resultados r) $ \res -> el "tr" $
                                forM_ res $ \v -> el "td" $ renderValor v