import Data.Proxy
import Control.Lens ((&), (%~), (.~))
import Reflex.Dom
import Reflex.Dom.Xhr
import Reflex.Dom.Class
import Data.Aeson
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Calendar
import Control.Monad
import Common

data Busca = NadaBuscado | Buscando Text | BuscaTerminada ResultadoBusca

search querySearchedEv = do
    responsesEv <- performRequestAsync $ toRequest <$> querySearchedEv
    let buscaIniciadaEv = Buscando <$> querySearchedEv
        resultadosChegaramEv = toBuscaTerminada <$> (decodeXhrResponse <$> responsesEv)
    holdDyn NadaBuscado (leftmost [buscaIniciadaEv, resultadosChegaramEv])
    where toRequest termo = postJson "http://localhost:8080/busca" (FormBusca termo)
          toBuscaTerminada Nothing = BuscaTerminada $ ErroBusca "Aconteceu um erro interno na desserialização. Por favor reporte isso como um Bug."
          toBuscaTerminada (Just res) = BuscaTerminada res

main = mainWidgetWithHead htmlHead htmlBody

htmlHead :: Widget x ()
htmlHead = do
    elAttr "meta" (Map.fromList [("charset", "utf-8")]) (pure ())
    elAttr "meta" (Map.fromList [("name", "viewport"), ("content", "width=device-width, initial-scale=1, shrink-to-fit=no")]) (pure ())
    elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href", "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"), ("integrity", "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"), ("crossorigin", "anonymous")]) (pure ())
    el "title" (text "Buscador de Diários Oficiais")

renderValor (ValorTexto t) = text t
renderValor (ValorLista l) = el "ul" $ forM_ l $ \v -> el "li" $ renderValor v
renderValor (ValorMatch matches) = forM_ matches $ \(s, negrito) -> if negrito then el "strong" (text s) else text s

btn attrs disableEv contents = do
    allAttrs <- holdDyn attrs (fixAttrs <$> disableEv)
    elDynAttr "button" allAttrs contents
    where fixAttrs True  = Map.insert "disabled" "" attrs
          fixAttrs False = attrs

formPreventDefault :: forall t m a. DomBuilder t m => m a -> m (Element EventResult (DomBuilderSpace m) t, a)
formPreventDefault c =
    let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
            & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Submit (const preventDefault)
    in element "form" cfg c

htmlBody :: Widget x ()
htmlBody = do
    divClass "container" $ do
        divClass "row" $ do
            divClass "col-sm-12" $ mdo
                (form, resultadosDyn) <- formPreventDefault $ do
                    divClass "mx-auto col-sm-12 col-md-8 col-lg-4" $ mdo
                        divClass "input-group" $ mdo
                            input <- textInput def
                            let dynStr = _textInput_value input
                                querySearchedEv = tag (current dynStr) $ domEvent Submit form
                            resultadosDyn <- search querySearchedEv
                            let buscandoEv = fmap (\case Buscando _ -> True
                                                         _          -> False) $ updated resultadosDyn
                                btnAttrs = Map.fromList [ ("type", "submit"), ("class", "btn btn-outline-secondary") ]
                            divClass "input-group-append" $
                                btn btnAttrs buscandoEv (text "Buscar")
                            elClass "small" "form-text text-muted" $ text "Tente por exemplo \"data, diario, conteudo sendo: multa\" e aperte ENTER"
                            return resultadosDyn
                
                divClass "alert alert-warning" $ el "ul" $ do
                    el "li" $ text "Não me responsabilizo pelo conteúdo dos Diários exibidos/buscados. A importação dos Diários é um processo onde partes significativas destes podem se perder ou serem alteradas (e isso realmente acontece com frequência)."
                    el "li" $ text "O código deste buscador é aberto. Acesse " >> elAttr "a" ("href" =: "https://github.com/mzabani/buscador-diarios-oficiais") (text "https://github.com/mzabani/buscador-diarios-oficiais") >> text " para ver."
                    el "li" $ text "Mesmo se não for desenvolvedor, fique à vontade para pedir a inclusão de um Diário Oficial ou uma funcionalidade. Especialmente se for para finalidade de pesquisa. Acesso o link do Github acima e crie uma \"Issue\" se quiser."
                
                dyn_ $ ffor resultadosDyn $ \case
                        NadaBuscado -> pure ()
                        Buscando t -> text $ "Buscando por '" <> t <> "'..."
                        BuscaTerminada (ErroBusca erroMsg) -> el "div" $ text erroMsg
                        BuscaTerminada (Resultados (Resultado _ [])) -> el "div" $ text "Nenhum resultado encontrado.."
                        BuscaTerminada (Resultados r) -> elClass "table" "table table-bordered" $ do
                            el "tr" $
                                forM_ (colunas r) $ \col -> el "th" $ text col
                            forM_ (resultados r) $ \res -> el "tr" $
                                forM_ res $ \v -> el "td" $ renderValor v