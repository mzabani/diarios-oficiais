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
    where toRequest termo = postJson "/busca" (FormBusca termo)
          toBuscaTerminada Nothing = BuscaTerminada $ ErroBusca "Aconteceu um erro interno na desserialização. Por favor reporte isso como um Bug."
          toBuscaTerminada (Just res) = BuscaTerminada res

main = mainWidgetWithHead htmlHead htmlBody

tshow = T.pack . show

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
                            elClass "small" "form-text text-muted" $ text "Tente buscar por exemplo por \"issqn\", \"empresa multa\" ou \"servidor exonerado\""
                            return resultadosDyn
                
                divClass "alert alert-warning" $ el "ul" $ do
                    el "li" $ text "Não me responsabilizo pelo conteúdo dos Diários exibidos/buscados. A importação dos Diários é um processo onde partes significativas destes podem se perder ou serem alteradas (e isso realmente acontece com frequência)."
                    el "ul" $ do
                        el "li" $ el "strong" (text "Sempre verifique o conteúdo encontrado") >> text " antes de assumir que está encontrando o que pensa estar buscando"
                        el "li" $ text "O buscador busca parágrafos, mas o que é exatamente um parágrafo aqui pode variar muito, e geralmente não corresponderá ao que humanos entendem por parágrafo"
                    el "li" $ text "O código deste buscador é aberto. Acesse " >> elAttr "a" ("href" =: "https://github.com/mzabani/diarios-oficiais") (text "https://github.com/mzabani/diarios-oficiais") >> text " para ver."
                    el "li" $ text "Por enquanto apenas o Diário Oficial da cidade de Campinas está disponível, " >> el "strong" (text "em geral") >> text " para os últimos 365 dias"
                    el "li" $ text "Para fins de pesquisa, a busca avançada pode ajudar: tente por exemplo \"boletim de ocorrência grupos:data diario\" para ver o total de BOs (de forma aproximada) por cidade e data. Mude os grupos (mas use apenas \"data\" e \"diario\") para ver outras métricas"
                    el "ul" $ do
                        el "li" $ text "Tente também \"habite-se diario:Campinas data>=2019-02-01\" para ver parágrafos com \"habite-se\" da cidade de Campinas a partir de 1º de fevereiro de 2019"
                        el "li" $ text "Tente também \"alvará deferido data>=2019-01-01 data<=2019-01-31 grupos:data\" para ver alvarás deferidos em todos diários oficiais disponíveis por data no mês de Janeiro/2019"
                        el "li" $ text "Tente também \"covid\" para ver medidas públicas relacionadas ao Corona Vírus"
                
                dyn_ $ ffor resultadosDyn $ \case
                        NadaBuscado -> pure ()
                        Buscando t -> text $ "Buscando por '" <> t <> "'..."
                        BuscaTerminada (ErroBusca erroMsg) -> el "div" $ text erroMsg
                        BuscaTerminada (Resultados _ (Resultado _ [])) -> el "div" $ text "Nenhum resultado encontrado.."
                        BuscaTerminada (Resultados qtdResultados r) -> do
                            el "div" $ text $ "Busca retornou " <> tshow qtdResultados <> " resultados. Exibindo os " <> tshow (length (resultados r)) <> " mais recentes"
                            elClass "table" "table table-bordered" $ do
                                el "tr" $
                                    forM_ (colunas r) $ \col -> el "th" $ text col
                                forM_ (resultados r) $ \(mConteudoDiarioId, res) -> el "tr" $
                                    forM_ res $ \v -> el "td" $ do
                                        case mConteudoDiarioId of
                                            Just conteudoDiarioId -> elAttr "a" ("href" =: ("/ler/" <> tshow conteudoDiarioId)) $ renderValor v
                                            Nothing -> renderValor v