{-# LANGUAGE PartialTypeSignatures #-}
import Data.Proxy
import Data.Functor ((<&>))
import Control.Lens ((&), (%~), (.~))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.These (These)
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

data Busca = NadaBuscado | Buscando (Text, Int) | BuscaTerminada ResultadoBusca

data ListandoParagrafos = SemListar | Listando Int | ListaObtida (Maybe (Int, [(Int, Text)])) deriving Show

search querySearchedEv = do
    responsesEv <- performRequestAsync $ toRequest <$> querySearchedEv
    let buscaIniciadaEv = Buscando <$> querySearchedEv
        resultadosChegaramEv = toBuscaTerminada <$> (decodeXhrResponse <$> responsesEv)
    holdDyn NadaBuscado (leftmost [buscaIniciadaEv, resultadosChegaramEv])
    where toRequest (termo, pagina) = xhrRequest "GET" ("/busca?q=" <> termo <> "&p=" <> tshow pagina) def
          toBuscaTerminada Nothing = BuscaTerminada $ ErroBusca "Aconteceu um erro interno. Por favor atualize a página e tente novamente. Se persistir, reporte isso como um Bug."
          toBuscaTerminada (Just res) = BuscaTerminada res

listarParagrafos baseUrl paragrafoListEv = do
    responsesEv <- performRequestAsync $ toRequest <$> paragrafoListEv
    let buscaIniciadaEv = Listando <$> paragrafoListEv
        resultadosChegaramEv = toBuscaTerminada <$> (decodeXhrResponse <$> responsesEv)
    holdDyn SemListar (leftmost [buscaIniciadaEv, resultadosChegaramEv])
    where toRequest pid = xhrRequest "GET" (baseUrl <> tshow pid) def
          toBuscaTerminada = ListaObtida

main = mainWidgetWithHead htmlHead htmlBody

tshow = T.pack . show

htmlHead :: Widget x ()
htmlHead = do
    -- Global site tag (gtag.js) - Google Analytics
    elAttr "script" (Map.fromList [ ("async", ""), ("src", "https://www.googletagmanager.com/gtag/js?id=UA-166907630-1") ]) (pure ())
    el "script" $ text $
                        "window.dataLayer = window.dataLayer || [];\n"
                        <> "function gtag(){dataLayer.push(arguments);}\n"
                        <> "gtag('js', new Date());\n"
                        <> "gtag('config', 'UA-166907630-1');"

    elAttr "meta" (Map.fromList [("charset", "utf-8")]) (pure ())
    elAttr "meta" (Map.fromList [("name", "viewport"), ("content", "width=device-width, initial-scale=1, shrink-to-fit=no")]) (pure ())
    elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href", "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"), ("integrity", "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"), ("crossorigin", "anonymous")]) (pure ())
    el "title" (text "Buscador de Diários Oficiais")

renderValor (ValorTexto t) = text t
renderValor (ValorLista l) = el "ul" $ forM_ l $ \v -> el "li" $ renderValor v
renderValor (ValorMatch matches) = forM_ matches $ \(s, negrito) -> if negrito then el "strong" (text s) else text s

elOnClick elName className contents = do
    (e, _) <- elClass' elName className contents
    return $ domEvent Click e

btn attrs disableEv contents = do
    allAttrs <- holdDyn attrs (fixAttrs <$> disableEv)
    elDynAttr' "button" allAttrs contents
    where fixAttrs True  = Map.insert "disabled" "" attrs
          fixAttrs False = attrs

btnClass className disableEv contents = do
    (b, _) <- btn (Map.fromList [("type", "button"), ("class", className)]) disableEv contents
    return $ domEvent Click b

dynEv :: forall t m a b. (Reflex t, MonadHold t m, Adjustable t m, NotReady t m, PostBuild t m) => Dynamic t a -> (a -> m (Event t b)) -> m (Event t b)
dynEv dynObj = join . fmap (switchHold never) . dyn . ffor dynObj

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
                (form, (resultadosDyn, pgAtualDyn)) <- formPreventDefault $ do
                    divClass "mx-auto col-sm-12 col-md-8 col-lg-4" $ mdo
                        divClass "input-group" $ mdo
                            input <- inputElement $ def & inputElementConfig_elementConfig %~ elementConfig_initialAttributes .~ Map.fromList [("autofocus", "")]
                            pgAtualDyn <- holdDyn 1 pgClickEv
                            let formSubmitEv = domEvent Submit form
                                searchFields = zipDyn (_inputElement_value input) pgAtualDyn
                                querySearchedEv = tag (current searchFields) $ leftmost [ formSubmitEv, () <$ updated pgAtualDyn ]
                            resultadosDyn <- search querySearchedEv
                            let buscandoEv = fmap (\case Buscando _ -> True
                                                         _          -> False) $ updated resultadosDyn
                                btnAttrs = Map.fromList [ ("type", "submit"), ("class", "btn btn-outline-secondary") ]
                            divClass "input-group-append" $
                                btn btnAttrs buscandoEv (text "Buscar")
                            elClass "small" "form-text text-muted" $ text "Tente buscar por exemplo por \"issqn\", \"empresa multa\" ou \"servidor exonerado\""
                            return (resultadosDyn, pgAtualDyn)
                
                divClass "alert alert-warning" $ el "ul" $ do
                    el "li" $ text "Não me responsabilizo pelo conteúdo dos Diários exibidos/buscados. A importação dos Diários é um processo onde partes significativas destes podem se perder ou serem alteradas (e isso realmente acontece com frequência)."
                    el "ul" $ do
                        el "li" $ el "strong" (text "Sempre verifique o conteúdo encontrado") >> text " antes de assumir que encontrou o que pensava estar buscando"
                        el "li" $ text "O buscador busca parágrafos, mas o que é exatamente um parágrafo aqui pode variar muito, e geralmente não corresponderá ao que humanos entendem por parágrafo"
                    el "li" $ text "O código deste buscador é aberto. Acesse " >> elAttr "a" ("href" =: "https://github.com/mzabani/diarios-oficiais") (text "https://github.com/mzabani/diarios-oficiais") >> text " para ver."
                    el "li" $ text "Por enquanto apenas o Diário Oficial da cidade de Campinas está disponível, " >> el "strong" (text "em geral") >> text " para os últimos 365 dias"
                    el "li" $ text "Para fins de pesquisa, a busca avançada pode ajudar: tente por exemplo \"boletim de ocorrência grupos:data diario\" para ver o total de BOs (de forma aproximada) por cidade e data. Mude os grupos (mas use apenas \"data\" e \"diario\") para ver outras métricas"
                    el "ul" $ do
                        el "li" $ text "Tente também \"habite-se diario:Campinas data>=2019-02-01\" para ver parágrafos com \"habite-se\" da cidade de Campinas a partir de 1º de fevereiro de 2019"
                        el "li" $ text "Tente também \"alvará deferido data>=2019-01-01 data<=2019-01-31 grupos:data\" para ver alvarás deferidos em todos diários oficiais disponíveis por data no mês de Janeiro/2019"
                        el "li" $ text "Tente também \"covid\" para ver medidas públicas relacionadas ao Corona Vírus"
                
                pgClickEv :: Event DomTimeline Int <- dynEv resultadosDyn $ \case
                        NadaBuscado -> return never
                        Buscando (t, _) -> do
                            text $ "Buscando por '" <> t <> "'..."
                            return never
                        BuscaTerminada (ErroBusca erroMsg) -> do
                            el "div" $ text erroMsg
                            return never
                        BuscaTerminada (Resultados _ (Resultado _ [])) -> do
                            el "div" $ text "Nenhum resultado encontrado.."
                            return never
                        BuscaTerminada (Resultados qtdResultados r) -> do
                            el "div" $ text $ "Busca retornou " <> tshow qtdResultados <> " resultados, exibidos do mais recente ao mais antigo."
                            elClass "table" "table table-bordered" $ do
                                el "tr" $
                                    forM_ (colunas r) $ \col -> el "th" $ text col
                                forM_ (resultados r) $ \(mParagrafoId, res) -> el "tr" $ do
                                    let
                                        primeirasColunas = take (length res - 1) res
                                        colunaParagrafo = drop (length res - 1) res
                                    forM_ primeirasColunas $ \v -> el "td" $ renderValor v
                                    forM_ colunaParagrafo $ \v -> el "td" $ do
                                        case mParagrafoId of
                                            Just paragrafoId -> mdo
                                                carregarAnterioresClickEv <- elClass "p" "text-center" $ btnClass "btn btn-link" never $ text "Carregar parágrafos anteriores"
                                                paragrafosAnterioresDyn <- traceDynWith show <$> listarParagrafos "/listar-paragrafos-anteriores/" (tag (current pidMaisAntigoDyn) carregarAnterioresClickEv)
                                                paragrafosPosterioresDyn <- listarParagrafos "/listar-paragrafos-posteriores/" (tag (current pidMaisRecenteDyn) carregarPosterioresClickEv)
                                                pidMaisAntigoDyn <- holdDyn paragrafoId $ fforMaybe (updated paragrafosAnterioresDyn) $ \case
                                                        SemListar -> Nothing
                                                        Listando _ -> Nothing
                                                        ListaObtida Nothing -> Nothing
                                                        ListaObtida (Just (_, ps)) -> fst <$> listToMaybe ps
                                                pidMaisRecenteDyn <- holdDyn paragrafoId $ fforMaybe (updated paragrafosPosterioresDyn) $ \case
                                                        SemListar -> Nothing
                                                        Listando _ -> Nothing
                                                        ListaObtida Nothing -> Nothing
                                                        ListaObtida (Just (_, ps)) -> fst <$> listToMaybe (reverse ps)
                                                -- Até o merge de https://github.com/reflex-frp/reflex-dom/pull/255, precisamos verificar
                                                -- se a Response já foi processada armazenando o paragrafoId mais recente..
                                                todosParagrafosAnterioresDyn <- foldDynMaybe (\mpps (ultimoPid, todosPs) -> 
                                                    case mpps of
                                                        Nothing -> Nothing
                                                        Just (pid, ps) ->
                                                            if pid == ultimoPid then Nothing else Just (pid, ps ++ todosPs)) ((-1), []) $ updated paragrafosAnterioresDyn <&> \case
                                                                SemListar -> Nothing
                                                                Listando _ -> Nothing
                                                                ListaObtida Nothing -> Nothing
                                                                ListaObtida (Just (pid, ps)) -> Just (pid, fmap snd ps)
                                                todosParagrafosPosterioresDyn <- foldDynMaybe (\mpps (ultimoPid, todosPs) -> 
                                                    case mpps of
                                                        Nothing -> Nothing
                                                        Just (pid, ps) ->
                                                            if pid == ultimoPid then Nothing else Just (pid, todosPs ++ ps)) ((-1), []) $ updated paragrafosPosterioresDyn <&> \case
                                                                SemListar -> Nothing
                                                                Listando _ -> Nothing
                                                                ListaObtida Nothing -> Nothing
                                                                ListaObtida (Just (pid, ps)) -> Just (pid, fmap snd ps)
                                                dyn_ $ todosParagrafosAnterioresDyn <&> \(_, ps) -> forM_ ps (\t -> el "p" $ text t)
                                                el "p" $ renderValor v
                                                dyn_ $ todosParagrafosPosterioresDyn <&> \(_, ps) -> forM_ ps (\t -> el "p" $ text t)
                                                carregarPosterioresClickEv <- elClass "p" "text-center" $ btnClass "btn btn-link" never $ text "Carregar parágrafos posteriores"
                                                return ()
                                            Nothing -> renderValor v

                            let ultimaPagina = qtdResultados `quot` 20 + 1
                                todasPaginasDyn = pgAtualDyn <&> \pg -> 1 : [max 2 (pg - 4) .. pg] ++ [(pg + 1) .. min (ultimaPagina - 1) (pg + 4)] ++ if pg < ultimaPagina then [ultimaPagina] else []
                                mkPgBtn pg = (pg <$) <$> btnClass "btn btn-link" (updated $ (== pg) <$> pgAtualDyn) (text $ tshow pg)
                            pgClickEv <- elClass "div" "text-center" $ dynEv todasPaginasDyn $ \pgs -> mergeWith const <$> forM pgs mkPgBtn
                            return pgClickEv
                return ()