module BuscaSpec (spec) where

import RIO
import qualified RIO.List as List
import qualified RIO.Text as Text
import qualified Database.PostgreSQL.Simple as DB
import Busca (parseConsulta, Consulta(..), queryGrupos)
import Common (FormBusca(..), ResultadoBusca(..))
import TestDb (aroundWithConn)
import Test.QuickCheck
import Test.Hspec


spec :: Spec
spec = do
    aroundWithConn $ do
        describe "Testando o Parser de Busca" $ do
            it "expressões válidas" $ \conn -> property $ expressaoValidaParseada conn

arbSeparadoPorEspacos :: [Text] -> Gen Text
arbSeparadoPorEspacos xs = Text.intercalate " " <$> listOf (elements xs)

newtype Palavra = Palavra { unPalavra :: Text }
instance Arbitrary Palavra where
    arbitrary = Palavra <$> elements ["issqn", "Alvará", "Covid-19", "multa"]

newtype ClausulaDatas = ClausulaDatas { unClausulaDatas :: Text }
instance Arbitrary ClausulaDatas where
    arbitrary =
        ClausulaDatas <$>
            elements [
                ""
                , "data: 2020-01-03"
                , "data=2020-1-03"
                , "data >= 2020-01-3"
                , "data <2018-11-27"
                ]

newtype ClausulaGrupos = ClausulaGrupos { unClausulaGrupos :: Text }
instance Arbitrary ClausulaGrupos where
    arbitrary =
        ClausulaGrupos <$>
            oneof [
                pure ""
                , fmap ("grupos: " <>) $ arbSeparadoPorEspacos ["data", "diario"]
                , fmap ("grupos : " <>) $ arbSeparadoPorEspacos ["data", "diario"]
                , fmap ("grupos :" <>) $ arbSeparadoPorEspacos ["data", "diario"]
                ]

newtype ConsultaValida = ConsultaValida { unConsulta :: Text } deriving Show
instance Arbitrary ConsultaValida where
    arbitrary = do
        palavrasBuscadas <- listOf1 $ arbitrary @Palavra
        filtroDatas <- listOf $ arbitrary @ClausulaDatas
        grupos <- arbitrary @ClausulaGrupos
        return $ ConsultaValida $ 
            separarPorEspacos unPalavra palavrasBuscadas
            <> " " <> separarPorEspacos unClausulaDatas filtroDatas
            <> " " <> unClausulaGrupos grupos


separarPorEspacos :: (a -> Text) -> [a] -> Text
separarPorEspacos f l = Text.intercalate " " $ map f l

expressaoValidaParseada :: DB.Connection -> ConsultaValida -> IO ()
expressaoValidaParseada conn (unConsulta -> textoConsulta) = do
    let fb = FormBusca { buscaTermo = textoConsulta, buscaPagina = 1 }
    parseConsulta fb `shouldSatisfy` isRight
    resultadosBusca <- queryGrupos fb conn
    resultadosBusca `shouldSatisfy`
        \case
            Resultados _ _ -> True
            _              -> False