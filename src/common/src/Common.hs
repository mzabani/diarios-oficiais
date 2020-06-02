module Common where

import GHC.Generics
import Data.Aeson
import Data.Time
import Data.Text (Text)

data FormBusca = FormBusca {
    buscaTermo :: Text
    , buscaPagina :: Int
} deriving (Generic, ToJSON, FromJSON)

-- TODO: Indexar comprimento de colunas e valores para que sejam iguais!
data ResultadoBusca = ErroBusca Text | Resultados Int Resultado deriving (Generic, ToJSON, FromJSON, Show)
data Resultado = Resultado {
    colunas :: [Text],
    resultados :: [(Maybe Int, [Valor])]
} deriving (Generic, ToJSON, FromJSON, Show)

-- | ValorMatch tem booleanos que indicam se palavra é um match de busca (e portanto deve ficar em negrito)
data Valor = ValorTexto Text | ValorMatch [(Text, Bool)] | ValorLista [Valor] deriving (Generic, ToJSON, FromJSON, Show)