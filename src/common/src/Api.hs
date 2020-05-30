module Api (SinglePageAPI) where

import Servant.API
import Data.Text (Text)
import Common

type SinglePageAPI = "busca" :> QueryParam' '[Required] "q" Text :> QueryParam' '[Required] "p" Int :> Get '[JSON] Common.ResultadoBusca
                :<|> "listar-paragrafos-anteriores" :> Capture "paragrafoId" Int :> Get '[JSON] (Int, [(Int, Text)])
                :<|> "listar-paragrafos-posteriores" :> Capture "paragrafoId" Int :> Get '[JSON] (Int, [(Int, Text)])
                :<|> Raw