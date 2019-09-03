module ConcursosPublicos.Xml where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Map.Lazy as Map
import Text.XML hiding (parseLBS)

nodeText :: Node -> T.Text
nodeText (NodeContent t) = t
nodeText (NodeElement e) = T.concat $ fmap nodeText (elementNodes e)
nodeText _               = ""

attr :: Name -> Node -> T.Text
attr name (NodeElement el) = fromMaybe "" $ Map.lookup name (elementAttributes el)
attr _     _               = ""