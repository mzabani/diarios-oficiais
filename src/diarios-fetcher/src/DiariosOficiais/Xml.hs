module DiariosOficiais.Xml where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Map.Lazy as Map
import Data.String.Conv (toS)
import Text.XML hiding (parseLBS)
import Text.Blaze (toMarkup)
import Text.Blaze.Html.Renderer.Text (renderHtml)

nodeText :: Node -> T.Text
nodeText (NodeContent t) = t
nodeText (NodeElement e) = T.concat $ fmap nodeText (elementNodes e)
nodeText _               = ""

nodeHtml :: Node -> T.Text
nodeHtml (NodeContent t) = t
nodeHtml (NodeElement e) = toS $ renderHtml $ toMarkup e
nodeHtml _ = ""

attr :: Name -> Node -> T.Text
attr name (NodeElement el) = fromMaybe "" $ Map.lookup name (elementAttributes el)
attr _     _               = ""