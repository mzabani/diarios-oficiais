module Import (
  module Exports
  , digitToIntMay
  ) where

import Data.Int as Exports
import Data.Tuple as Exports
import Data.Bool as Exports
import Data.Char as Exports hiding (digitToInt)
import Data.Maybe as Exports hiding (fromJust)
import Data.Either as Exports
import Data.Foldable as Exports
import Data.Traversable as Exports
import Control.Monad as Exports
import Control.Applicative as Exports
import Data.Eq as Exports
import Data.Ord as Exports
import Data.Function as Exports
import Data.List as Exports
import Prelude as Exports (Num, Integral, Integer, mod, undefined, Show, (+), (-), (*), (/))

digitToIntMay :: Char -> Maybe Int
digitToIntMay '0' = Just 0
digitToIntMay '1' = Just 1
digitToIntMay '2' = Just 2
digitToIntMay '3' = Just 3
digitToIntMay '4' = Just 4
digitToIntMay '5' = Just 5
digitToIntMay '6' = Just 6
digitToIntMay '7' = Just 7
digitToIntMay '8' = Just 8
digitToIntMay '9' = Just 9
digitToIntMay _   = Nothing
