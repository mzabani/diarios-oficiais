{-# LANGUAGE NoImplicitPrelude #-}
module Zlude (safeGroupBy, distinctBy, head, fst, snd, thd, (==), (>), (<), (>=), (<=), NonEmpty, Zlude.concat, Zlude.concatMap, (<>)) where

import Data.List.NonEmpty
import Data.Eq
import Data.Function
import Data.Ord
import Data.Foldable
import Data.Functor
import Data.Tuple
import Data.Monoid
import qualified Data.List as List

safeGroupBy :: Ord a => (b -> a) -> [b] -> [(a, NonEmpty b)]
safeGroupBy f l = fmap (\(first : others) -> (snd first, fst first :| fmap fst others)) $ List.groupBy (\a b -> snd a == snd b) $ List.sortBy (compare `on` snd) $ fmap (\x -> (x, f x)) l

distinctBy :: Ord a => (b -> a) -> [b] -> [b]
distinctBy f = fmap (head . snd) . safeGroupBy f

thd :: (a,b,c) -> c
thd (_, _, x) = x

concat :: (Foldable t, Foldable u) => t (u a) -> [a]
concat = foldr (\l ret -> foldr (:) [] l List.++ ret) []

concatMap :: (Foldable t, Foldable u) => (a -> t b) -> u a -> [b]
concatMap f = Zlude.concat . foldr (\el ret -> f el : ret) []