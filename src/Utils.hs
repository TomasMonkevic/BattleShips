module Utils where

import Data.List (group, sort, sortBy, (\\))

sg :: Ord a => [a] -> [[a]]
sg = group . sort

allUnique :: Ord a => [a] -> Bool
allUnique = all ( (==) 1 . length) . sg

arrayToTuple :: [a] -> Maybe (a,a)
arrayToTuple [] = Nothing
arrayToTuple (a1:a2:t) = Just (a1,a2)

tupleToArray :: Maybe (a,a) -> [a]
tupleToArray Nothing = []
tupleToArray (Just v) = [fst v, snd v]
 