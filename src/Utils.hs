module Utils where

    import Data.List (group, sort, sortBy, (\\))

    sg :: Ord a => [a] -> [[a]]
    sg = group . sort

    allUnique :: Ord a => [a] -> Bool
    allUnique = all ( (==) 1 . length) . sg
 