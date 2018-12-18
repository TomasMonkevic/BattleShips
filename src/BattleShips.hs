module BattleShips where

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Moves
import Data.List as L
import System.Random
import qualified Data.ByteString.Lazy.Char8 as LS8

shipCoords :: [(String, String)]
shipCoords = [
    ("A", "1"), ----
    ("A", "2"), ----
    ("B", "1"),
    ("B", "2"),

    ("A", "10"), --------
    ("B", "10"),
    ("C", "10"),
    ("D", "10"),

    ("D", "1"), ----
    ("E", "1"),   ----
    ("E", "2"),
    ("F", "2"),

    ("H", "1"), -------
    ("I", "1"),      --
    ("J", "1"),
    ("J", "2"),

    ("H", "10"),   --
    ("I", "10"), ------
    ("I", "9"),
    ("J", "10")]

wtfBoard :: [(Char, Integer)]
wtfBoard = (,) <$> ['A'..'J'] <*> [1..10]

board :: [Maybe (String, String)]
board = map f wtfBoard 
    where
        f :: (Char, Integer) -> Maybe (String, String)
        f (c, i) = Just ([c], show i)

availableMoves :: Maybe Moves -> Integer -> [Maybe (String, String)]
availableMoves moves player = 
    let 
        playerMoves = if player == 0 then fst $ allMoves moves else snd $ allMoves moves
    in
        L.filter (f playerMoves) board
        where
            f :: [Maybe (String, String)] -> Maybe (String, String) -> Bool
            f pm c = L.notElem c pm

getRandomInt :: IO Int
getRandomInt = do
    g <- newStdGen
    return (fst (randomR (0, (L.length board) - 1) g))

isHit :: Maybe Moves -> Maybe ShotType
isHit Nothing = Nothing
isHit (Just Moves {coords = c}) = case c of
    Nothing -> Nothing
    Just (c1) -> if elem c1 shipCoords then Just HIT else Just MISS

isGameOver :: Maybe Moves -> Bool
isGameOver Nothing = False
isGameOver (Just Moves { coords = c}) = case c of
    Nothing -> True
    Just (c1) -> False

damageShip :: Maybe Moves -> [(String, String)] -> [(String, String)]
damageShip Nothing as = as
damageShip (Just Moves { coords = c}) as = case c of
    Nothing -> as
    Just (c1) -> delete c1 as

getCoords :: Maybe Moves -> Maybe (String, String)
getCoords Nothing = Nothing
getCoords (Just Moves { coords = c}) = c

play :: Int -> Maybe Moves -> [(String, String)] -> LS8.ByteString
play randomInt m aliveShips =
    case (availableMoves m 1) of 
        [] -> "It's a draw :|"
        am2 -> case aliveShips of
                [] -> encode (Moves Nothing (isHit m) m)
                as -> encode (Moves (am2 !! (randomInt `mod` L.length am2)) (isHit m) m)