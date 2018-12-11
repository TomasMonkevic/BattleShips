module BattleShips where

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Moves
import Http
import Data.List as L
import System.Random
import qualified Data.ByteString.Lazy.Char8 as LS8

gameUrl :: String
gameUrl = "http://battleship.haskell.lt/game/"

gameId :: String
gameId = "tm_justas_12"
-- gameId = "tm_test41"

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

board :: [Maybe (String, String)]
board = do
    x <- ['A'..'J']
    y <- [1..10]
    return (Just ([x], show y))

availableMoves :: Maybe Moves -> Integer -> [Maybe (String, String)]
availableMoves moves player = 
    let 
        playerMoves = if player == 0 then fst $ allMoves moves else snd $ allMoves moves
    in
        L.filter (f playerMoves) board
        where
            f :: [Maybe (String, String)] -> Maybe (String, String) -> Bool
            f pm c = L.notElem c pm

getNextMove :: [Maybe (String, String)] -> IO (Maybe (String, String))
getNextMove am = do
    g <- newStdGen
    return $ am !! (fst (randomR (0, (L.length am) - 1) g))

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

play :: Maybe Moves -> [(String, String)] -> LS8.ByteString
play m aliveShips =
    case (availableMoves m 1) of 
        [] -> "It's a draw :|"
        am2 -> case aliveShips of
                [] -> encode (Moves Nothing (isHit m) m)
                --how the fuck do I shoot to random position if that is an IO function?
                as -> encode (Moves (Just ("A", "1")) (isHit m) m)