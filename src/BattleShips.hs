module BattleShips where

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Moves
import Http
import Data.List as L
import System.Random

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

play :: Integer -> String -> Maybe Moves -> [(String, String)] -> IO()
play turn player m aliveShips = do
    if turn == 0
    then do
        let am = availableMoves m (if player == "A" then 0 else 1)
        case am of 
            [] -> do 
                return ()
            am2 -> do
                case aliveShips of
                    [] -> do
                        let move = Moves Nothing (isHit m) m
                        httpPost player (encode move)
                        putStrLn "I lost :("
                    as -> do
                        moveCoord <- getNextMove am2
                        let move = Moves moveCoord (isHit m) m
                        postResponse <- httpPost player (encode move)
                        print postResponse
                        print move
                        play 1 player (Just move) (damageShip m as)
    else do
        getResponse <- (httpGet player)
        if getResponse == "No move available at the moment"
        then do
            return ()
        else do
            print getResponse
            let getMoves = decode getResponse :: Maybe Moves
            if isGameOver getMoves 
            then
                putStrLn "I won :)"
            else 
                play 0 player getMoves aliveShips