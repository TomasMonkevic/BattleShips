module Main where

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Moves
import Http

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

play :: Integer -> String -> Maybe Moves -> IO()
play turn player m = do
    if turn == 0
    then do
        moveCoord <- getNextMove $ availableMoves m (if player == "A" then 0 else 1)
        let move = Moves moveCoord (isHit m) m
        postResponse <- httpPost player (encode move)
        print postResponse
        print move
        play 1 player (Just move)
    else do
        getResponse <- (httpGet player)
        print getResponse
        let getMoves = decode getResponse :: Maybe Moves
        play 0 player getMoves

main :: IO ()
main = do
    line <- getLine
    if line == "A"
        then
            play 0 "A" Nothing
        else
            play 1 "B" Nothing
