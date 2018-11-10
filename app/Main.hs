module Main where

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Moves
import Http

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
