module Main where

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Moves
import Http
import BattleShips

main :: IO ()
main = do
    putStrLn "Enter player type (A or B): "
    line <- getLine
    if line == "A"
        then
            play 0 "A" Nothing shipCoords
        else
            play 1 "B" Nothing shipCoords
