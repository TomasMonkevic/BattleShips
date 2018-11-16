module Main where

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Moves
import Http
import BattleShips

main :: IO ()
main = do
    line <- getLine
    if line == "A"
        then
            play 0 "A" Nothing
        else
            play 1 "B" Nothing
