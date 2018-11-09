module Main where

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Moves
import Http

main :: IO ()
main = do
    move <- getNextMove $ availableMoves Nothing 0
    let firstMove = Moves move Nothing Nothing
    postResponse <- httpPost "A" (encode firstMove)
    print postResponse
    getResponse <- (httpGet "B")
    print getResponse

    let getMoves = decode getResponse :: Maybe Moves
    move2 <- getNextMove $ availableMoves getMoves 1
    let opa = Moves move2 (Just MISS) getMoves
    postResponse2 <- httpPost "B" (encode opa)
    print postResponse2
    getResponse2 <- (httpGet "A")
    print getResponse2
