module Main where

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Moves
import Http

main :: IO ()
main = do
    let testMoves = Moves ("F","4") (Just HIT)  (Just $ Moves ("F", "1") Nothing Nothing)
    response <- httpPost (encode testMoves)
    print response
