module Main where

-- {-# LANGUAGE OverloadedStrings #-}

-- import Data.Aeson
-- import Moves
-- import Http
-- import BattleShips

-- main :: IO ()
-- main = do
--     putStrLn "Enter player type (A or B): "
--     line <- getLine
--     if line == "A"
--         then
--             play 0 "A" Nothing shipCoords
--         else
--             play 1 "B" Nothing shipCoords

{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Text.Lazy.Encoding
import Control.Monad.IO.Class

import Data.Aeson
import Moves
import BattleShips

import Data.Monoid (mconcat)

attack :: ActionM ()
attack = do
  raw "Attack!"

defend :: ActionM ()
defend = do
  --gameId <- param "gameId"
  b <- body
  let getMoves = decode b :: Maybe Moves
  if isGameOver getMoves 
    then do
        raw "Get fucked! I won :)"
    else do
        -- don't forget to damage ship  
        -- raw (play 0 player getMoves (damageShip getMoves aliveShips))
        response <- liftIO (play getMoves shipCoords)
        raw response

main = scotty 3000 $ do
  get "/game/:gameId" attack
  post "/game/:gameId" defend

