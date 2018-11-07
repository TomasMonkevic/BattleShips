module Moves where

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Vector
import qualified Data.ByteString.Char8 as S8
import Data.Text as T
import qualified Data.CaseInsensitive as CI
import Utils

data ShotType = MISS | HIT
    deriving (Show, Eq)

--instance FromJSON ShotType
instance ToJSON ShotType where
    toJSON MISS = String (T.pack "MISS")
    toJSON HIT = String (T.pack "HIT")

data Moves = Moves { 
    coords :: (String, String), 
    result :: Maybe ShotType, 
    prev :: Maybe Moves 
}
    deriving Show

-- instance FromJSON Moves
instance ToJSON Moves where
    toJSON (Moves coords result prev) = 
        Array (fromList [ String $ T.pack "coord",
            toJSON coords,
            String $ T.pack "result",
            toJSON result,
            String $ T.pack "prev",
            toJSON prev])

allMoves :: Maybe Moves -> ([(String, String)], [(String, String)])
allMoves moves = f moves (moveCount moves) ([],[])
    where 
        f :: Maybe Moves -> Integer -> ([(String, String)], [(String, String)]) -> ([(String, String)], [(String, String)])
        f Nothing _ acc = acc
        f (Just Moves { coords = c, prev = p}) player (playerOneMoves, playerTwoScore) = 
            if player `mod` 2 == 0 then
                    f p (player + 1) (playerOneMoves, c : playerTwoScore)
                else
                    f p (player + 1) (c : playerOneMoves, playerTwoScore)

moveCount :: Maybe Moves -> Integer
moveCount moves = f moves 0
        where 
            f :: Maybe Moves -> Integer -> Integer
            f Nothing acc = acc
            f (Just Moves { prev = p})  acc = f p (acc + 1)
                    

areMovesValid :: Maybe Moves -> Bool
areMovesValid moves = 
    let
        m = allMoves moves
    in
        (allUnique (fst m)) && (allUnique (snd m))