module Moves where

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Vector
import qualified Data.ByteString.Char8 as S8
import Data.Text as T
import qualified Data.CaseInsensitive as CI
import Control.Monad
import Utils
import Data.List as L
import Data.Aeson.Types

data ShotType = MISS | HIT
    deriving (Show, Eq)

instance FromJSON ShotType where
    parseJSON (String "HIT") = return HIT
    parseJSON (String "MISS") = return MISS
    parseJSON _ = mzero

instance ToJSON ShotType where
    toJSON MISS = String "MISS"
    toJSON HIT = String "HIT"

data Moves = Moves { 
    coords :: Maybe (String, String), 
    result :: Maybe ShotType, 
    prev :: Maybe Moves 
}
    deriving (Show, Eq)

instance FromJSON Moves where
    parseJSON (Array v) = f (toList v) 
        where
            f :: [Value] -> Parser Moves
            f (cs:c:rs:r:ps:p:xs) = do
                _c <- parseJSON c
                _r <- parseJSON r
                _p <- parseJSON p
                return $ Moves (arrayToTuple _c) _r _p
            f [] = mzero
    parseJSON _ = mzero

instance ToJSON Moves where
    toJSON (Moves coords result prev) = 
        Array (fromList [ String "coord",
            toJSON coords,
            String "result",
            toJSON result,
            String "prev",
            toJSON prev])

arrayToTuple :: [a] -> Maybe (a,a)
arrayToTuple [] = Nothing
arrayToTuple (a1:a2:t) = Just (a1,a2)

allMoves :: Maybe Moves -> ([Maybe (String, String)], [Maybe (String, String)])
allMoves moves = f moves (moveCount moves) ([],[])
    where 
        f :: Maybe Moves -> Integer -> ([Maybe (String, String)], [Maybe (String, String)]) -> ([Maybe (String, String)], [Maybe (String, String)])
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

score :: Maybe Moves -> (Integer, Integer)
score moves = f moves (moveCount moves) (0,0)
    where 
        f :: Maybe Moves -> Integer -> (Integer, Integer) -> (Integer, Integer)
        f Nothing _ acc = acc
        f (Just Moves { result = r, prev = p}) player (playerOneScore, playerTwoScore) = 
            if r == Just HIT then
                if player `mod` 2 == 0 then --when ship was hit, second players seys HIT, the score is increase for the first player and vice versa
                    f p (player + 1) (playerOneScore + 1, playerTwoScore)
                else
                    f p (player + 1) (playerOneScore, playerTwoScore + 1)
            else
                f p (player + 1) (playerOneScore, playerTwoScore)