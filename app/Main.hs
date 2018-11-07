module Main where

{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Data.Aeson
-- import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Char8 as S8
import Data.Text (Text)
import qualified Data.CaseInsensitive as CI
import Moves

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings

    let testMoves = Moves ("D","3") (Just HIT)  (Just $ Moves ("A", "6") Nothing Nothing)

    initialRequest <- parseRequest "http://battleship.haskell.lt/game/tm_test3/player/A"
    let request = initialRequest { 
        method = ("POST"), 
        requestBody = RequestBodyLBS ( encode testMoves),
        requestHeaders = [("Content-Type", "application/json+nomaps")]
    }
    response <- httpLbs request manager
    -- putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
    print $ responseBody response
