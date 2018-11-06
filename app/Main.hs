module Main where

{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
-- import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Char8 as S8
import Data.Text (Text)
import qualified Data.CaseInsensitive as CI

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings

    -- Create the request
    -- let requestObject = object ["name" .= "Michael", "age" .= 30]
    -- let requestObject = object [ "name" .= ("Michael" :: Text) , "age"  .= (30 :: Int) ]

    initialRequest <- parseRequest "http://battleship.haskell.lt/game/tm_test3/player/A"
    let request = initialRequest { 
        method = ("POST"), 
        requestBody = RequestBodyBS ("[\"coord\", [\"B\",\"6\"], \"result\", null,\"prev\", null]"),
        requestHeaders = [("Content-Type", "application/json+nomaps")]
    }
    response <- httpLbs request manager
    -- putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
    print $ responseBody response
