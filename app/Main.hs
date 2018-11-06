module Main where

import Network.HTTP.Client
-- import Network.HTTP.Types.Status (statusCode)
import Data.Aeson (object, (.=), encode)
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
        method = (S8.pack "POST"), 
        requestBody = RequestBodyBS (S8.pack "{\"coord\":[\"B\",\"5\"],\"result\":null,\"prev\":null}"),
        requestHeaders = [(CI.mk (S8.pack "Content-Type"), S8.pack "application/json+nomaps")]
    }
    response <- httpLbs request manager
    -- putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
    print $ responseBody response
