module Http where

import Network.HTTP.Client
import qualified Data.ByteString.Lazy.Char8 as LS8
import Data.Text (Text)
import qualified Data.CaseInsensitive as CI

gameUrl :: String
gameUrl = "http://battleship.haskell.lt/game/"

gameId :: String
gameId = "tm_test21"

httpPost :: [Char] -> LS8.ByteString -> IO LS8.ByteString
httpPost player json = do
    manager <- newManager defaultManagerSettings

    initialRequest <- parseRequest (gameUrl ++ gameId ++ "/player/" ++ player)
    let request = initialRequest { 
        method = ("POST"), 
        requestBody = RequestBodyLBS json,
        requestHeaders = [("Content-Type", "application/json+nomaps")]
    }
    response <- httpLbs request manager
    return $ responseBody response

httpGet :: [Char] -> IO LS8.ByteString
httpGet player = do
    manager <- newManager defaultManagerSettings

    initialRequest <- parseRequest (gameUrl ++ gameId ++ "/player/" ++ player)
    let request = initialRequest { 
        method = ("GET"), 
        requestHeaders = [("Accept", "application/json+nomaps")]
    }
    response <- httpLbs request manager
    return $ responseBody response