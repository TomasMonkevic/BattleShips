module Http where

import Network.HTTP.Client
import qualified Data.ByteString.Lazy.Char8 as LS8
import Data.Text (Text)
import qualified Data.CaseInsensitive as CI

httpPost :: LS8.ByteString -> IO LS8.ByteString
httpPost json = do
    manager <- newManager defaultManagerSettings

    initialRequest <- parseRequest "http://battleship.haskell.lt/game/tm_test3/player/A"
    let request = initialRequest { 
        method = ("POST"), 
        requestBody = RequestBodyLBS json,
        requestHeaders = [("Content-Type", "application/json+nomaps")]
    }
    response <- httpLbs request manager
    return $ responseBody response

httpGet :: IO LS8.ByteString
httpGet = do
    manager <- newManager defaultManagerSettings

    initialRequest <- parseRequest "http://battleship.haskell.lt/game/tm_test3/player/B"
    let request = initialRequest { 
        method = ("GET"), 
        requestHeaders = [("Accept", "application/json+nomaps")]
    }
    response <- httpLbs request manager
    return $ responseBody response