module Http where

import Network.HTTP.Client
import qualified Data.ByteString.Lazy.Char8 as LS8
import Data.Text (Text)
import qualified Data.CaseInsensitive as CI

httpPost :: [Char] -> LS8.ByteString -> String -> String -> IO LS8.ByteString
httpPost player json gUrl gId = do
    manager <- newManager defaultManagerSettings

    initialRequest <- parseRequest (gUrl ++ gId ++ "/player/" ++ player)
    let request = initialRequest { 
        method = ("POST"), 
        requestBody = RequestBodyLBS json,
        requestHeaders = [("Content-Type", "application/json+nomaps")]
    }
    response <- httpLbs request manager
    return $ responseBody response

httpGet :: [Char] -> String -> String -> IO LS8.ByteString
httpGet player gUrl gId = do
    manager <- newManager defaultManagerSettings

    initialRequest <- parseRequest (gUrl ++ gId ++ "/player/" ++ player)
    let request = initialRequest { 
        method = ("GET"), 
        requestHeaders = [("Accept", "application/json+nomaps")]
    }
    response <- httpLbs request manager
    return $ responseBody response