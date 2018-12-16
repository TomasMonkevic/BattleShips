{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module WebService where
    
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Default.Class
import Data.String
import Data.Text.Lazy (Text)
import qualified Data.ByteString.Lazy.Char8 as LS8
import qualified Data.Map.Strict as Map
import Network.Wai.Middleware.RequestLogger
import Prelude
import Web.Scotty.Trans
import Data.Aeson
import Moves
import BattleShips

newtype AppState = AppState { webState :: Map.Map Text (LS8.ByteString, [(String, String)]) }

newtype WebM a = WebM { runWebM :: ReaderT (TVar AppState) IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

gets :: (AppState -> b) -> WebM b
gets f = ask >>= liftIO . readTVarIO >>= return . f

modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

app :: ScottyT Text WebM ()
app = do
    middleware logStdoutDev
    get "/game/:gameId" $ do
        gameId <- param "gameId"
        battlesStates <- webM $ gets webState
        let battleState = Map.lookup gameId battlesStates
        case battleState of
            Nothing -> raw "No moves were made!"
            Just (moves, aliveShips) -> raw moves

    post "/game/:gameId" $ do
        b <- body
        let getMoves = decode b :: Maybe Moves
        if isGameOver getMoves 
            then do
                raw "I won :)"
            else do
                gameId <- param "gameId"
                battlesStates <- webM $ gets webState

                let battleState = Map.lookup gameId battlesStates
                let as = case battleState of
                        Nothing -> damageShip getMoves shipCoords
                        Just (moves, aliveShips) -> damageShip getMoves aliveShips

                response <- liftIO (play getMoves as)
                let updatedBattlesStates = Map.insert gameId (response, as) battlesStates
                webM $ modify $ \ st -> st { webState = updatedBattlesStates }
                raw response
