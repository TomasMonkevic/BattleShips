{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
-- An example of embedding a custom monad into
-- Scotty's transformer stack, using ReaderT to provide access
-- to a TVar containing global state.
--
-- Note: this example is somewhat simple, as our top level
-- is IO itself. The types of 'scottyT' and 'scottyAppT' are
-- general enough to allow a Scotty application to be
-- embedded into any MonadIO monad.
module Main (main) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Reader

import Network.Wai.Middleware.RequestLogger

import Data.Default.Class
import Data.String
import Data.Text.Lazy (Text)

import Prelude

import Web.Scotty.Trans

import Data.Aeson
import Moves
import BattleShips

import qualified Data.ByteString.Lazy.Char8 as LS8
import qualified Data.Map.Strict as Map

newtype AppState = AppState { tickCount :: Map.Map Text (LS8.ByteString, [(String, String)]) }

instance Default AppState where
    def = AppState Map.empty

-- Why 'ReaderT (TVar AppState)' rather than 'StateT AppState'?
-- With a state transformer, 'runActionToIO' (below) would have
-- to provide the state to _every action_, and save the resulting
-- state, using an MVar. This means actions would be blocking,
-- effectively meaning only one request could be serviced at a time.
-- The 'ReaderT' solution means only actions that actually modify
-- the state need to block/retry.
--
-- Also note: your monad must be an instance of 'MonadIO' for
-- Scotty to use it.
newtype WebM a = WebM { runWebM :: ReaderT (TVar AppState) IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

-- Scotty's monads are layered on top of our custom monad.
-- We define this synonym for lift in order to be explicit
-- about when we are operating at the 'WebM' layer.
webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

-- Some helpers to make this feel more like a state monad.
gets :: (AppState -> b) -> WebM b
gets f = ask >>= liftIO . readTVarIO >>= return . f

modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

main :: IO ()
main = do
    sync <- newTVarIO def
        -- 'runActionToIO' is called once per action.
    let runActionToIO m = runReaderT (runWebM m) sync

    scottyT 3000 runActionToIO app

app :: ScottyT Text WebM ()
app = do
    middleware logStdoutDev
    get "/game/:gameId" $ do
        gameId <- param "gameId"
        battlesStates <- webM $ gets tickCount
        let battleState = Map.lookup gameId battlesStates
        case battleState of
            Nothing -> raw "No moves were made!"
            Just (moves, aliveShips) -> raw moves

    post "/game/:gameId" $ do
        b <- body
        let getMoves = decode b :: Maybe Moves
        if isGameOver getMoves 
            then do
                raw "Get fucked! I won :)"
            else do
                gameId <- param "gameId"
                battlesStates <- webM $ gets tickCount

                let battleState = Map.lookup gameId battlesStates
                let as = case battleState of
                        Nothing -> damageShip getMoves shipCoords
                        Just (moves, aliveShips) -> damageShip getMoves aliveShips

                response <- liftIO (play getMoves as)
                let b1 = Map.delete gameId battlesStates
                let b2 = Map.insert gameId (response, as) battlesStates
                webM $ modify $ \ st -> st { tickCount = b2 }
                raw response
