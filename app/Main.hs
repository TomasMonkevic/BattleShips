{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Main (main) where

import WebService
import Web.Scotty.Trans
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad.Reader
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
    sync <- newTVarIO (AppState Map.empty)
    let runActionToIO m = runReaderT (runWebM m) sync
    scottyT 3000 runActionToIO app
