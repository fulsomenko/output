{-# LANGUAGE OverloadedStrings #-}

module Output.App
    ( AppState
    , initAppState
    , runApp
    ) where

import Output.Repository.Json (JsonRepository)

-- | Application state monad
-- This would be a monad transformer stack in a real implementation
type AppState = JsonRepository

-- | Initialize the application state
initAppState :: IO ()
initAppState = do
    putStrLn "Initializing Output application..."

-- | Run the application
runApp :: IO ()
runApp = do
    putStrLn "Starting Output - Korean Learning App"
    initAppState
    putStrLn "Ready!"
