{-# LANGUAGE OverloadedStrings #-}

module Output.CLI
    ( runCLI
    ) where

import System.Environment (getArgs)

-- | Run the CLI application
-- This is a placeholder for now
runCLI :: IO ()
runCLI = do
    args <- getArgs
    case args of
        [] -> putStrLn "Output - Korean Learning App\nUsage: output [command]"
        ["help"] -> printHelp
        ["version"] -> putStrLn "Output v0.1.0.0"
        _ -> putStrLn "Unknown command. Try 'output help'"

-- | Print help message
printHelp :: IO ()
printHelp = putStr $ unlines
    [ "Output - Korean Learning App"
    , ""
    , "Commands:"
    , "  add-word          Add a new vocabulary word"
    , "  complete-exercise Log an exercise completion"
    , "  view-progress     Show current learning progress"
    , "  next-exercises    Get today's exercises"
    , "  help              Show this help message"
    ]
