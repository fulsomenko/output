module Main where

import System.Environment (getArgs)
import Output.CLI (runCLI)
import Output.TUI.App (runTUI)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["tui"] -> runTUI
        ["--tui"] -> runTUI
        ["-t"] -> runTUI
        _ -> runCLI
