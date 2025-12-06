{-# LANGUAGE OverloadedStrings #-}

module Output.CLI
    ( runCLI
    ) where

import System.Environment (getArgs)
import Data.Time (getCurrentTime, utcToLocalTime, utc)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import Output.Domain.Types
import Output.Domain.Progress (emptyUserProgress)
import Output.Domain.Exercise
import Output.Repository.Json (JsonRepository(..), runJsonRepository)
import Output.Repository.Class

-- | Run the CLI application
runCLI :: IO ()
runCLI = do
    args <- getArgs
    case args of
        [] -> printUsage
        ["help"] -> printHelp
        ["version"] -> putStrLn "Output v0.1.0.0"
        ["view-progress"] -> viewProgress
        ["next-exercises"] -> nextExercises
        ["drill"] -> startDrill Nothing
        ["drill", levelStr] -> case parseLevel levelStr of
            Just level -> startDrill (Just level)
            Nothing -> putStrLn $ "Invalid level: " ++ levelStr ++ ". Use 1-6."
        _ -> putStrLn "Unknown command. Try 'output help'"

-- | Parse a TOPIK level from string
parseLevel :: String -> Maybe TOPIK_Level
parseLevel s = case readMaybe s :: Maybe Int of
    Just 1 -> Just One
    Just 2 -> Just Two
    Just 3 -> Just Three
    Just 4 -> Just Four
    Just 5 -> Just Five
    Just 6 -> Just Six
    _ -> Nothing

-- | Print usage
printUsage :: IO ()
printUsage = putStrLn "Output - Korean Learning App\nUsage: output [command]\nTry 'output help' for more information."

-- | Print help message
printHelp :: IO ()
printHelp = putStr $ unlines
    [ "Output - Korean Learning App"
    , ""
    , "Commands:"
    , "  tui               Launch interactive TUI mode"
    , "  view-progress     Show current learning progress"
    , "  next-exercises    Get today's due exercises"
    , "  drill             Start an interactive drill session"
    , "  drill <level>     Drill for specific TOPIK level (1-6)"
    , "  help              Show this help message"
    , "  version           Show version"
    , ""
    , "Run 'output tui' for the full interactive terminal UI experience."
    ]

-- | View current progress
viewProgress :: IO ()
viewProgress = do
    progress <- runJsonRepository getProgress
    putStrLn "\n=== Learning Progress ==="
    putStrLn $ "Current TOPIK Level: " ++ showLevel (upCurrentLevel progress)
    putStrLn $ "Daily Streak: " ++ show (upDailyStreak progress) ++ " days"
    putStrLn $ "Words Learned: " ++ show (upTotalWordsLearned progress)
    putStrLn $ "Words Reviewed: " ++ show (upTotalWordsReviewed progress)
    let states = upVocabularyStates progress
    let masteryStats = countByMastery (Map.elems states)
    putStrLn "\nMastery Breakdown:"
    putStrLn $ "  New: " ++ show (masteryNew masteryStats)
    putStrLn $ "  Learning: " ++ show (masteryLearning masteryStats)
    putStrLn $ "  Intermediate: " ++ show (masteryIntermediate masteryStats)
    putStrLn $ "  Mastered: " ++ show (masteryMastered masteryStats)
    putStrLn ""
  where
    showLevel One = "1"
    showLevel Two = "2"
    showLevel Three = "3"
    showLevel Four = "4"
    showLevel Five = "5"
    showLevel Six = "6"

-- | Mastery stats
data MasteryStats = MasteryStats
    { masteryNew :: Int
    , masteryLearning :: Int
    , masteryIntermediate :: Int
    , masteryMastered :: Int
    }

countByMastery :: [VocabularyState] -> MasteryStats
countByMastery states = MasteryStats
    { masteryNew = length $ filter ((== New) . vstMasteryLevel) states
    , masteryLearning = length $ filter ((== Learning) . vstMasteryLevel) states
    , masteryIntermediate = length $ filter ((== Intermediate) . vstMasteryLevel) states
    , masteryMastered = length $ filter ((== Mastered) . vstMasteryLevel) states
    }

-- | Get next exercises due for review
nextExercises :: IO ()
nextExercises = do
    now <- utcToLocalTime utc <$> getCurrentTime
    dueWords <- runJsonRepository $ getWordsForReview now
    if null dueWords
        then putStrLn "No exercises due! You're all caught up."
        else do
            putStrLn $ "\n" ++ show (length dueWords) ++ " words due for review:"
            mapM_ (\(VocabularyId vid) -> putStrLn $ "  - Word ID: " ++ show vid) dueWords

-- | Start an interactive drill session
startDrill :: Maybe TOPIK_Level -> IO ()
startDrill mLevel = do
    cards <- runJsonRepository $ case mLevel of
        Nothing -> getAllVocabCards
        Just level -> getVocabCardsForLevel level

    if null cards
        then putStrLn "No vocabulary cards found. Add some words first!"
        else do
            putStrLn "\n=== Korean Drill ==="
            putStrLn $ "Loaded " ++ show (length cards) ++ " vocabulary cards"
            putStrLn "Type your answer in Korean. Press Enter to submit."
            putStrLn "Type 'quit' to exit.\n"

            -- Generate exercise prompts from the first 10 cards
            let prompts = map (generateExercisePrompt Writing) (take 10 cards)

            runDrillLoop prompts 0 0

-- | Run the drill loop
runDrillLoop :: [ExercisePrompt] -> Int -> Int -> IO ()
runDrillLoop [] correct total = do
    putStrLn "\n=== Session Complete ==="
    putStrLn $ "Score: " ++ show correct ++ "/" ++ show total
    let pct = if total > 0 then (fromIntegral correct / fromIntegral total * 100 :: Double) else 0
    putStrLn $ "Accuracy: " ++ show (round pct :: Int) ++ "%"

runDrillLoop (prompt:rest) correct total = do
    putStrLn $ "Translate to Korean: " ++ T.unpack (epQuestion prompt)
    putStr "> "
    hFlush stdout
    answer <- TIO.getLine

    if T.strip answer == "quit"
        then runDrillLoop [] correct total  -- End session
        else do
            let isCorrect = checkAnswer prompt answer
            if isCorrect
                then putStrLn "✓ Correct!\n"
                else do
                    putStrLn $ "✗ Incorrect. Answer: " ++ T.unpack (epExpectedAnswer prompt)
                    putStrLn ""
            runDrillLoop rest (if isCorrect then correct + 1 else correct) (total + 1)
