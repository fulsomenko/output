{-# LANGUAGE OverloadedStrings #-}

module Output.TUI.App
    ( runTUI
    , theAttrMap
    ) where

import Brick
import Brick.BChan (newBChan)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCross
import Data.Time (getCurrentTime, utcToLocalTime, utc)
import qualified Data.Map as Map
import Data.Text (pack)
import System.Environment (lookupEnv)

import Output.TUI.Types
import Output.TUI.Draw (drawUI)
import Output.TUI.Events (handleEvent)
import Output.Repository.Json (runJsonRepository, loadSettings, loadLearnedWords, loadStudentProfile)
import Output.Domain.Settings (AppSettings(..))
import Output.Repository.Class
    ( getAllVocabCards
    , getProgress
    , getTypingProgress
    , getAllVocabStates
    , getAllActivities
    )
import Output.Domain.TypingWord (loadAllTypingWords)
import Output.Domain.Types (VocabularyState(..))
import Output.Algorithm.Streak (calculateStreak)

-- | Attribute map for styling
theAttrMap :: AttrMap
theAttrMap = attrMap V.defAttr
    [ (titleAttr,        fg V.cyan `V.withStyle` V.bold)
    , (menuAttr,         V.defAttr)
    , (menuSelectedAttr, fg V.cyan `V.withStyle` V.bold)
    , (promptAttr,       fg V.yellow)
    , (inputAttr,        V.defAttr)
    , (correctAttr,      fg V.green `V.withStyle` V.bold)
    , (incorrectAttr,    fg V.red)
    , (hintAttr,         fg V.blue)
    , (statsAttr,        fg V.white)
    , (aiAttr,           fg V.magenta `V.withStyle` V.bold)
    ]

-- | Override Ollama connection settings from environment variables.
-- OLLAMA_HOST and OLLAMA_MODEL take priority over settings.json and defaults.
applyEnvOverrides :: AppSettings -> IO AppSettings
applyEnvOverrides s = do
    host  <- lookupEnv "OLLAMA_HOST"
    model <- lookupEnv "OLLAMA_MODEL"
    pure s
        { settingsOllamaHost  = maybe (settingsOllamaHost s)  pack host
        , settingsOllamaModel = maybe (settingsOllamaModel s) pack model
        }

-- | Run the TUI application
runTUI :: IO ()
runTUI = do
    -- Create channel for async events (LLM responses etc.)
    chan <- newBChan 10

    -- Build the Brick app, closing over the channel so event handlers can use it
    let mkApp = App
            { appDraw         = drawUI
            , appChooseCursor = neverShowCursor
            , appHandleEvent  = handleEvent chan
            , appStartEvent   = pure ()
            , appAttrMap      = const theAttrMap
            }

    -- Get current time for due card calculation
    utcNow <- getCurrentTime
    let now = utcToLocalTime utc utcNow

    -- Load all persisted data
    topikCards     <- runJsonRepository getAllVocabCards
    learnedCards   <- loadLearnedWords
    let cards = topikCards ++ learnedCards
    progress       <- runJsonRepository getProgress
    typingWords    <- loadAllTypingWords "data"
    typingProgress <- runJsonRepository getTypingProgress
    vocabStates    <- runJsonRepository getAllVocabStates
    activities     <- runJsonRepository getAllActivities
    settings       <- loadSettings >>= applyEnvOverrides
    studentProfile <- loadStudentProfile

    let dueCards = Map.keys $ Map.filter isDue vocabStates
        isDue state = vstNextReviewDate state <= now
        streak = calculateStreak now activities

    let initialState = initialAppState
            { asVocabCards     = cards
            , asProgress       = Just progress
            , asTypingWords    = typingWords
            , asTypingProgress = typingProgress
            , asVocabStates    = vocabStates
            , asDueCards       = dueCards
            , asDailyStreak    = streak
            , asActivities     = activities
            , asSettings       = settings
            , asStudentProfile = studentProfile
            }

    let buildVty = VCross.mkVty V.defaultConfig
    initialVty <- buildVty
    _finalState <- customMain initialVty buildVty (Just chan) mkApp initialState

    putStrLn "Thanks for practicing! 감사합니다!"
