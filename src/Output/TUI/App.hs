{-# LANGUAGE OverloadedStrings #-}

module Output.TUI.App
    ( runTUI
    , app
    , theAttrMap
    ) where

import Brick
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCross
import Data.Time (getCurrentTime, utcToLocalTime, utc)
import qualified Data.Map as Map

import Output.TUI.Types
import Output.TUI.Draw (drawUI)
import Output.TUI.Events (handleEvent)
import Output.Repository.Json (runJsonRepository)
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

-- | Brick application definition
app :: App AppState AppEvent Name
app = App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure ()
    , appAttrMap = const theAttrMap
    }

-- | Attribute map for styling
theAttrMap :: AttrMap
theAttrMap = attrMap V.defAttr
    [ (titleAttr, fg V.cyan `V.withStyle` V.bold)
    , (menuAttr, V.defAttr)
    , (menuSelectedAttr, fg V.cyan `V.withStyle` V.bold)
    , (promptAttr, fg V.yellow)
    , (inputAttr, V.defAttr)
    , (correctAttr, fg V.green `V.withStyle` V.bold)
    , (incorrectAttr, fg V.red)
    , (hintAttr, fg V.blue)
    , (statsAttr, fg V.white)
    ]

-- | Run the TUI application
runTUI :: IO ()
runTUI = do
    -- Get current time for due card calculation
    utcNow <- getCurrentTime
    let now = utcToLocalTime utc utcNow

    -- Load vocabulary and progress
    cards <- runJsonRepository getAllVocabCards
    progress <- runJsonRepository getProgress

    -- Load typing vocabulary and progress
    typingWords <- loadAllTypingWords "data"
    typingProgress <- runJsonRepository getTypingProgress

    -- Load SRS data
    vocabStates <- runJsonRepository getAllVocabStates
    activities <- runJsonRepository getAllActivities

    -- Calculate due cards (cards with nextReviewDate <= now)
    let dueCards = Map.keys $ Map.filter isDue vocabStates
        isDue state = vstNextReviewDate state <= now

    -- Calculate streak from activity history
    let streak = calculateStreak now activities

    let initialState = initialAppState
            { asVocabCards = cards
            , asProgress = Just progress
            , asTypingWords = typingWords
            , asTypingProgress = typingProgress
            , asVocabStates = vocabStates
            , asDueCards = dueCards
            , asDailyStreak = streak
            , asActivities = activities
            }

    -- Build vty and run
    let buildVty = VCross.mkVty V.defaultConfig
    initialVty <- buildVty
    _finalState <- customMain initialVty buildVty Nothing app initialState

    -- Show goodbye message
    putStrLn "Thanks for practicing! 감사합니다!"
