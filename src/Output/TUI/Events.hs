{-# LANGUAGE OverloadedStrings #-}

module Output.TUI.Events
    ( handleEvent
    ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (digitToInt)
import Brick
import qualified Graphics.Vty as V
import qualified Data.Text as T
import Data.Time (getCurrentTime, utcToLocalTime, utc)
import qualified Data.Map as Map

import Output.TUI.Types
import Output.Domain.Exercise (ExercisePrompt(..), checkAnswer, generateExercisePrompt)
import Output.Domain.Types
    ( ExerciseType(..)
    , TypingProgress(..)
    , VocabularyId
    , VocabularyCard(..)
    , VocabularyState(..)
    , newVocabularyState
    )
import Output.Domain.Jamo (qwertyToJamo)
import Output.Domain.TypingLevel (TypingLevel(..), mainLevels, getSubLevels, hasSubLevels)
import Output.Domain.TypingWord (TypingWord(..))
import Output.Domain.TypingExercise (TypingExerciseType(..), TypingPrompt(..), createSessionPrompts, validateTyping, CharStatus(..))
import Output.Domain.Activity (ActivityEntry(..), Performance(..), Percentage(..))
import Output.Repository.Json (runJsonRepository)
import Output.Repository.Class (markLevelCompleted, saveVocabState, logActivity)
import Output.Algorithm.SRS (Quality(..), SRSAlgorithm(..), ratingToQuality)
import Output.Algorithm.SpacedRepetition (defaultSM2, applySRSResult)
import qualified Data.Set as Set

-- | Main event handler
handleEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent ev = do
    s <- get
    case asScreen s of
        MainMenuScreen -> handleMenuEvent ev
        DrillScreen -> handleDrillEvent ev
        TypingPracticeScreen -> handleTypingEvent ev
        TypingLevelSelectScreen -> handleLevelSelectEvent ev
        ProgressScreen -> handleProgressEvent ev
        HelpScreen -> handleHelpEvent ev
        QuitConfirmScreen -> handleQuitEvent ev

-- | Handle menu events
handleMenuEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleMenuEvent (VtyEvent (V.EvKey V.KEsc [])) =
    modify $ \s -> s { asScreen = QuitConfirmScreen }
handleMenuEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) =
    modify $ \s -> s { asScreen = QuitConfirmScreen }
handleMenuEvent (VtyEvent (V.EvKey V.KUp [])) =
    modify $ \s -> s { asMenuIndex = max 0 (asMenuIndex s - 1) }
handleMenuEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) =
    modify $ \s -> s { asMenuIndex = max 0 (asMenuIndex s - 1) }
handleMenuEvent (VtyEvent (V.EvKey V.KDown [])) =
    modify $ \s -> s { asMenuIndex = min 6 (asMenuIndex s + 1) }
handleMenuEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) =
    modify $ \s -> s { asMenuIndex = min 6 (asMenuIndex s + 1) }
handleMenuEvent (VtyEvent (V.EvKey V.KEnter [])) = do
    s <- get
    case asMenuIndex s of
        0 -> startTypingPractice     -- Learn keyboard with levels (start here!)
        1 -> startDrill TypingMode   -- Typing drill
        2 -> startDrill ReadingMode  -- Reading drill
        3 -> startDrill WritingMode  -- Writing drill (most advanced)
        4 -> modify $ \st -> st { asScreen = ProgressScreen }
        5 -> modify $ \st -> st { asScreen = HelpScreen }
        6 -> modify $ \st -> st { asScreen = QuitConfirmScreen }
        _ -> pure ()
handleMenuEvent (VtyEvent (V.EvKey (V.KChar '?') [])) =
    modify $ \s -> s { asScreen = HelpScreen }
handleMenuEvent _ = pure ()

-- | Start a drill session
-- Prioritizes due cards (from SRS), then new cards, max 10 total
startDrill :: DrillMode -> EventM Name AppState ()
startDrill mode = do
    s <- get
    let cards = asVocabCards s
    if null cards
        then modify $ \st -> st { asMessage = Just "No vocabulary loaded!" }
        else do
            let exType = case mode of
                    WritingMode -> Writing
                    ReadingMode -> Reading
                    TypingMode -> Typing

            -- Select cards: prioritize due cards, then new cards
            let dueCardIds = asDueCards s
                vocabStates = asVocabStates s
                cardMap = Map.fromList [(vocabId c, c) | c <- cards]

                -- Get due cards (cards that have been reviewed before and are due)
                dueCards = [c | vid <- dueCardIds
                             , Just c <- [Map.lookup vid cardMap]]

                -- Get new cards (never reviewed)
                newCards = [c | c <- cards
                             , not (Map.member (vocabId c) vocabStates)]

                -- Take up to 10: due first, then new
                selectedCards = take 10 (dueCards ++ newCards)

            if null selectedCards
                then modify $ \st -> st { asMessage = Just "No cards due for review!" }
                else do
                    let prompts = map (generateExercisePrompt exType) selectedCards
                        drill = initialDrillState mode prompts
                    modify $ \st -> st
                        { asScreen = DrillScreen
                        , asDrill = Just drill
                        , asMessage = Nothing
                        }

-- | Handle drill events
handleDrillEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleDrillEvent (VtyEvent (V.EvKey V.KEsc [])) = endDrill
handleDrillEvent ev = do
    s <- get
    case asDrill s of
        Nothing -> endDrill
        Just drill -> handleDrillMode (dsMode drill) ev

-- | Handle events based on drill mode
handleDrillMode :: DrillMode -> BrickEvent Name AppEvent -> EventM Name AppState ()
handleDrillMode ReadingMode ev = handleReadingDrill ev
handleDrillMode _ ev = handleWritingDrill ev

-- | Handle writing/typing drill events
handleWritingDrill :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleWritingDrill (VtyEvent (V.EvKey V.KEnter [])) = do
    s <- get
    case asDrill s of
        Nothing -> pure ()
        Just drill -> case dsShowResult drill of
            Just _ -> advanceDrill  -- Move to next exercise
            Nothing -> submitAnswer  -- Check answer
handleWritingDrill (VtyEvent (V.EvKey V.KBS [])) = do
    s <- get
    case asDrill s of
        Nothing -> pure ()
        Just drill -> when (dsShowResult drill == Nothing) $
            modify $ \st -> st { asDrill = Just $ drill { dsUserInput = T.dropEnd 1 (dsUserInput drill) } }
handleWritingDrill (VtyEvent (V.EvKey (V.KChar c) [])) = do
    s <- get
    case asDrill s of
        Nothing -> pure ()
        Just drill -> when (dsShowResult drill == Nothing) $
            modify $ \st -> st { asDrill = Just $ drill { dsUserInput = dsUserInput drill <> T.singleton c } }
handleWritingDrill _ = pure ()

-- | Handle reading drill events
handleReadingDrill :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleReadingDrill (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
    s <- get
    case asDrill s of
        Nothing -> pure ()
        Just drill -> modify $ \st -> st { asDrill = Just $ drill { dsRevealAnswer = True } }
handleReadingDrill (VtyEvent (V.EvKey (V.KChar c) [])) | c `elem` ['1'..'4'] = do
    s <- get
    case asDrill s of
        Nothing -> pure ()
        Just drill -> when (dsRevealAnswer drill) $ do
            let quality = ratingToQuality (digitToInt c)
            recordAndAdvance quality
handleReadingDrill (VtyEvent (V.EvKey V.KEnter [])) = do
    s <- get
    case asDrill s of
        Nothing -> pure ()
        Just drill -> when (dsRevealAnswer drill) $
            recordAndAdvance Good  -- Default to Good on Enter
handleReadingDrill _ = pure ()

-- | Submit an answer and check correctness
submitAnswer :: EventM Name AppState ()
submitAnswer = do
    s <- get
    case asDrill s of
        Nothing -> pure ()
        Just drill -> do
            let idx = dsCurrentIndex drill
            when (idx < length (dsExercises drill)) $ do
                let prompt = dsExercises drill !! idx
                    isCorrect = checkAnswer prompt (dsUserInput drill)
                    vocabId' = epVocabId prompt
                    exType = epExerciseType prompt

                -- Update drill state
                modify $ \st -> st
                    { asDrill = Just $ drill
                        { dsShowResult = Just isCorrect
                        , dsCorrectCount = if isCorrect then dsCorrectCount drill + 1 else dsCorrectCount drill
                        , dsTotalCount = dsTotalCount drill + 1
                        }
                    }

                -- Update SRS state
                let quality = if isCorrect then Good else Again
                updateCardSRS vocabId' quality exType

-- | Record result and advance to next exercise (for reading mode)
recordAndAdvance :: Quality -> EventM Name AppState ()
recordAndAdvance quality = do
    s <- get
    case asDrill s of
        Nothing -> pure ()
        Just drill -> do
            let idx = dsCurrentIndex drill
            when (idx < length (dsExercises drill)) $ do
                let prompt = dsExercises drill !! idx
                    vocabId' = epVocabId prompt
                    exType = epExerciseType prompt

                -- Update SRS state
                updateCardSRS vocabId' quality exType

            let newDrill = drill
                    { dsCorrectCount = if quality >= Good then dsCorrectCount drill + 1 else dsCorrectCount drill
                    , dsTotalCount = dsTotalCount drill + 1
                    }
            put $ s { asDrill = Just newDrill }
            advanceDrill

-- | Advance to next exercise
advanceDrill :: EventM Name AppState ()
advanceDrill = do
    s <- get
    case asDrill s of
        Nothing -> pure ()
        Just drill -> do
            let newIdx = dsCurrentIndex drill + 1
            if newIdx >= length (dsExercises drill)
                then endDrill
                else modify $ \st -> st
                    { asDrill = Just $ drill
                        { dsCurrentIndex = newIdx
                        , dsUserInput = ""
                        , dsShowResult = Nothing
                        , dsRevealAnswer = False
                        }
                    }

-- | End drill session and return to menu
endDrill :: EventM Name AppState ()
endDrill = do
    s <- get
    let msg = case asDrill s of
            Nothing -> Nothing
            Just drill -> Just $ "Session complete! Score: "
                <> T.pack (show $ dsCorrectCount drill)
                <> "/"
                <> T.pack (show $ dsTotalCount drill)
    modify $ \st -> st
        { asScreen = MainMenuScreen
        , asDrill = Nothing
        , asMessage = msg
        }

-- | Update SRS state for a vocabulary card after review
-- 1. Get current state from asVocabStates (or initialize new)
-- 2. Call SM-2 algorithm to calculate new state
-- 3. Update asVocabStates in memory
-- 4. Persist via runJsonRepository
-- 5. Log the activity
updateCardSRS :: VocabularyId -> Quality -> ExerciseType -> EventM Name AppState ()
updateCardSRS vocabId' quality exType = do
    s <- get

    -- Get current time
    utcNow <- liftIO getCurrentTime
    let now = utcToLocalTime utc utcNow

    -- Get or initialize vocabulary state
    let vocabStates = asVocabStates s
        currentState = case Map.lookup vocabId' vocabStates of
            Just state -> state
            Nothing -> newVocabularyState vocabId' now

    -- Calculate new SRS state using SM-2
    let srsResult = calculateReview defaultSM2 currentState quality now
        newState = applySRSResult currentState quality now srsResult

    -- Update in-memory state
    let newVocabStates = Map.insert vocabId' newState vocabStates

    -- Recalculate due cards
    let newDueCards = Map.keys $ Map.filter isDue newVocabStates
        isDue state = vstNextReviewDate state <= now

    modify $ \st -> st
        { asVocabStates = newVocabStates
        , asDueCards = newDueCards
        }

    -- Persist to JSON
    liftIO $ runJsonRepository $ saveVocabState newState

    -- Log activity
    let performance = Performance
            { perfAccuracy = Percentage (if quality >= Good then 100 else 0)
            , perfTimeSpent = 0  -- TODO: track actual time
            , perfWpm = Nothing
            }
        activity = ActivityEntry
            { actDate = now
            , actExerciseType = exType
            , actVocabularyId = Just vocabId'
            , actPerformance = performance
            , actSuccess = quality >= Good
            , actNotes = Nothing
            }
    liftIO $ runJsonRepository $ logActivity activity

-- | Handle progress screen events
handleProgressEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleProgressEvent (VtyEvent (V.EvKey V.KEsc [])) =
    modify $ \s -> s { asScreen = MainMenuScreen }
handleProgressEvent (VtyEvent (V.EvKey V.KEnter [])) =
    modify $ \s -> s { asScreen = MainMenuScreen }
handleProgressEvent _ = pure ()

-- | Handle help screen events
handleHelpEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleHelpEvent (VtyEvent (V.EvKey V.KEsc [])) =
    modify $ \s -> s { asScreen = MainMenuScreen }
handleHelpEvent (VtyEvent (V.EvKey V.KEnter [])) =
    modify $ \s -> s { asScreen = MainMenuScreen }
handleHelpEvent (VtyEvent (V.EvKey (V.KChar '?') [])) =
    modify $ \s -> s { asScreen = MainMenuScreen }
handleHelpEvent _ = pure ()

-- | Handle quit confirmation events
handleQuitEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleQuitEvent (VtyEvent (V.EvKey (V.KChar 'y') [])) = halt
handleQuitEvent (VtyEvent (V.EvKey (V.KChar 'Y') [])) = halt
handleQuitEvent (VtyEvent (V.EvKey (V.KChar 'n') [])) =
    modify $ \s -> s { asScreen = MainMenuScreen }
handleQuitEvent (VtyEvent (V.EvKey (V.KChar 'N') [])) =
    modify $ \s -> s { asScreen = MainMenuScreen }
handleQuitEvent (VtyEvent (V.EvKey V.KEsc [])) =
    modify $ \s -> s { asScreen = MainMenuScreen }
handleQuitEvent _ = pure ()

-- | Start typing practice - go to level selector
startTypingPractice :: EventM Name AppState ()
startTypingPractice = do
    s <- get
    let words = asTypingWords s
    if null words
        then modify $ \st -> st { asMessage = Just "No typing vocabulary loaded!" }
        else do
            -- Create initial typing state for level selection
            case headMay mainLevels of
                Nothing -> modify $ \st -> st { asMessage = Just "Error loading levels" }
                Just level -> do
                    let ts = initialTypingState level Echo [] words
                    modify $ \st -> st
                        { asScreen = TypingLevelSelectScreen
                        , asTyping = Just ts
                            { typSelectedLevel = tlNumber level
                            , typLevelSelectMode = TopLevelSelect
                            , typSelectedIndex = 0
                            }
                        , asMessage = Nothing
                        }

-- | Handle level selection events
handleLevelSelectEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleLevelSelectEvent (VtyEvent (V.EvKey V.KEsc [])) = handleLevelSelectEsc
handleLevelSelectEvent (VtyEvent (V.EvKey V.KUp [])) = navigateLevelUp
handleLevelSelectEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) = navigateLevelUp
handleLevelSelectEvent (VtyEvent (V.EvKey V.KDown [])) = navigateLevelDown
handleLevelSelectEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = navigateLevelDown
handleLevelSelectEvent (VtyEvent (V.EvKey V.KEnter [])) = handleLevelSelectEnter
handleLevelSelectEvent _ = pure ()

-- | Handle Esc in level selector (back or exit)
handleLevelSelectEsc :: EventM Name AppState ()
handleLevelSelectEsc = do
    s <- get
    case asTyping s of
        Nothing -> modify $ \st -> st { asScreen = MainMenuScreen }
        Just ts -> case typLevelSelectMode ts of
            TopLevelSelect ->
                -- At top level, exit to main menu
                modify $ \st -> st { asScreen = MainMenuScreen, asTyping = Nothing }
            SubLevelSelect _ ->
                -- In sub-level, go back to top level
                modify $ \st -> st
                    { asTyping = Just ts
                        { typLevelSelectMode = TopLevelSelect
                        , typSelectedIndex = 0  -- Reset to first main level
                        }
                    }

-- | Navigate up in current level list
navigateLevelUp :: EventM Name AppState ()
navigateLevelUp = do
    s <- get
    case asTyping s of
        Nothing -> pure ()
        Just ts -> do
            let currentIdx = typSelectedIndex ts
            when (currentIdx > 0) $
                modify $ \st -> st { asTyping = Just ts { typSelectedIndex = currentIdx - 1 } }

-- | Navigate down in current level list
navigateLevelDown :: EventM Name AppState ()
navigateLevelDown = do
    s <- get
    case asTyping s of
        Nothing -> pure ()
        Just ts -> do
            let currentIdx = typSelectedIndex ts
                maxIdx = case typLevelSelectMode ts of
                    TopLevelSelect -> length mainLevels - 1
                    SubLevelSelect parentLevel -> length (getSubLevels parentLevel) - 1
            when (currentIdx < maxIdx) $
                modify $ \st -> st { asTyping = Just ts { typSelectedIndex = currentIdx + 1 } }

-- | Handle Enter in level selector (drill-in or start)
handleLevelSelectEnter :: EventM Name AppState ()
handleLevelSelectEnter = do
    s <- get
    case asTyping s of
        Nothing -> pure ()
        Just ts -> case typLevelSelectMode ts of
            TopLevelSelect -> do
                -- Get the selected main level
                let idx = typSelectedIndex ts
                case safeIndex mainLevels idx of
                    Nothing -> pure ()
                    Just level -> do
                        let levelNum = tlNumber level
                        if hasSubLevels levelNum
                            then
                                -- Drill into sub-levels
                                modify $ \st -> st
                                    { asTyping = Just ts
                                        { typLevelSelectMode = SubLevelSelect levelNum
                                        , typSelectedIndex = 0
                                        }
                                    }
                            else
                                -- Start the level directly
                                startLevelSession level ts
            SubLevelSelect parentLevel -> do
                -- Get the selected sub-level
                let subs = getSubLevels parentLevel
                    idx = typSelectedIndex ts
                case safeIndex subs idx of
                    Nothing -> pure ()
                    Just level -> startLevelSession level ts

-- | Safe index into list
safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:_) 0 = Just x
safeIndex (_:xs) n = if n < 0 then Nothing else safeIndex xs (n - 1)

-- | Safe head
headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

-- | Start a typing practice session at the given level
startLevelSession :: TypingLevel -> TypingState -> EventM Name AppState ()
startLevelSession level ts = do
    let levelNum = tlNumber level
        -- Filter words for this exact level
        levelWords = filter (\w -> twLevel w == levelNum) (typWords ts)
    if null levelWords
        then modify $ \st -> st { asMessage = Just $ "No words for level " <> tlName level }
        else do
            let prompts = createSessionPrompts Echo (take 20 levelWords)
                newTs = ts
                    { typLevel = level
                    , typPrompts = prompts
                    , typCurrentIndex = 0
                    , typTypedJamo = []
                    , typCharStatuses = []
                    , typStartTime = Nothing
                    , typStats = emptyTypingStats
                    }
            modify $ \st -> st
                { asScreen = TypingPracticeScreen
                , asTyping = Just newTs
                }

-- | Handle typing practice events
handleTypingEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleTypingEvent (VtyEvent (V.EvKey V.KEsc [])) = endTypingPractice
handleTypingEvent (VtyEvent (V.EvKey (V.KChar '?') [])) = toggleHints
handleTypingEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = skipTypingWord
handleTypingEvent (VtyEvent (V.EvKey V.KBS [])) = handleTypingBackspace
handleTypingEvent (VtyEvent (V.EvKey (V.KChar c) [])) = handleTypingKeypress c
handleTypingEvent _ = pure ()

-- | Handle a keypress in typing mode
handleTypingKeypress :: Char -> EventM Name AppState ()
handleTypingKeypress c = do
    s <- get
    case asTyping s of
        Nothing -> pure ()
        Just ts -> case qwertyToJamo c of
            Nothing -> pure ()  -- Not a Korean key
            Just jamo -> do
                let currentPrompt = getCurrentTypingPrompt ts
                case currentPrompt of
                    Nothing -> advanceTypingWord  -- No more prompts
                    Just prompt -> do
                        let expected = tpExpectedJamo prompt
                        let typed = typTypedJamo ts
                        let newTyped = typed ++ [jamo]
                        let statuses = validateTyping expected newTyped
                        let isCorrect = length newTyped <= length expected &&
                                       statuses !! (length newTyped - 1) == Correct

                        -- Update stats
                        let stats = typStats ts
                        let newStats = if isCorrect
                                then stats { tsStreak = tsStreak stats + 1 }
                                else stats { tsStreak = 0 }

                        let newTs = ts
                                { typTypedJamo = newTyped
                                , typCharStatuses = statuses
                                , typLastKeyCorrect = Just isCorrect
                                , typStats = newStats
                                }

                        modify $ \st -> st { asTyping = Just newTs }

                        -- Check if word is complete
                        when (length newTyped >= length expected) $ do
                            let allCorrect = all (== Correct) statuses
                            updateTypingStatsAndAdvance allCorrect

-- | Handle backspace in typing mode
handleTypingBackspace :: EventM Name AppState ()
handleTypingBackspace = do
    s <- get
    case asTyping s of
        Nothing -> pure ()
        Just ts -> do
            let typed = typTypedJamo ts
            when (not $ null typed) $ do
                let newTyped = init typed
                let currentPrompt = getCurrentTypingPrompt ts
                let statuses = case currentPrompt of
                        Just prompt -> validateTyping (tpExpectedJamo prompt) newTyped
                        Nothing -> []
                modify $ \st -> st
                    { asTyping = Just ts
                        { typTypedJamo = newTyped
                        , typCharStatuses = statuses
                        , typLastKeyCorrect = Nothing
                        }
                    }

-- | Get current typing prompt
getCurrentTypingPrompt :: TypingState -> Maybe TypingPrompt
getCurrentTypingPrompt ts
    | typCurrentIndex ts < length (typPrompts ts) =
        Just (typPrompts ts !! typCurrentIndex ts)
    | otherwise = Nothing

-- | Update stats and advance to next word
updateTypingStatsAndAdvance :: Bool -> EventM Name AppState ()
updateTypingStatsAndAdvance wasCorrect = do
    s <- get
    case asTyping s of
        Nothing -> pure ()
        Just ts -> do
            let stats = typStats ts
            let newStats = stats
                    { tsWordsCompleted = tsWordsCompleted stats + 1
                    , tsCorrectWords = if wasCorrect
                        then tsCorrectWords stats + 1
                        else tsCorrectWords stats
                    , tsAccuracy = let total = tsWordsCompleted stats + 1
                                       correct = if wasCorrect
                                           then tsCorrectWords stats + 1
                                           else tsCorrectWords stats
                                   in (fromIntegral correct / fromIntegral total) * 100
                    }
            modify $ \st -> st { asTyping = Just ts { typStats = newStats } }
            advanceTypingWord

-- | Skip current word
skipTypingWord :: EventM Name AppState ()
skipTypingWord = updateTypingStatsAndAdvance False

-- | Advance to next typing word
advanceTypingWord :: EventM Name AppState ()
advanceTypingWord = do
    s <- get
    case asTyping s of
        Nothing -> pure ()
        Just ts -> do
            let newIdx = typCurrentIndex ts + 1
            if newIdx >= length (typPrompts ts)
                then endTypingPractice
                else modify $ \st -> st
                    { asTyping = Just ts
                        { typCurrentIndex = newIdx
                        , typTypedJamo = []
                        , typCharStatuses = []
                        , typStartTime = Nothing
                        , typLastKeyCorrect = Nothing
                        }
                    }

-- | Toggle hints display
toggleHints :: EventM Name AppState ()
toggleHints = do
    s <- get
    case asTyping s of
        Nothing -> pure ()
        Just ts -> modify $ \st -> st
            { asTyping = Just ts { typShowHints = not (typShowHints ts) }
            }

-- | End typing practice and return to level selector
endTypingPractice :: EventM Name AppState ()
endTypingPractice = do
    s <- get
    case asTyping s of
        Nothing -> modify $ \st -> st { asScreen = MainMenuScreen }
        Just ts -> do
            let stats = typStats ts
                levelNum = tlNumber (typLevel ts)
                accuracy = tsAccuracy stats
                completedEnough = tsWordsCompleted stats >= 5  -- Minimum words to count
                accuracyPassed = accuracy >= 80.0
                shouldMarkComplete = completedEnough && accuracyPassed

            -- Mark level as completed if criteria met
            when shouldMarkComplete $ do
                liftIO $ runJsonRepository $ markLevelCompleted levelNum
                -- Update local progress
                let oldProgress = asTypingProgress s
                    newProgress = oldProgress
                        { tpCompletedLevels = Set.insert levelNum (tpCompletedLevels oldProgress)
                        }
                modify $ \st -> st { asTypingProgress = newProgress }

            let completionNote = if shouldMarkComplete then " ★ Level complete!" else ""
                msg = "Session complete! "
                    <> T.pack (show $ tsCorrectWords stats)
                    <> "/"
                    <> T.pack (show $ tsWordsCompleted stats)
                    <> " words | Accuracy: "
                    <> T.pack (show (round accuracy :: Int))
                    <> "%"
                    <> completionNote

            -- Return to level selector (not main menu)
            modify $ \st -> st
                { asScreen = TypingLevelSelectScreen
                , asTyping = Just ts
                    { typPrompts = []
                    , typCurrentIndex = 0
                    , typTypedJamo = []
                    , typCharStatuses = []
                    }
                , asMessage = Just msg
                }

