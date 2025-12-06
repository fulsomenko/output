{-# LANGUAGE OverloadedStrings #-}

module Output.TUI.Events
    ( handleEvent
    ) where

import Control.Monad (when)
import Brick
import qualified Graphics.Vty as V
import qualified Data.Text as T
import Data.Text (Text)

import Output.TUI.Types
import Output.Domain.Exercise (ExercisePrompt(..), checkAnswer, generateExercisePrompt)
import Output.Domain.Types (ExerciseType(..))

-- | Main event handler
handleEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent ev = do
    s <- get
    case asScreen s of
        MainMenuScreen -> handleMenuEvent ev
        DrillScreen -> handleDrillEvent ev
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
    modify $ \s -> s { asMenuIndex = min 5 (asMenuIndex s + 1) }
handleMenuEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) =
    modify $ \s -> s { asMenuIndex = min 5 (asMenuIndex s + 1) }
handleMenuEvent (VtyEvent (V.EvKey V.KEnter [])) = do
    s <- get
    case asMenuIndex s of
        0 -> startDrill WritingMode  -- Writing drill
        1 -> startDrill ReadingMode  -- Reading drill
        2 -> startDrill TypingMode   -- Typing drill
        3 -> modify $ \st -> st { asScreen = ProgressScreen }
        4 -> modify $ \st -> st { asScreen = HelpScreen }
        5 -> modify $ \st -> st { asScreen = QuitConfirmScreen }
        _ -> pure ()
handleMenuEvent (VtyEvent (V.EvKey (V.KChar '?') [])) =
    modify $ \s -> s { asScreen = HelpScreen }
handleMenuEvent _ = pure ()

-- | Start a drill session
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
            let prompts = map (generateExercisePrompt exType) (take 10 cards)
            let drill = initialDrillState mode prompts
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
            let isCorrect = c >= '3'  -- 3 or 4 = correct, 1 or 2 = incorrect
            recordAndAdvance isCorrect
handleReadingDrill (VtyEvent (V.EvKey V.KEnter [])) = do
    s <- get
    case asDrill s of
        Nothing -> pure ()
        Just drill -> when (dsRevealAnswer drill) $
            recordAndAdvance True  -- Default to correct on Enter
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
                let isCorrect = checkAnswer prompt (dsUserInput drill)
                modify $ \st -> st
                    { asDrill = Just $ drill
                        { dsShowResult = Just isCorrect
                        , dsCorrectCount = if isCorrect then dsCorrectCount drill + 1 else dsCorrectCount drill
                        , dsTotalCount = dsTotalCount drill + 1
                        }
                    }

-- | Record result and advance to next exercise
recordAndAdvance :: Bool -> EventM Name AppState ()
recordAndAdvance isCorrect = do
    s <- get
    case asDrill s of
        Nothing -> pure ()
        Just drill -> do
            let newDrill = drill
                    { dsCorrectCount = if isCorrect then dsCorrectCount drill + 1 else dsCorrectCount drill
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
