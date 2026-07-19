{-# LANGUAGE OverloadedStrings #-}

module Output.TUI.Events
    ( handleEvent
    ) where

import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe, isJust)
import Brick
import Brick.BChan (BChan, writeBChan)
import qualified Graphics.Vty as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, utcToLocalTime, utc, localDay, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Map as Map

import Output.TUI.Types
import Output.Domain.Exercise (ExercisePrompt(..), checkAnswer, generateExercisePrompt)
import Output.Domain.Types
    ( ExerciseType(..)
    , TypingProgress(..)
    , VocabularyId
    , VocabularyCard(..)
    , VocabularyState(..)
    , MasteryLevel(..)
    , newVocabularyState
    , TOPIK_Level(..)
    )
import Output.Domain.Jamo (qwertyToJamo)
import Output.Domain.TypingLevel (TypingLevel(..), mainLevels, getSubLevels, hasSubLevels)
import Output.Domain.TypingWord (TypingWord(..))
import Output.Domain.TypingExercise (TypingExerciseType(..), TypingPrompt(..), createSessionPrompts, validateTyping, CharStatus(..))
import Output.Domain.Activity (ActivityEntry(..), Performance(..), Percentage(..))
import Output.Domain.Settings (AppSettings(..), Language(..), showLanguage)
import Output.Repository.Json
    ( runJsonRepository, saveSettings, saveLearnedWord
    , loadStudentProfile, appendSessionNote
    )
import Output.Repository.Class (markLevelCompleted, saveVocabState, logActivity, getAllActivities)
import Output.Algorithm.SRS (Quality(..), SRSAlgorithm(..), ratingToQuality)
import Output.Algorithm.SpacedRepetition (defaultSM2, applySRSResult)
import Output.LLM.Client (callOllama, ollamaFromSettings, OllamaMessage(..))
import Output.LLM.Agent (AgentTask(..), agentSystemPrompt, parseAssessmentLevel)
import Output.LLM.Persona (Persona(..))
import Output.LLM.Extractor (ExtractedWord(..), extractVocabFromConversation, isSpokenOccasion)
import Output.LLM.Summarizer (summarizeSession)
import Output.Domain.StudentProfile (StudentProfile(..), SessionNote(..))
import qualified Data.Set as Set

-- | Main event handler. The BChan is threaded through so LLM handlers
-- can fork async calls and deliver results back to the app.
-- Async background events (LLMSummary, LLMExtraction) are handled here
-- unconditionally — they may arrive after the user has left the chat screen.
handleEvent :: BChan AppEvent -> BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent _ (AppEvent (LLMSummary summary))      = handleSummaryEvent summary
handleEvent _ (AppEvent (LLMExtraction extracted)) = handleExtractionEvent extracted
handleEvent chan ev = do
    s <- get
    case asScreen s of
        MainMenuScreen          -> handleMenuEvent chan ev
        DrillScreen             -> handleDrillEvent ev
        TypingPracticeScreen    -> handleTypingEvent ev
        TypingLevelSelectScreen -> handleLevelSelectEvent ev
        ProgressScreen          -> handleProgressEvent ev
        StatsScreen             -> handleStatsEvent ev
        DayDetailScreen         -> handleDayDetailEvent ev
        HelpScreen              -> handleHelpEvent ev
        QuitConfirmScreen       -> handleQuitEvent ev
        LLMChatScreen           -> handleLLMChatEvent chan ev
        SettingsScreen          -> handleSettingsEvent ev
        PersonaScreen           -> handlePersonaEvent ev

-- | Append an incoming session note to the student profile (disk + in-memory).
-- The note log is append-only so no past observations are ever lost.
handleSummaryEvent :: Text -> EventM Name AppState ()
handleSummaryEvent noteContent = do
    now <- liftIO getCurrentTime
    let localNow = utcToLocalTime utc now
        note     = SessionNote { snDate = localNow, snContent = noteContent }
    liftIO $ appendSessionNote note
    modify $ \st -> st
        { asStudentProfile = Just $ case asStudentProfile st of
            Nothing ->
                StudentProfile { spNotes = [note], spLastUpdated = localNow }
            Just p  ->
                p { spNotes = spNotes p ++ [note], spLastUpdated = localNow }
        }

-- | Handle vocabulary words extracted from a lesson (screen-independent).
handleExtractionEvent :: [ExtractedWord] -> EventM Name AppState ()
handleExtractionEvent extracted = do
    s <- get
    let level = maybe One intToLevel (settingsKoreanLevel (asSettings s))
    newCards <- liftIO $ mapM (saveLearnedWord level) extracted
    now <- liftIO getCurrentTime
    let localNow = utcToLocalTime utc now
        saved    = [c | Just c <- newCards]
    when (not $ null saved) $ do
        let newVocabCards  = asVocabCards s ++ saved
            initialStates  = map (\c -> (vocabId c, newVocabularyState (vocabId c) localNow)) saved
            newVocabStates = foldr (\(vid, vs) m -> Map.insert vid vs m)
                                   (asVocabStates s) initialStates
        modify $ \st -> st
            { asVocabCards  = newVocabCards
            , asVocabStates = newVocabStates
            }

-- | Handle menu events
handleMenuEvent :: BChan AppEvent -> BrickEvent Name AppEvent -> EventM Name AppState ()
handleMenuEvent _    (VtyEvent (V.EvKey V.KEsc [])) =
    modify $ \s -> s { asScreen = QuitConfirmScreen }
handleMenuEvent _    (VtyEvent (V.EvKey (V.KChar 'q') [])) =
    modify $ \s -> s { asScreen = QuitConfirmScreen }
handleMenuEvent _    (VtyEvent (V.EvKey V.KUp [])) =
    modify $ \s -> s { asMenuIndex = max 0 (asMenuIndex s - 1) }
handleMenuEvent _    (VtyEvent (V.EvKey (V.KChar 'k') [])) =
    modify $ \s -> s { asMenuIndex = max 0 (asMenuIndex s - 1) }
handleMenuEvent _    (VtyEvent (V.EvKey V.KDown [])) =
    modify $ \s -> s { asMenuIndex = min 14 (asMenuIndex s + 1) }
handleMenuEvent _    (VtyEvent (V.EvKey (V.KChar 'j') [])) =
    modify $ \s -> s { asMenuIndex = min 14 (asMenuIndex s + 1) }
handleMenuEvent chan (VtyEvent (V.EvKey V.KEnter [])) = do
    s <- get
    case asMenuIndex s of
        -- AI lessons (0-4)
        0 -> startLLMLesson chan AssessLevel
        1 -> startLLMLesson chan HaveConversation
        2 -> startLLMLesson chan GenerateVocabulary
        3 -> startLLMLesson chan GenerateSentences
        4 -> startLLMLesson chan TeachGrammar
        -- Existing drills (5-9)
        5 -> startDueCardReview
        6 -> startTypingPractice
        7 -> startDrill TypingMode
        8 -> startDrill ReadingMode
        9 -> startDrill WritingMode
        -- Progress / stats (10-11)
        10 -> modify $ \st -> st { asScreen = ProgressScreen }
        11 -> do
            freshActivities <- liftIO $ runJsonRepository getAllActivities
            modify $ \st -> st { asScreen = StatsScreen, asActivities = freshActivities, asStatsSelectedDay = 0 }
        -- Settings / help / quit (12-14)
        12 -> modify $ \st -> st { asScreen = SettingsScreen }
        13 -> modify $ \st -> st { asScreen = HelpScreen }
        14 -> modify $ \st -> st { asScreen = QuitConfirmScreen }
        _ -> pure ()
handleMenuEvent _    (VtyEvent (V.EvKey (V.KChar '?') [])) =
    modify $ \s -> s { asScreen = HelpScreen }
handleMenuEvent _ _ = pure ()

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
                    utcNow <- liftIO getCurrentTime
                    let prompts = map (generateExercisePrompt exType) selectedCards
                        drill = (initialDrillState mode prompts) { dsCardStartTime = Just utcNow }
                    modify $ \st -> st
                        { asScreen = DrillScreen
                        , asDrill = Just drill
                        , asMessage = Nothing
                        }

-- | Start a review session for all cards currently due in the SRS queue
startDueCardReview :: EventM Name AppState ()
startDueCardReview = do
    s <- get
    let dueIds  = asDueCards s
        cardMap = Map.fromList [(vocabId c, c) | c <- asVocabCards s]
        dueCards = [c | vid <- dueIds, Just c <- [Map.lookup vid cardMap]]
    if null dueCards
        then modify $ \st -> st { asMessage = Just "No cards due — great work! Check back later." }
        else do
            utcNow <- liftIO getCurrentTime
            let prompts = map (generateExercisePrompt Reading) dueCards
                drill   = (initialDrillState ReadingMode prompts) { dsCardStartTime = Just utcNow }
            modify $ \st -> st
                { asScreen = DrillScreen
                , asDrill  = Just drill
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
                utcNow <- liftIO getCurrentTime
                let prompt = dsExercises drill !! idx
                    isCorrect = checkAnswer prompt (dsUserInput drill)
                    vocabId' = epVocabId prompt
                    exType = epExerciseType prompt
                    cardTime = case dsCardStartTime drill of
                        Nothing    -> 0
                        Just start -> round (diffUTCTime utcNow start)

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
                updateCardSRS vocabId' quality exType cardTime

-- | Record result and advance to next exercise (for reading mode)
recordAndAdvance :: Quality -> EventM Name AppState ()
recordAndAdvance quality = do
    s <- get
    case asDrill s of
        Nothing -> pure ()
        Just drill -> do
            let idx = dsCurrentIndex drill
            when (idx < length (dsExercises drill)) $ do
                utcNow <- liftIO getCurrentTime
                let prompt = dsExercises drill !! idx
                    vocabId' = epVocabId prompt
                    exType = epExerciseType prompt
                    cardTime = case dsCardStartTime drill of
                        Nothing    -> 0
                        Just start -> round (diffUTCTime utcNow start)
                -- Update SRS state
                updateCardSRS vocabId' quality exType cardTime

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
                else do
                    utcNow <- liftIO getCurrentTime
                    modify $ \st -> st
                        { asDrill = Just $ drill
                            { dsCurrentIndex = newIdx
                            , dsUserInput = ""
                            , dsShowResult = Nothing
                            , dsRevealAnswer = False
                            , dsCardStartTime = Just utcNow
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

-- ---------------------------------------------------------------------------
-- AI Lesson handlers
-- ---------------------------------------------------------------------------

-- | Build a student brief from the current AppState.
-- Combines quantitative stats (TOPIK level, vocabulary mastery) with the
-- AI-generated qualitative summary stored in asStudentProfile.
studentBrief :: AppState -> Maybe Text
studentBrief s
    | statsEmpty && profileEmpty = Nothing
    | otherwise                  = Just (T.unlines $ statsLines ++ profileLines)
  where
    settings = asSettings s
    states   = Map.elems (asVocabStates s)
    total    = length (asVocabCards s)
    nMastered      = length [v | v <- states, vstMasteryLevel v == Mastered]
    nIntermediate  = length [v | v <- states, vstMasteryLevel v == Intermediate]
    nLearning      = length [v | v <- states, vstMasteryLevel v == Learning]
    nNew           = length [v | v <- states, vstMasteryLevel v == New]
    levelStr = maybe "Not yet assessed"
                     (\l -> "TOPIK " <> T.pack (show l))
                     (settingsKoreanLevel settings)
    statsEmpty   = total == 0 && settingsKoreanLevel settings == Nothing
    profileEmpty = all (null . spNotes) (asStudentProfile s)
    statsLines =
        [ "TOPIK Level: " <> levelStr
        , "Vocabulary:   " <> T.pack (show total) <> " words"
            <> "  (" <> T.pack (show nMastered)     <> " Mastered"
            <> " · " <> T.pack (show nIntermediate) <> " Intermediate"
            <> " · " <> T.pack (show nLearning)     <> " Learning"
            <> " · " <> T.pack (show nNew)           <> " New)"
        ]
    recentNotes = case asStudentProfile s of
        Nothing -> []
        Just p  -> takeLast 5 (spNotes p)
    takeLast n xs = drop (max 0 (length xs - n)) xs
    profileLines
        | null recentNotes = []
        | otherwise =
            "" : "Session history (oldest → newest):" : map formatNote recentNotes
    formatNote note =
        "[" <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d" (snDate note)) <> "] "
        <> snContent note

-- | Open an AI lesson chat. Kicks off the AI's opening message immediately.
startLLMLesson :: BChan AppEvent -> AgentTask -> EventM Name AppState ()
startLLMLesson chan task = do
    s <- get
    let settings  = asSettings s
        level     = fromMaybe 1 (settingsKoreanLevel settings)
        persona   = Persona { personaName  = settingsPersonaName  settings
                            , personaStyle = settingsPersonaStyle settings }
        chat      = initialLLMChatState task persona
        brief     = studentBrief s
        sysPrompt = agentSystemPrompt persona task (settingsLanguage settings) level brief
        cfg       = ollamaFromSettings settings
    put s { asScreen = LLMChatScreen, asLLMChat = Just chat, asMessage = Nothing }
    liftIO $ void $ forkIO $ do
        let openingMsg = OllamaMessage { role = "user", content = "[Begin lesson]" }
        result <- callOllama cfg sysPrompt [openingMsg]
        writeBChan chan (LLMResponse result)

-- | Handle events on the LLM chat screen.
-- Async events are mode-independent; key events are routed by llmInputMode.
handleLLMChatEvent :: BChan AppEvent -> BrickEvent Name AppEvent -> EventM Name AppState ()
-- Async response arrived from the background thread
handleLLMChatEvent chan (AppEvent (LLMResponse result)) = do
    s <- get
    case asLLMChat s of
        Nothing   -> pure ()
        Just chat -> case result of
            Left err ->
                modify $ \st -> st
                    { asLLMChat = Just chat { llmWaiting = False, llmError = Just err } }
            Right resp -> do
                let spoken       = isSpokenOccasion resp
                    assistantMsg = LLMMessage { llmRole = "assistant", llmContent = resp
                                              , llmIsSpoken = spoken }
                    newMsgs      = llmMessages chat ++ [assistantMsg]
                    detectedLevel = parseAssessmentLevel resp
                    newSettings   = case detectedLevel of
                        Nothing  -> asSettings s
                        Just lvl -> (asSettings s) { settingsKoreanLevel = Just lvl }
                when (isJust detectedLevel) $
                    liftIO $ saveSettings newSettings
                modify $ \st -> st
                    { asLLMChat  = Just chat { llmMessages = newMsgs, llmWaiting = False }
                    , asSettings = newSettings
                    }
                vScrollToEnd (viewportScroll ChatHistoryViewport)
                let settings = asSettings s
                    cfg      = ollamaFromSettings settings
                    lang     = settingsLanguage settings
                    oMsgs    = map (\m -> OllamaMessage (llmRole m) (llmContent m)) newMsgs
                liftIO $ void $ forkIO $ do
                    extracted <- extractVocabFromConversation cfg lang oMsgs
                    case extracted of
                        Right ws | not (null ws) -> writeBChan chan (LLMExtraction ws)
                        _                        -> pure ()
-- Route key events by input mode
handleLLMChatEvent chan ev = do
    s <- get
    case asLLMChat s of
        Nothing   -> pure ()
        Just chat -> case llmInputMode chat of
            NormalMode -> handleChatNormal chan ev
            InsertMode -> handleChatInsert chan ev

-- | Normal mode: navigation, send, mode switch, keyboard toggle.
handleChatNormal :: BChan AppEvent -> BrickEvent Name AppEvent -> EventM Name AppState ()
handleChatNormal chan (VtyEvent (V.EvKey V.KEnter [])) = sendChatMessage chan
handleChatNormal chan (VtyEvent (V.EvKey V.KEsc [])) = do
    s <- get
    case asLLMChat s of
        Just chat | not (null (llmMessages chat)) -> do
            let settings = asSettings s
                cfg      = ollamaFromSettings settings
                lang     = settingsLanguage settings
                oMsgs    = map (\m -> OllamaMessage (llmRole m) (llmContent m))
                               (llmMessages chat)
            liftIO $ void $ forkIO $ do
                result <- summarizeSession cfg lang oMsgs
                case result of
                    Right noteText -> writeBChan chan (LLMSummary noteText)
                    Left _         -> pure ()
        _ -> pure ()
    modify $ \st -> st { asScreen = MainMenuScreen, asLLMChat = Nothing }
handleChatNormal _ (VtyEvent (V.EvKey (V.KChar 'i') [])) =
    modify $ \s -> case asLLMChat s of
        Nothing -> s
        Just c  -> s { asLLMChat = Just c { llmInputMode = InsertMode } }
handleChatNormal _ (VtyEvent (V.EvKey (V.KChar 'K') [])) = do
    modify $ \s -> case asLLMChat s of
        Nothing -> s
        Just c  -> s { asLLMChat = Just c { llmShowKeyboard = not (llmShowKeyboard c) } }
    -- The ~10-row layout shift leaves ghost rows when vty diffs old→new.
    -- Write a blank picture first so vty's baseline is empty; the next
    -- Brick render then diffs empty→new and sends a complete repaint.
    vty <- getVtyHandle
    liftIO $ do
        (w, h) <- V.displayBounds (V.outputIface vty)
        V.update vty $ V.picForImage $ V.backgroundFill w h
handleChatNormal _ (VtyEvent (V.EvKey V.KUp [])) =
    vScrollBy (viewportScroll ChatHistoryViewport) (-1)
handleChatNormal _ (VtyEvent (V.EvKey V.KDown [])) =
    vScrollBy (viewportScroll ChatHistoryViewport) 1
handleChatNormal _ (VtyEvent (V.EvKey (V.KChar 'k') [])) =
    vScrollBy (viewportScroll ChatHistoryViewport) (-1)
handleChatNormal _ (VtyEvent (V.EvKey (V.KChar 'j') [])) =
    vScrollBy (viewportScroll ChatHistoryViewport) 1
handleChatNormal _ _ = pure ()

-- | Insert mode: typing, backspace, newline, return to normal.
handleChatInsert :: BChan AppEvent -> BrickEvent Name AppEvent -> EventM Name AppState ()
handleChatInsert _ (VtyEvent (V.EvKey V.KEsc [])) =
    modify $ \s -> case asLLMChat s of
        Nothing -> s
        Just c  -> s { asLLMChat = Just c { llmInputMode = NormalMode } }
handleChatInsert _ (VtyEvent (V.EvKey V.KEnter [])) =
    modify $ \s -> case asLLMChat s of
        Nothing -> s
        Just c  -> s { asLLMChat = Just c { llmInput = llmInput c <> "\n" } }
handleChatInsert _ (VtyEvent (V.EvKey (V.KChar ch) [])) =
    modify $ \s -> case asLLMChat s of
        Nothing -> s
        Just c  -> s { asLLMChat = Just c { llmInput = llmInput c <> T.singleton ch } }
handleChatInsert _ (VtyEvent (V.EvKey V.KBS [])) =
    modify $ \s -> case asLLMChat s of
        Nothing -> s
        Just c  -> s { asLLMChat = Just c { llmInput = T.dropEnd 1 (llmInput c) } }
handleChatInsert _ _ = pure ()

-- | Send the current input as a user message.
sendChatMessage :: BChan AppEvent -> EventM Name AppState ()
sendChatMessage chan = do
    s <- get
    case asLLMChat s of
        Nothing   -> pure ()
        Just chat ->
            let input = T.strip (llmInput chat)
            in if T.null input || llmWaiting chat
               then pure ()
               else do
                    let userMsg   = LLMMessage { llmRole = "user", llmContent = input
                                               , llmIsSpoken = False }
                        newMsgs   = llmMessages chat ++ [userMsg]
                        settings  = asSettings s
                        level     = fromMaybe 1 (settingsKoreanLevel settings)
                        brief     = studentBrief s
                        sysPrompt = agentSystemPrompt (llmPersona chat) (llmTask chat)
                                        (settingsLanguage settings) level brief
                        cfg       = ollamaFromSettings settings
                        oMsgs     = map (\m -> OllamaMessage (llmRole m) (llmContent m)) newMsgs
                    modify $ \st -> st
                        { asLLMChat = Just chat
                            { llmMessages  = newMsgs
                            , llmInput     = ""
                            , llmWaiting   = True
                            , llmError     = Nothing
                            , llmInputMode = NormalMode
                            }
                        }
                    liftIO $ void $ forkIO $ do
                        res <- callOllama cfg sysPrompt oMsgs
                        writeBChan chan (LLMResponse res)

-- | Handle settings screen events.
handleSettingsEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleSettingsEvent (VtyEvent (V.EvKey V.KEsc [])) =
    modify $ \s -> s { asScreen = MainMenuScreen }
handleSettingsEvent (VtyEvent (V.EvKey V.KEnter [])) =
    modify $ \s -> s { asScreen = MainMenuScreen }
-- L toggles language
handleSettingsEvent (VtyEvent (V.EvKey (V.KChar 'l') [])) = do
    s <- get
    let old = asSettings s
        new = old { settingsLanguage = case settingsLanguage old of
                        English -> Swedish
                        Swedish -> English
                  }
    liftIO $ saveSettings new
    modify $ \st -> st { asSettings = new }
handleSettingsEvent (VtyEvent (V.EvKey (V.KChar 'p') [])) =
    modify $ \s -> s { asScreen = PersonaScreen }
handleSettingsEvent _ = pure ()

-- | Handle persona screen events.
handlePersonaEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handlePersonaEvent (VtyEvent (V.EvKey V.KEsc [])) =
    modify $ \s -> s { asScreen = SettingsScreen }
-- [N] prompts inline editing of persona name via llmInput as scratch buffer
handlePersonaEvent (VtyEvent (V.EvKey (V.KChar 'n') [])) = do
    s <- get
    let name = settingsPersonaName (asSettings s)
    modify $ \st -> st { asPersonaEdit = Just (PersonaEditName, name) }
handlePersonaEvent (VtyEvent (V.EvKey (V.KChar 't') [])) = do
    s <- get
    let style = settingsPersonaStyle (asSettings s)
    modify $ \st -> st { asPersonaEdit = Just (PersonaEditStyle, style) }
-- While editing: character input, backspace, enter to confirm, esc to cancel
handlePersonaEvent (VtyEvent (V.EvKey (V.KChar c) [])) =
    modify $ \s -> case asPersonaEdit s of
        Nothing         -> s
        Just (field, t) -> s { asPersonaEdit = Just (field, t <> T.singleton c) }
handlePersonaEvent (VtyEvent (V.EvKey V.KBS [])) =
    modify $ \s -> case asPersonaEdit s of
        Nothing         -> s
        Just (field, t) -> s { asPersonaEdit = Just (field, T.dropEnd 1 t) }
handlePersonaEvent (VtyEvent (V.EvKey V.KEnter [])) = do
    s <- get
    case asPersonaEdit s of
        Nothing -> pure ()
        Just (field, value) -> do
            let old = asSettings s
                new = case field of
                    PersonaEditName  -> old { settingsPersonaName  = value }
                    PersonaEditStyle -> old { settingsPersonaStyle = value }
            liftIO $ saveSettings new
            modify $ \st -> st { asSettings = new, asPersonaEdit = Nothing }
handlePersonaEvent _ = pure ()

-- | Update SRS state for a vocabulary card after review
-- 1. Get current state from asVocabStates (or initialize new)
-- 2. Call SM-2 algorithm to calculate new state
-- 3. Update asVocabStates in memory
-- 4. Persist via runJsonRepository
-- 5. Log the activity
updateCardSRS :: VocabularyId -> Quality -> ExerciseType -> Int -> EventM Name AppState ()
updateCardSRS vocabId' quality exType timeSpent = do
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
            , perfTimeSpent = timeSpent
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

-- | Handle stats screen events
handleStatsEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleStatsEvent (VtyEvent (V.EvKey V.KEsc [])) =
    modify $ \s -> s { asScreen = MainMenuScreen }
handleStatsEvent (VtyEvent (V.EvKey V.KUp [])) =
    modify $ \s -> s { asStatsSelectedDay = max 0 (asStatsSelectedDay s - 1) }
handleStatsEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) =
    modify $ \s -> s { asStatsSelectedDay = max 0 (asStatsSelectedDay s - 1) }
handleStatsEvent (VtyEvent (V.EvKey V.KDown [])) = do
    s <- get
    let maxIdx = max 0 (Set.size (Set.fromList (map (localDay . actDate) (asActivities s))) - 1)
    modify $ \st -> st { asStatsSelectedDay = min maxIdx (asStatsSelectedDay st + 1) }
handleStatsEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = do
    s <- get
    let maxIdx = max 0 (Set.size (Set.fromList (map (localDay . actDate) (asActivities s))) - 1)
    modify $ \st -> st { asStatsSelectedDay = min maxIdx (asStatsSelectedDay st + 1) }
handleStatsEvent (VtyEvent (V.EvKey V.KEnter [])) = do
    s <- get
    if null (asActivities s)
        then pure ()
        else modify $ \st -> st { asScreen = DayDetailScreen }
handleStatsEvent _ = pure ()

-- | Handle day detail screen events
handleDayDetailEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleDayDetailEvent (VtyEvent (V.EvKey V.KEsc [])) =
    modify $ \s -> s { asScreen = StatsScreen }
handleDayDetailEvent (VtyEvent (V.EvKey V.KEnter [])) =
    modify $ \s -> s { asScreen = StatsScreen }
handleDayDetailEvent _ = pure ()

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
handleQuitEvent (VtyEvent (V.EvKey V.KEnter [])) = halt
handleQuitEvent (VtyEvent (V.EvKey (V.KChar 'y') [])) = halt
handleQuitEvent (VtyEvent (V.EvKey (V.KChar 'Y') [])) = halt
handleQuitEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleQuitEvent (VtyEvent (V.EvKey (V.KChar 'Q') [])) = halt
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
            utcNow <- liftIO getCurrentTime
            let prompts = createSessionPrompts Echo (take 20 levelWords)
                newTs = ts
                    { typLevel = level
                    , typPrompts = prompts
                    , typCurrentIndex = 0
                    , typTypedJamo = []
                    , typCharStatuses = []
                    , typStartTime = Nothing
                    , typStats = emptyTypingStats
                    , typSessionStart = Just utcNow
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
            utcNow <- liftIO getCurrentTime
            let now = utcToLocalTime utc utcNow
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

            -- Log activity so typing sessions count toward the streak
            when completedEnough $ do
                let elapsedSeconds = case typSessionStart ts of
                        Nothing    -> 0 :: Double
                        Just start -> realToFrac (diffUTCTime utcNow start)
                    wpm = if elapsedSeconds > 0
                            then round ((fromIntegral (tsWordsCompleted stats) / elapsedSeconds) * 60 :: Double)
                            else (0 :: Int)
                    performance = Performance
                        { perfAccuracy  = Percentage accuracy
                        , perfTimeSpent = round elapsedSeconds
                        , perfWpm       = if wpm > 0 then Just wpm else Nothing
                        }
                    activity = ActivityEntry
                        { actDate         = now
                        , actExerciseType = Typing
                        , actVocabularyId = Nothing
                        , actPerformance  = performance
                        , actSuccess      = shouldMarkComplete
                        , actNotes        = Just (tlName (typLevel ts))
                        }
                liftIO $ runJsonRepository $ logActivity activity

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

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Map a TOPIK level integer (1-6) to TOPIK_Level, defaulting to One.
intToLevel :: Int -> TOPIK_Level
intToLevel 2 = Two
intToLevel 3 = Three
intToLevel 4 = Four
intToLevel 5 = Five
intToLevel 6 = Six
intToLevel _ = One

