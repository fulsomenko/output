{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Output.TUI.Types
    ( -- * Widget Names
      Name(..)
      -- * Screens
    , Screen(..)
      -- * Drill State
    , DrillState(..)
    , DrillMode(..)
    , initialDrillState
      -- * Typing State
    , TypingState(..)
    , TypingStats(..)
    , LevelSelectMode(..)
    , initialTypingState
    , emptyTypingStats
      -- * Session Stats
    , SessionStats(..)
    , emptySessionStats
      -- * LLM Chat
    , InputMode(..)
    , LLMMessage(..)
    , LLMChatState(..)
    , initialLLMChatState
      -- * Persona Edit
    , PersonaEditField(..)
      -- * Dictionary Lookup
    , LookupStatus(..)
    , LookupState(..)
    , initialLookupState
    , lookupQuery
    , lookupResults
    , applyJamo
    , popJamo
    , submitLookup
    , retargetLookup
    , applyLookupOutcome
    , moveCursor
    , isSavedWord
    , saveHighlighted
      -- * Application State
    , AppState(..)
    , AppEvent(..)
    , initialAppState
      -- * Attribute Names
    , titleAttr
    , menuAttr
    , menuSelectedAttr
    , promptAttr
    , inputAttr
    , correctAttr
    , incorrectAttr
    , hintAttr
    , statsAttr
    , keyboardAttr
    , keyHighlightAttr
    , keyDisabledAttr
    , aiAttr
    ) where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime, LocalTime)
import Brick (AttrName, attrName)
import GHC.Generics (Generic)

import Output.Domain.Types
import Output.Domain.Activity (ActivityEntry)
import Output.Domain.Exercise (ExercisePrompt)
import Output.Domain.Jamo (Jamo, composeJamoSequence, decomposeKorean)
import Output.Domain.TypingLevel (TypingLevel)
import Output.Domain.TypingWord (TypingWord)
import Output.Domain.TypingExercise (TypingExerciseType, TypingPrompt, CharStatus)
import Output.Domain.Settings (AppSettings, defaultSettings)
import Output.Dict.Krdict (KrdictWord (..))
import Output.LLM.Agent (AgentTask(..))
import Output.LLM.Persona (Persona)
import Output.LLM.Extractor (ExtractedWord)
import Output.Domain.StudentProfile (StudentProfile)

-- | Widget names for focus management
data Name
    = MenuList
    | ExerciseInput
    | ProgressView
    | HelpView
    | TypingInput
    | KeyboardView
    | LevelSelector
    | ChatHistoryViewport
    | LookupViewport
    | VocabularyViewport
    deriving (Show, Eq, Ord)

-- | Application screens
data Screen
    = MainMenuScreen
    | DrillScreen
    | TypingPracticeScreen
    | TypingLevelSelectScreen
    | ProgressScreen
    | StatsScreen
    | DayDetailScreen
    | HelpScreen
    | QuitConfirmScreen
    | LLMChatScreen
    | SettingsScreen
    | PersonaScreen
    | LookupScreen
    | VocabularyScreen
    deriving (Show, Eq)

-- | Mode of the drill exercise
data DrillMode
    = WritingMode       -- User types Korean translation
    | ReadingMode       -- User sees Korean, self-grades understanding
    | TypingMode        -- User types Korean with real-time feedback
    deriving (Show, Eq, Generic)

-- | State for an active drill session
data DrillState = DrillState
    { dsExercises :: [ExercisePrompt]    -- All exercises in the session
    , dsCurrentIndex :: Int               -- Current exercise index
    , dsUserInput :: Text                 -- Current user input
    , dsShowResult :: Maybe Bool          -- Nothing = not answered, Just = show result
    , dsCorrectCount :: Int               -- Running correct count
    , dsTotalCount :: Int                 -- Running total count
    , dsMode :: DrillMode                 -- Current drill mode
    , dsRevealAnswer :: Bool              -- For reading mode: reveal answer
    , dsCardStartTime :: Maybe UTCTime   -- When current card was shown (for time tracking)
    } deriving (Show, Eq, Generic)

-- | Create initial drill state
initialDrillState :: DrillMode -> [ExercisePrompt] -> DrillState
initialDrillState mode exercises = DrillState
    { dsExercises = exercises
    , dsCurrentIndex = 0
    , dsUserInput = ""
    , dsShowResult = Nothing
    , dsCorrectCount = 0
    , dsTotalCount = 0
    , dsMode = mode
    , dsRevealAnswer = False
    , dsCardStartTime = Nothing
    }

-- | Statistics for typing practice
data TypingStats = TypingStats
    { tsWPM :: Double              -- Words per minute
    , tsAccuracy :: Double         -- Accuracy percentage
    , tsStreak :: Int              -- Current correct streak
    , tsErrorMap :: Map Jamo Int   -- Error count per jamo
    , tsWordsCompleted :: Int      -- Total words completed
    , tsCorrectWords :: Int        -- Words typed correctly
    } deriving (Show, Eq, Generic)

-- | Empty typing stats
emptyTypingStats :: TypingStats
emptyTypingStats = TypingStats
    { tsWPM = 0
    , tsAccuracy = 100
    , tsStreak = 0
    , tsErrorMap = Map.empty
    , tsWordsCompleted = 0
    , tsCorrectWords = 0
    }

-- | Mode for level selection UI
data LevelSelectMode
    = TopLevelSelect      -- Viewing main levels (Level 1, 2, 3...)
    | SubLevelSelect Int  -- Viewing sub-levels of a parent level
    deriving (Show, Eq, Generic)

-- | State for typing practice mode
data TypingState = TypingState
    { typLevel :: TypingLevel               -- Current level
    , typExerciseType :: TypingExerciseType -- Type of exercise
    , typPrompts :: [TypingPrompt]          -- All prompts for session
    , typCurrentIndex :: Int                -- Current prompt index
    , typTypedJamo :: [Jamo]                -- Current input as jamo
    , typCharStatuses :: [CharStatus]       -- Per-character feedback
    , typStartTime :: Maybe UTCTime         -- When current word started
    , typStats :: TypingStats               -- Session statistics
    , typShowHints :: Bool                  -- Whether to show hints
    , typLastKeyCorrect :: Maybe Bool       -- Result of last keypress
    , typWords :: [TypingWord]              -- Available words for this level
    , typSelectedLevel :: Int               -- Level selection (for level select screen)
    , typLevelSelectMode :: LevelSelectMode -- Top-level or sub-level view
    , typSelectedIndex :: Int               -- Index in current level list
    , typSessionStart :: Maybe UTCTime      -- When this typing session began (for WPM/time)
    } deriving (Show, Eq, Generic)

-- | Create initial typing state
initialTypingState :: TypingLevel -> TypingExerciseType -> [TypingPrompt] -> [TypingWord] -> TypingState
initialTypingState level exType prompts twWords = TypingState
    { typLevel = level
    , typExerciseType = exType
    , typPrompts = prompts
    , typCurrentIndex = 0
    , typTypedJamo = []
    , typCharStatuses = []
    , typStartTime = Nothing
    , typStats = emptyTypingStats
    , typShowHints = True
    , typLastKeyCorrect = Nothing
    , typWords = twWords
    , typSelectedLevel = 1
    , typLevelSelectMode = TopLevelSelect
    , typSelectedIndex = 0
    , typSessionStart = Nothing
    }

-- | Session statistics for today's practice
data SessionStats = SessionStats
    { ssCardsReviewed :: Int      -- Cards reviewed this session
    , ssCorrectCount :: Int       -- Correct answers this session
    , ssNewCardsLearned :: Int    -- New cards encountered this session
    , ssSessionStart :: LocalTime -- When this session started
    } deriving (Show, Eq, Generic)

-- | Empty session stats (for new session)
emptySessionStats :: LocalTime -> SessionStats
emptySessionStats now = SessionStats
    { ssCardsReviewed = 0
    , ssCorrectCount = 0
    , ssNewCardsLearned = 0
    , ssSessionStart = now
    }

-- | A single message in an LLM conversation.
data LLMMessage = LLMMessage
    { llmRole      :: Text
    , llmContent   :: Text
    , llmIsSpoken  :: Bool  -- True = AI requested spoken/listening practice
    } deriving (Show, Eq, Generic)

-- | State for an active LLM chat session.
data LLMChatState = LLMChatState
    { llmTask         :: AgentTask
    , llmPersona      :: Persona
    , llmMessages     :: [LLMMessage]
    , llmInput        :: Text
    , llmWaiting      :: Bool         -- True while waiting for Ollama
    , llmPartial      :: Maybe Text   -- Nothing = pre-stream; Just t = streaming in progress
    , llmError        :: Maybe Text   -- Last error from Ollama, if any
    , llmInputMode    :: InputMode    -- Normal (navigate) or Insert (type)
    , llmShowKeyboard :: Bool         -- Whether to show the keyboard reference
    } deriving (Show, Eq, Generic)

initialLLMChatState :: AgentTask -> Persona -> LLMChatState
initialLLMChatState task persona = LLMChatState
    { llmTask         = task
    , llmPersona      = persona
    , llmMessages     = []
    , llmInput        = ""
    , llmWaiting      = True    -- AI sends the opening message immediately
    , llmPartial      = Nothing
    , llmError        = Nothing
    , llmInputMode    = NormalMode
    , llmShowKeyboard = True
    }

-- | Vim-style input mode for the chat screen.
data InputMode = NormalMode | InsertMode
    deriving (Show, Eq, Generic)

-- | Which persona field is being edited on the persona screen.
data PersonaEditField = PersonaEditName | PersonaEditStyle
    deriving (Show, Eq)

-- | Status of the dictionary lookup screen.
data LookupStatus
    = LookupIdle                      -- No lookup run yet
    | LookupLoading                   -- Request in flight
    | LookupResults [KrdictWord]      -- Fetched word entries
    | LookupError Text                -- Failure message
    deriving (Show, Eq)

-- | Live state of the dictionary lookup screen.
--
-- Two input modes, mirroring the chat screen: InsertMode types the query in
-- two-beolsik; NormalMode browses the results (j/k or the arrows move the
-- cursor, Space or Enter saves the highlighted word). 'lkSaved' tracks the
-- headwords saved this session so the UI can badge them as saved.
data LookupState = LookupState
    { lkInput       :: [Jamo]           -- ^ Jamo typed so far (two-beolsik order)
    , lkStatus      :: LookupStatus
    , lkNote        :: Maybe Text       -- ^ Feedback note (e.g. a save result)
    , lkMode        :: InputMode        -- ^ InsertMode = typing query; NormalMode = browsing
    , lkCursor      :: Int              -- ^ 0-based index of the highlighted result
    , lkSaved       :: [Text]           -- ^ Headwords saved this session (drives the badge)
    , lkSuggestions :: [Text]           -- ^ "Did you mean" words when a lookup finds nothing
    } deriving (Show, Eq)

-- | Fresh lookup state: empty input, ready to type.
initialLookupState :: LookupState
initialLookupState = LookupState
    { lkInput       = []
    , lkStatus      = LookupIdle
    , lkNote        = Nothing
    , lkMode        = InsertMode
    , lkCursor      = 0
    , lkSaved       = []
    , lkSuggestions = []
    }

-- | The currently typed word, composed in real time from the jamo.
lookupQuery :: LookupState -> Text
lookupQuery = composeJamoSequence . lkInput

-- | The words currently on screen (empty unless the status holds a result set).
lookupResults :: LookupState -> [KrdictWord]
lookupResults s = case lkStatus s of
    LookupResults w -> w
    _               -> []

-- | Append one jamo to the input.
applyJamo :: LookupState -> Jamo -> LookupState
applyJamo s j = s { lkInput = lkInput s ++ [j] }

-- | Remove the last jamo from the input (no-op when empty).
popJamo :: LookupState -> LookupState
popJamo s
    | null (lkInput s)  = s
    | otherwise         = s { lkInput = init (lkInput s) }

-- | Enter is pressed while typing: reset to loading, drop any stale note and
-- start selection over for the next result set.
submitLookup :: LookupState -> LookupState
submitLookup s = s
    { lkStatus      = LookupLoading
    , lkNote        = Nothing
    , lkCursor      = 0
    , lkSaved       = []
    , lkSuggestions = []
    }

-- | Replace the input with `target` (decomposed back into jamo) and reset the
-- per-lookup transient fields, so a caller can re-submit the lookup for that
-- word. Used to adopt a "did you mean" suggestion.
retargetLookup :: Text -> LookupState -> LookupState
retargetLookup target s = s
    { lkInput       = decomposeKorean target
    , lkStatus      = LookupIdle
    , lkNote        = Nothing
    , lkMode        = InsertMode
    , lkCursor      = 0
    , lkSaved       = []
    , lkSuggestions = []
    }

-- | Apply a finished lookup outcome, switching to NormalMode so the user can
-- browse and save the results (or read the error / suggestion).
applyLookupOutcome :: Either Text [KrdictWord] -> LookupState -> LookupState
applyLookupOutcome (Right wds) s =
    s { lkStatus = LookupResults wds, lkNote = Nothing, lkMode = NormalMode, lkCursor = 0, lkSaved = [] }
applyLookupOutcome (Left err)   s =
    s { lkStatus = LookupError err,   lkNote = Nothing, lkMode = NormalMode, lkCursor = 0, lkSaved = [] }

-- | Move the highlight by 'delta' (pass +1 for j / down, -1 for k / up),
-- clamped to the visible results. A no-op when there is no result set.
moveCursor :: Int -> LookupState -> LookupState
moveCursor delta s
    | null rs   = s
    | otherwise = s { lkCursor = clamp 0 (n - 1) (lkCursor s + delta) }
  where
    rs    = lookupResults s
    n     = length rs
    clamp lo hi x = max lo (min hi x)

-- | Whether a result's headword was already saved this session (drives the badge).
isSavedWord :: LookupState -> KrdictWord -> Bool
isSavedWord s w = kwWord w `elem` lkSaved s

-- | Record the highlighted word as saved and advance the cursor to the next
-- result (clamped at the end). Returns the saved word so the caller can
-- persist it. A no-op when there is no result set.
saveHighlighted :: LookupState -> (LookupState, Maybe KrdictWord)
saveHighlighted s
    | null rs   = (s, Nothing)
    | otherwise =
        let i      = max 0 (min (n - 1) (lkCursor s))
            w      = rs !! i
            saved' = if kwWord w `elem` lkSaved s
                         then lkSaved s
                         else lkSaved s ++ [kwWord w]
        in ( s { lkSaved  = saved'
              , lkCursor = min (n - 1) (i + 1)
              }
           , Just w
           )
  where
    rs = lookupResults s
    n  = length rs

-- | Custom events for the application
data AppEvent
    = Tick
    | LLMResponse   (Either Text Text)    -- Async reply from lesson AI (non-streaming, kept for compat)
    | LLMToken      Text                  -- Incremental streaming token from Ollama
    | LLMStreamDone Text                  -- Stream complete; carries full accumulated text
    | LLMStreamError Text                 -- Stream failed; carries error message
    | LLMExtraction [ExtractedWord]       -- Vocabulary extracted from last turn
    | LLMSummary    Text                  -- Session summary from summarization agent
    | KrdictLookupDone (Either Text [KrdictWord])  -- Async dictionary lookup result
    deriving (Show, Eq)

-- | Main application state
data AppState = AppState
    { asScreen :: Screen
    , asDrill :: Maybe DrillState
    , asTyping :: Maybe TypingState         -- Typing practice state
    , asMenuIndex :: Int                    -- Selected menu item
    , asVocabCards :: [VocabularyCard]      -- Loaded vocabulary
    , asProgress :: Maybe UserProgress      -- User's progress (legacy)
    , asMessage :: Maybe Text               -- Status message
    , asTypingWords :: [TypingWord]         -- Typing vocabulary words
    , asTypingProgress :: TypingProgress    -- Typing level completion status
    -- SRS-related state
    , asVocabStates :: Map VocabularyId VocabularyState  -- Live SRS states
    , asDueCards :: [VocabularyId]          -- Cards due for review
    , asSessionStats :: Maybe SessionStats  -- Today's session stats
    , asDailyStreak :: Int                  -- Current streak in days
    , asActivities :: [ActivityEntry]       -- Full activity log
    , asStatsSelectedDay :: Int             -- Selected day index in stats screen
    , asVocabCursor :: Int                  -- Selected index in the vocabulary screen
    -- LLM / AI lessons
    , asLLMChat       :: Maybe LLMChatState             -- Active AI lesson chat
    , asSettings      :: AppSettings                    -- Language, level, Ollama config
    , asPersonaEdit   :: Maybe (PersonaEditField, Text) -- Active persona field edit
    , asStudentProfile :: Maybe StudentProfile          -- Cumulative student brief
    , asLookup :: Maybe LookupState                      -- Active dictionary lookup session
    } deriving (Show, Eq)

-- | Initial application state
initialAppState :: AppState
initialAppState = AppState
    { asScreen = MainMenuScreen
    , asDrill = Nothing
    , asTyping = Nothing
    , asMenuIndex = 0
    , asVocabCards = []
    , asProgress = Nothing
    , asMessage = Nothing
    , asTypingWords = []
    , asTypingProgress = emptyTypingProgress
    -- SRS defaults
    , asVocabStates = Map.empty
    , asDueCards = []
    , asSessionStats = Nothing
    , asDailyStreak = 0
    , asActivities = []
    , asStatsSelectedDay = 0
    , asVocabCursor = 0
    -- LLM defaults
    , asLLMChat        = Nothing
    , asSettings       = defaultSettings
    , asPersonaEdit    = Nothing
    , asStudentProfile = Nothing
    , asLookup         = Nothing
    }

-- | Attribute names for styling
titleAttr :: AttrName
titleAttr = attrName "title"

menuAttr :: AttrName
menuAttr = attrName "menu"

menuSelectedAttr :: AttrName
menuSelectedAttr = attrName "menuSelected"

promptAttr :: AttrName
promptAttr = attrName "prompt"

inputAttr :: AttrName
inputAttr = attrName "input"

correctAttr :: AttrName
correctAttr = attrName "correct"

incorrectAttr :: AttrName
incorrectAttr = attrName "incorrect"

hintAttr :: AttrName
hintAttr = attrName "hint"

statsAttr :: AttrName
statsAttr = attrName "stats"

keyboardAttr :: AttrName
keyboardAttr = attrName "keyboard"

keyHighlightAttr :: AttrName
keyHighlightAttr = attrName "keyHighlight"

keyDisabledAttr :: AttrName
keyDisabledAttr = attrName "keyDisabled"

aiAttr :: AttrName
aiAttr = attrName "ai"
