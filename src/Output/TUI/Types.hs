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
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (UTCTime)
import Brick (AttrName, attrName)
import GHC.Generics (Generic)

import Output.Domain.Types
import Output.Domain.Exercise (ExercisePrompt)
import Output.Domain.Jamo (Jamo)
import Output.Domain.TypingLevel (TypingLevel)
import Output.Domain.TypingWord (TypingWord)
import Output.Domain.TypingExercise (TypingExerciseType, TypingPrompt, CharStatus)

-- | Widget names for focus management
data Name
    = MenuList
    | ExerciseInput
    | ProgressView
    | HelpView
    | TypingInput
    | KeyboardView
    | LevelSelector
    deriving (Show, Eq, Ord)

-- | Application screens
data Screen
    = MainMenuScreen
    | DrillScreen
    | TypingPracticeScreen
    | TypingLevelSelectScreen
    | ProgressScreen
    | HelpScreen
    | QuitConfirmScreen
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
    } deriving (Show, Eq, Generic)

-- | Create initial typing state
initialTypingState :: TypingLevel -> TypingExerciseType -> [TypingPrompt] -> [TypingWord] -> TypingState
initialTypingState level exType prompts words = TypingState
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
    , typWords = words
    , typSelectedLevel = 1
    , typLevelSelectMode = TopLevelSelect
    , typSelectedIndex = 0
    }

-- | Custom events for the application
data AppEvent
    = Tick  -- For timers if needed
    deriving (Show, Eq)

-- | Main application state
data AppState = AppState
    { asScreen :: Screen
    , asDrill :: Maybe DrillState
    , asTyping :: Maybe TypingState       -- Typing practice state
    , asMenuIndex :: Int                  -- Selected menu item
    , asVocabCards :: [VocabularyCard]    -- Loaded vocabulary
    , asProgress :: Maybe UserProgress    -- User's progress
    , asMessage :: Maybe Text             -- Status message
    , asTypingWords :: [TypingWord]       -- Typing vocabulary words
    , asTypingProgress :: TypingProgress  -- Typing level completion status
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
