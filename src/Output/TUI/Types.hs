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
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Brick (AttrName, attrName)
import GHC.Generics (Generic)

import Output.Domain.Types
import Output.Domain.Exercise (ExercisePrompt)

-- | Widget names for focus management
data Name
    = MenuList
    | ExerciseInput
    | ProgressView
    | HelpView
    deriving (Show, Eq, Ord)

-- | Application screens
data Screen
    = MainMenuScreen
    | DrillScreen
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

-- | Custom events for the application
data AppEvent
    = Tick  -- For timers if needed
    deriving (Show, Eq)

-- | Main application state
data AppState = AppState
    { asScreen :: Screen
    , asDrill :: Maybe DrillState
    , asMenuIndex :: Int                  -- Selected menu item
    , asVocabCards :: [VocabularyCard]    -- Loaded vocabulary
    , asProgress :: Maybe UserProgress    -- User's progress
    , asMessage :: Maybe Text             -- Status message
    } deriving (Show, Eq)

-- | Initial application state
initialAppState :: AppState
initialAppState = AppState
    { asScreen = MainMenuScreen
    , asDrill = Nothing
    , asMenuIndex = 0
    , asVocabCards = []
    , asProgress = Nothing
    , asMessage = Nothing
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
