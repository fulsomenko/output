{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Output.Domain.TypingExercise
    ( -- * Exercise Types
      TypingExerciseType(..)
    , TypingPrompt(..)
    , TypingResult(..)
    , CharStatus(..)
      -- * Creating Prompts
    , createTypingPrompt
    , createSessionPrompts
      -- * Validation
    , validateTyping
    , validateCharacter
    , isWordComplete
    , isWordCorrect
      -- * Metrics
    , calculateWPM
    , calculateAccuracy
    , createResult
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Output.Domain.Jamo
import Output.Domain.TypingWord

-- | Types of typing exercises
data TypingExerciseType
    = Echo       -- Korean → Korean (pure muscle memory)
    | Build      -- English → Korean (vocabulary + typing)
    | Drill      -- Timed rapid-fire sequences
    | Sentence   -- Full sentences
    deriving (Eq, Show, Generic, Enum, Bounded)

instance FromJSON TypingExerciseType
instance ToJSON TypingExerciseType

-- | A single typing prompt
data TypingPrompt = TypingPrompt
    { tpWord :: TypingWord            -- The word to type
    , tpExerciseType :: TypingExerciseType
    , tpExpectedJamo :: [Jamo]        -- Expected keystroke sequence
    , tpHint :: Maybe Text            -- Optional romanization hint
    , tpShowKorean :: Bool            -- Whether to show Korean text
    } deriving (Eq, Show, Generic)

instance FromJSON TypingPrompt
instance ToJSON TypingPrompt

-- | Status of a single character
data CharStatus
    = Correct     -- Character typed correctly
    | Incorrect   -- Character typed incorrectly
    | Pending     -- Not yet typed
    deriving (Eq, Show, Generic, Enum, Bounded)

instance FromJSON CharStatus
instance ToJSON CharStatus

-- | Result of a typing exercise
data TypingResult = TypingResult
    { trCorrect :: Bool
    , trTypedJamo :: [Jamo]
    , trExpectedJamo :: [Jamo]
    , trWPM :: Double
    , trAccuracy :: Double
    , trTimeMs :: Int
    , trErrors :: [(Int, Jamo, Jamo)]  -- (position, expected, actual)
    } deriving (Eq, Show, Generic)

instance FromJSON TypingResult
instance ToJSON TypingResult

-- | Create a typing prompt from a word
createTypingPrompt :: TypingExerciseType -> TypingWord -> TypingPrompt
createTypingPrompt exType word = TypingPrompt
    { tpWord = word
    , tpExerciseType = exType
    , tpExpectedJamo = twJamo word
    , tpHint = case exType of
        Echo -> Nothing  -- No hints for echo
        Build -> Just (twRomanization word)
        Drill -> Nothing
        Sentence -> Just (twRomanization word)
    , tpShowKorean = case exType of
        Echo -> True     -- Show Korean, type it
        Build -> False   -- Show English, type Korean
        Drill -> True    -- Show Korean, type fast
        Sentence -> True
    }

-- | Create a list of prompts for a session
createSessionPrompts :: TypingExerciseType -> [TypingWord] -> [TypingPrompt]
createSessionPrompts exType = map (createTypingPrompt exType)

-- | Validate typing progress, returning status for each expected jamo
validateTyping :: [Jamo] -> [Jamo] -> [CharStatus]
validateTyping expected typed = zipStatuses 0 expected typed
  where
    zipStatuses _ [] _ = []
    zipStatuses idx (e:es) [] = Pending : zipStatuses (idx + 1) es []
    zipStatuses idx (e:es) (t:ts)
        | e == t    = Correct : zipStatuses (idx + 1) es ts
        | otherwise = Incorrect : zipStatuses (idx + 1) es ts

-- | Validate a single character at position
validateCharacter :: [Jamo] -> Int -> Jamo -> CharStatus
validateCharacter expected pos typed
    | pos >= length expected = Incorrect
    | expected !! pos == typed = Correct
    | otherwise = Incorrect

-- | Check if word is complete (all characters typed)
isWordComplete :: [Jamo] -> [Jamo] -> Bool
isWordComplete expected typed = length typed >= length expected

-- | Check if word is completely correct
isWordCorrect :: [Jamo] -> [Jamo] -> Bool
isWordCorrect expected typed = expected == typed

-- | Calculate words per minute
-- Using standard: 5 characters = 1 word
calculateWPM :: Int -> Int -> Double
calculateWPM charCount timeMs
    | timeMs <= 0 = 0
    | otherwise = (fromIntegral charCount / 5.0) / (fromIntegral timeMs / 60000.0)

-- | Calculate accuracy percentage
calculateAccuracy :: Int -> Int -> Double
calculateAccuracy correct total
    | total <= 0 = 100.0
    | otherwise = (fromIntegral correct / fromIntegral total) * 100.0

-- | Create a result from typed input
createResult :: [Jamo] -> [Jamo] -> Int -> TypingResult
createResult expected typed timeMs = TypingResult
    { trCorrect = expected == typed
    , trTypedJamo = typed
    , trExpectedJamo = expected
    , trWPM = calculateWPM (length expected) timeMs
    , trAccuracy = calculateAccuracy correctCount (length expected)
    , trTimeMs = timeMs
    , trErrors = errors
    }
  where
    statuses = validateTyping expected typed
    correctCount = length $ filter (== Correct) statuses
    errors = [ (i, e, t)
             | (i, (e, t, s)) <- zip [0..] (zip3 expected typed statuses)
             , s == Incorrect
             ]
