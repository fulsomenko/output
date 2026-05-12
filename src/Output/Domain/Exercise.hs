{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Output.Domain.Exercise
    ( ExercisePrompt(..)
    , ExerciseResult(..)
    , generateExercisePrompt
    , checkAnswer
    , normalizeAnswer
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Output.Domain.Types
    ( ExerciseType(..)
    , VocabularyCard(..)
    , VocabularyId
    )

-- | An exercise prompt presented to the user
data ExercisePrompt = ExercisePrompt
    { epVocabId :: VocabularyId
    , epExerciseType :: ExerciseType
    , epQuestion :: Text                  -- What to show the user
    , epExpectedAnswer :: Text            -- The correct answer
    , epHint :: Maybe Text                -- Optional hint
    , epAlternatives :: [Text]            -- Alternative correct answers
    , epExampleSentences :: [Text]        -- Example Korean sentences using this word
    , epExampleTranslations :: [Text]     -- Parallel English translations
    } deriving (Show, Eq, Generic)

instance FromJSON ExercisePrompt
instance ToJSON ExercisePrompt

-- | Result of a completed exercise
data ExerciseResult = ExerciseResult
    { erVocabId :: VocabularyId
    , erExerciseType :: ExerciseType
    , erCorrect :: Bool
    , erUserAnswer :: Text
    , erExpectedAnswer :: Text
    , erTimeTaken :: NominalDiffTime  -- Time in seconds
    , erWordsPerMinute :: Maybe Double  -- For typing exercises
    } deriving (Show, Eq, Generic)

instance FromJSON ExerciseResult
instance ToJSON ExerciseResult

-- | Generate an exercise prompt from a vocabulary card
generateExercisePrompt :: ExerciseType -> VocabularyCard -> ExercisePrompt
generateExercisePrompt exType card = case exType of
    Reading -> ExercisePrompt
        { epVocabId = vocabId card
        , epExerciseType = Reading
        , epQuestion = korean card  -- Show Korean
        , epExpectedAnswer = primaryEnglish  -- User thinks of English
        , epHint = Just $ romanization card
        , epAlternatives = english card
        , epExampleSentences = exampleSentences card
        , epExampleTranslations = exampleTranslations card
        }
    Writing -> ExercisePrompt
        { epVocabId = vocabId card
        , epExerciseType = Writing
        , epQuestion = primaryEnglish  -- Show English
        , epExpectedAnswer = korean card  -- User writes Korean
        , epHint = Just $ romanization card
        , epAlternatives = []
        , epExampleSentences = exampleSentences card
        , epExampleTranslations = exampleTranslations card
        }
    Typing -> ExercisePrompt
        { epVocabId = vocabId card
        , epExerciseType = Typing
        , epQuestion = korean card  -- Show Korean to type
        , epExpectedAnswer = korean card  -- User types Korean
        , epHint = Nothing
        , epAlternatives = []
        , epExampleSentences = exampleSentences card
        , epExampleTranslations = exampleTranslations card
        }
  where
    primaryEnglish = case english card of
        (e:_) -> e
        []    -> ""

-- | Check if a user's answer is correct
-- Handles normalization and alternative answers
checkAnswer :: ExercisePrompt -> Text -> Bool
checkAnswer prompt userAnswer =
    let normalized = normalizeAnswer userAnswer
        expected = normalizeAnswer (epExpectedAnswer prompt)
        alternatives = map normalizeAnswer (epAlternatives prompt)
    in normalized == expected || normalized `elem` alternatives

-- | Normalize an answer for comparison
-- Removes whitespace, lowercases, removes punctuation
normalizeAnswer :: Text -> Text
normalizeAnswer = T.strip . T.toLower . removePunctuation
  where
    removePunctuation = T.filter (not . isPunctuation)
    isPunctuation c = c `elem` (".,!?;:\"'()-" :: String)
