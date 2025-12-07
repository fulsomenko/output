{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Output.Domain.Types
    ( TOPIK_Level(..)
    , VocabularyId(..)
    , VocabularyCard(..)
    , ExerciseType(..)
    , MasteryLevel(..)
    , VocabularyState(..)
    , UserProgress(..)
      -- * Typing Progress
    , TypingProgress(..)
    , emptyTypingProgress
    ) where

import Data.Text (Text)
import Data.Time (LocalTime)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Aeson (FromJSON(..), ToJSON(..), FromJSONKey, ToJSONKey, object, withObject, (.:), (.=))
import GHC.Generics (Generic)

-- | TOPIK proficiency levels
data TOPIK_Level = One | Two | Three | Four | Five | Six
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON TOPIK_Level
instance ToJSON TOPIK_Level

-- | Unique identifier for vocabulary cards
newtype VocabularyId = VocabularyId Int
    deriving (Eq, Ord, Show, Generic)

instance FromJSON VocabularyId
instance ToJSON VocabularyId
instance FromJSONKey VocabularyId
instance ToJSONKey VocabularyId

-- | A Korean vocabulary card with context
data VocabularyCard = VocabularyCard
    { vocabId :: VocabularyId
    , korean :: Text
    , romanization :: Text  -- Romanized form (hangeul as text)
    , english :: [Text]     -- Multiple English translations
    , topicLevel :: TOPIK_Level
    , exampleSentences :: [Text]  -- Example sentences in Korean
    , exampleTranslations :: [Text]  -- Parallel translations
    } deriving (Show, Eq, Generic)

instance FromJSON VocabularyCard
instance ToJSON VocabularyCard

-- | Types of exercises users can complete
data ExerciseType = Reading | Writing | Typing
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON ExerciseType
instance ToJSON ExerciseType

-- | Mastery levels for a vocabulary word
data MasteryLevel = New | Learning | Intermediate | Mastered
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON MasteryLevel
instance ToJSON MasteryLevel

-- | Current state of a single vocabulary word
data VocabularyState = VocabularyState
    { vstVocabId :: VocabularyId
    , vstMasteryLevel :: MasteryLevel
    , vstLastReviewDate :: Maybe LocalTime
    , vstNextReviewDate :: LocalTime
    , vstReviewCount :: Int
    , vstCorrectCount :: Int
    , vstIncorrectCount :: Int
    } deriving (Show, Eq, Generic)

instance FromJSON VocabularyState
instance ToJSON VocabularyState

-- | Overall user progress and state
data UserProgress = UserProgress
    { upCurrentLevel :: TOPIK_Level
    , upDailyStreak :: Int
    , upLastActivityDate :: Maybe LocalTime
    , upTotalWordsLearned :: Int
    , upTotalWordsReviewed :: Int
    , upVocabularyStates :: Map VocabularyId VocabularyState
    } deriving (Show, Eq, Generic)

instance FromJSON UserProgress
instance ToJSON UserProgress

-- | Progress tracking for typing practice levels
data TypingProgress = TypingProgress
    { tpCompletedLevels :: Set Int  -- Set of level numbers completed at least once
    } deriving (Show, Eq, Generic)

-- | Custom JSON instance for extensible format:
-- { "typingPractice": { "completedLevels": [11, 12, 13] } }
instance ToJSON TypingProgress where
    toJSON progress = object
        [ "typingPractice" .= object
            [ "completedLevels" .= Set.toList (tpCompletedLevels progress)
            ]
        ]

instance FromJSON TypingProgress where
    parseJSON = withObject "TypingProgress" $ \v -> do
        tp <- v .: "typingPractice"
        levels <- tp .: "completedLevels"
        pure $ TypingProgress (Set.fromList levels)

-- | Empty typing progress (nothing completed)
emptyTypingProgress :: TypingProgress
emptyTypingProgress = TypingProgress Set.empty
