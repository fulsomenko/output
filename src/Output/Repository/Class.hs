{-# LANGUAGE DeriveGeneric #-}

module Output.Repository.Class
    ( ActivityRepository(..)
    , VocabularyRepository(..)
    , UserProgressRepository(..)
    , TypingProgressRepository(..)
    ) where

import Data.Time (LocalTime)
import Data.Map (Map)
import Output.Domain.Types
    ( VocabularyId
    , VocabularyCard
    , VocabularyState
    , UserProgress
    , TypingProgress
    , TOPIK_Level
    )
import Output.Domain.Activity (ActivityEntry)

-- | Repository for activity logging
class Monad m => ActivityRepository m where
    -- | Log a new activity entry
    logActivity :: ActivityEntry -> m ()

    -- | Get all activities for a specific date
    getActivitiesByDate :: LocalTime -> m [ActivityEntry]

    -- | Get all activities for a specific vocabulary item
    getActivitiesByVocab :: VocabularyId -> m [ActivityEntry]

    -- | Get all activities in a date range
    getActivitiesInRange :: LocalTime -> LocalTime -> m [ActivityEntry]

    -- | Get all activities (careful with large datasets)
    getAllActivities :: m [ActivityEntry]

-- | Repository for vocabulary data
class Monad m => VocabularyRepository m where
    -- | Save or update vocabulary state
    saveVocabState :: VocabularyState -> m ()

    -- | Get vocabulary state by ID
    getVocabState :: VocabularyId -> m (Maybe VocabularyState)

    -- | Get all vocabulary states
    getAllVocabStates :: m (Map VocabularyId VocabularyState)

    -- | Get vocabulary cards for a specific TOPIK level
    getVocabCardsForLevel :: TOPIK_Level -> m [VocabularyCard]

    -- | Get all vocabulary cards
    getAllVocabCards :: m [VocabularyCard]

    -- | Get words that are due for review (nextReviewDate <= now)
    getWordsForReview :: LocalTime -> m [VocabularyId]

    -- | Save a vocabulary card
    saveVocabCard :: VocabularyCard -> m ()

-- | Repository for user progress
class Monad m => UserProgressRepository m where
    -- | Save user progress
    saveProgress :: UserProgress -> m ()

    -- | Get current user progress
    getProgress :: m UserProgress

-- | Repository for typing practice progress
class Monad m => TypingProgressRepository m where
    -- | Get current typing progress
    getTypingProgress :: m TypingProgress

    -- | Save typing progress
    saveTypingProgress :: TypingProgress -> m ()

    -- | Mark a level as completed
    markLevelCompleted :: Int -> m ()
