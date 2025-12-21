{-# LANGUAGE DeriveGeneric #-}

-- | Pluggable Spaced Repetition System interface.
-- This module provides a typeclass abstraction for SRS algorithms,
-- allowing the app to use SM-2 now and switch to FSRS or other
-- algorithms in the future.
module Output.Algorithm.SRS
    ( -- * Quality of Recall
      Quality(..)
    , qualityToDouble
    , ratingToQuality
      -- * SRS Result
    , SRSResult(..)
      -- * Algorithm Typeclass
    , SRSAlgorithm(..)
    ) where

import Data.Time (LocalTime)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import Output.Domain.Types (VocabularyId, VocabularyState, MasteryLevel)

-- | Quality of recall rating.
-- Simplified from SM-2's 0-5 scale to 4 intuitive levels.
data Quality
    = Again     -- Complete failure, reset interval
    | Hard      -- Correct but with difficulty
    | Good      -- Correct with normal effort
    | Easy      -- Correct with no effort
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON Quality
instance ToJSON Quality

-- | Convert Quality to numeric value (0-5 scale for SM-2 compatibility)
qualityToDouble :: Quality -> Double
qualityToDouble Again = 0
qualityToDouble Hard  = 2
qualityToDouble Good  = 4
qualityToDouble Easy  = 5

-- | Convert 1-4 reading drill rating to Quality
-- 1-2 = failed recall, 3-4 = successful recall
ratingToQuality :: Int -> Quality
ratingToQuality 1 = Again
ratingToQuality 2 = Hard
ratingToQuality 3 = Good
ratingToQuality 4 = Easy
ratingToQuality _ = Good  -- Default

-- | Result of an SRS calculation.
-- Contains all the updated state after processing a review.
data SRSResult = SRSResult
    { srsNextReviewDate :: LocalTime    -- When to next review this card
    , srsNewInterval :: Integer         -- New interval in days
    , srsNewEaseFactor :: Double        -- Updated ease factor
    , srsNewMasteryLevel :: MasteryLevel -- Updated mastery classification
    } deriving (Show, Eq, Generic)

-- | Typeclass for SRS algorithms.
-- Implement this for SM-2, FSRS, or any other spaced repetition algorithm.
class SRSAlgorithm algo where
    -- | Calculate the next review based on recall quality.
    -- Takes the algorithm config, current card state, quality of recall,
    -- and current time; returns the updated SRS parameters.
    calculateReview
        :: algo
        -> VocabularyState  -- Current card state
        -> Quality          -- How well the user recalled
        -> LocalTime        -- Current time
        -> SRSResult

    -- | Initialize SRS state for a new card.
    -- Called when a card is seen for the first time.
    initializeCard
        :: algo
        -> VocabularyId     -- Card identifier
        -> LocalTime        -- Current time
        -> VocabularyState  -- Initial state with default SRS values
