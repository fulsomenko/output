{-# LANGUAGE DeriveGeneric #-}

-- | SM-2 Spaced Repetition Algorithm implementation.
-- This module implements the classic SuperMemo SM-2 algorithm
-- and provides an instance of the SRSAlgorithm typeclass.
module Output.Algorithm.SpacedRepetition
    ( -- * SM-2 Algorithm
      SM2(..)
    , defaultSM2
    , applySRSResult
      -- * Legacy Functions (for backward compatibility)
    , calculateNextReviewDate
    , calculateIntervalDays
    , updateMasteryLevel
    ) where

import Data.Time (LocalTime, localDay, addDays)
import Data.Time (LocalTime(..))
import GHC.Generics (Generic)

import Output.Domain.Types
    ( VocabularyId(..)
    , VocabularyState(..)
    , MasteryLevel(..)
    , defaultEaseFactor
    , newVocabularyState
    )
import Output.Algorithm.SRS
    ( SRSAlgorithm(..)
    , SRSResult(..)
    , Quality(..)
    , qualityToDouble
    )

-- | SM-2 algorithm configuration.
-- Currently has no configurable parameters, but structured for future extensibility.
data SM2 = SM2
    { sm2MinEaseFactor :: Double  -- Minimum ease factor (default 1.3)
    } deriving (Show, Eq, Generic)

-- | Default SM-2 configuration
defaultSM2 :: SM2
defaultSM2 = SM2
    { sm2MinEaseFactor = 1.3
    }

-- | SM-2 implementation of SRSAlgorithm
instance SRSAlgorithm SM2 where
    calculateReview algo state quality now =
        let q = qualityToDouble quality
            oldEF = vstEaseFactor state
            oldInterval = vstCurrentInterval state
            reviewCount = vstReviewCount state + 1
            correctCount = if quality >= Hard
                           then vstCorrectCount state + 1
                           else vstCorrectCount state

            -- SM-2 ease factor formula
            -- EF' = EF + (0.1 - (5 - q) * (0.08 + (5 - q) * 0.02))
            newEF = max (sm2MinEaseFactor algo) $
                    oldEF + (0.1 - (5 - q) * (0.08 + (5 - q) * 0.02))

            -- Interval calculation based on quality
            newInterval = case quality of
                Again -> 1  -- Reset to 1 day on failure
                Hard  -> max 1 (ceiling (fromIntegral oldInterval * 1.2))
                Good  -> if oldInterval == 0
                         then 1
                         else if oldInterval == 1
                              then 6
                              else ceiling (fromIntegral oldInterval * newEF)
                Easy  -> if oldInterval == 0
                         then 4
                         else ceiling (fromIntegral oldInterval * newEF * 1.3)

            -- Calculate next review date
            nextReview = addDaysToLocalTime newInterval now

            -- Update mastery level
            correctRate = fromIntegral correctCount / fromIntegral reviewCount
            mastery = calculateMasteryLevel reviewCount correctRate

        in SRSResult
            { srsNextReviewDate = nextReview
            , srsNewInterval = newInterval
            , srsNewEaseFactor = newEF
            , srsNewMasteryLevel = mastery
            }

    initializeCard _algo vid now = newVocabularyState vid now

-- | Apply SRS result to a vocabulary state
applySRSResult :: VocabularyState -> Quality -> LocalTime -> SRSResult -> VocabularyState
applySRSResult state quality now result = state
    { vstMasteryLevel = srsNewMasteryLevel result
    , vstLastReviewDate = Just now
    , vstNextReviewDate = srsNextReviewDate result
    , vstReviewCount = vstReviewCount state + 1
    , vstCorrectCount = if quality >= Hard
                        then vstCorrectCount state + 1
                        else vstCorrectCount state
    , vstIncorrectCount = if quality == Again
                          then vstIncorrectCount state + 1
                          else vstIncorrectCount state
    , vstEaseFactor = srsNewEaseFactor result
    , vstCurrentInterval = srsNewInterval result
    }

-- | Helper to add days to LocalTime
addDaysToLocalTime :: Integer -> LocalTime -> LocalTime
addDaysToLocalTime days lt = lt { localDay = addDays days (localDay lt) }

-- | Calculate mastery level from review stats
calculateMasteryLevel :: Int -> Double -> MasteryLevel
calculateMasteryLevel reviewCount correctRate
    | reviewCount == 0 = New
    | reviewCount < 3 = Learning
    | correctRate >= 0.85 = Mastered
    | correctRate >= 0.65 = Intermediate
    | otherwise = Learning

--------------------------------------------------------------------------------
-- Legacy functions for backward compatibility
--------------------------------------------------------------------------------

-- | Calculate the next review date based on performance
-- Legacy function - prefer using SRSAlgorithm typeclass
calculateNextReviewDate :: LocalTime -> Int -> Int -> LocalTime
calculateNextReviewDate lastReviewDate totalReviews correctReviews
    | totalReviews == 0 =
        let day = localDay lastReviewDate
            newDay = addDays 1 day
        in lastReviewDate { localDay = newDay }
    | otherwise =
        let correctRate = fromIntegral correctReviews / fromIntegral totalReviews :: Double
            interval = calculateIntervalDays totalReviews correctRate
            day = localDay lastReviewDate
            newDay = addDays interval day
        in lastReviewDate { localDay = newDay }

-- | Calculate interval using SM-2 algorithm
-- Legacy function - prefer using SRSAlgorithm typeclass
calculateIntervalDays :: Int -> Double -> Integer
calculateIntervalDays reviewCount correctRate
    | reviewCount < 1 = 1
    | reviewCount == 1 && correctRate >= 0.7 = 3
    | reviewCount == 1 && correctRate < 0.7 = 1
    | otherwise =
        let easeFactor = 2.5 - (5 - quality) * (0.08 + (5 - quality) * 0.02)
            baseInterval = if reviewCount <= 1 then 1 else 3
            quality = min 5.0 (max 0.0 (correctRate * 5))
        in max 1 (ceiling (fromIntegral baseInterval * easeFactor))

-- | Determine mastery level based on correct/total reviews
-- Legacy function - prefer using SRSAlgorithm typeclass
updateMasteryLevel :: Int -> Int -> MasteryLevel
updateMasteryLevel totalReviews correctReviews
    | totalReviews == 0 = New
    | totalReviews < 3 = Learning
    | correctRate >= 0.85 = Mastered
    | correctRate >= 0.65 = Intermediate
    | otherwise = Learning
  where
    correctRate = fromIntegral correctReviews / fromIntegral totalReviews :: Double
