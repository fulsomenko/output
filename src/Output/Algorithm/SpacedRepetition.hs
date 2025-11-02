{-# LANGUAGE DeriveGeneric #-}

module Output.Algorithm.SpacedRepetition
    ( calculateNextReviewDate
    , calculateIntervalDays
    , updateMasteryLevel
    ) where

import Data.Time (LocalTime, localDay, addDays, TimeOfDay(..))
import Data.Time (LocalTime(..))
import Output.Domain.Types (MasteryLevel(..))
import Output.Domain.Activity (Performance(..))

-- | SM-2 algorithm parameters
data SM2Params = SM2Params
    { sm2QFactor :: Double           -- Quality factor (0-5)
    , sm2EaseFactor :: Double        -- Ease factor (initial 2.5)
    , sm2PreviousInterval :: Integer -- Previous interval in days
    }

-- | Calculate the next review date based on performance
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
updateMasteryLevel :: Int -> Int -> MasteryLevel
updateMasteryLevel totalReviews correctReviews
    | totalReviews == 0 = New
    | totalReviews < 3 = Learning
    | correctRate >= 0.85 = Mastered
    | correctRate >= 0.65 = Intermediate
    | otherwise = Learning
  where
    correctRate = fromIntegral correctReviews / fromIntegral totalReviews :: Double

-- | Simple quality factor from performance
performanceToQuality :: Performance -> Double
performanceToQuality perf =
    let accuracy = case perfAccuracy perf of
            x | x >= 90 -> 5.0
              | x >= 80 -> 4.0
              | x >= 70 -> 3.0
              | x >= 60 -> 2.0
              | otherwise -> 1.0
    in accuracy
