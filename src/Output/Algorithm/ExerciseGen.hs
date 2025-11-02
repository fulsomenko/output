{-# LANGUAGE DeriveGeneric #-}

module Output.Algorithm.ExerciseGen
    ( generateDailyExercises
    , selectExercisesForDay
    ) where

import Data.Map (Map, elems, filterWithKey)
import Data.Time (LocalTime)
import Data.List (sortBy)
import Data.Ord (comparing)
import Output.Domain.Types
    ( VocabularyId
    , VocabularyState(..)
    , ExerciseType(..)
    , MasteryLevel(..)
    )

-- | Generate a balanced set of exercises for the day
generateDailyExercises :: LocalTime -> Map VocabularyId VocabularyState -> (Int, Int, Int)
generateDailyExercises today vocabStates =
    let wordsForReview = filterDueForReview today vocabStates
        numWords = length (elems wordsForReview)
    in distributeExercises numWords

-- | Select specific exercises for the day
selectExercisesForDay :: LocalTime
                      -> Map VocabularyId VocabularyState
                      -> (Int, Int, Int)  -- (reading, writing, typing)
                      -> [(ExerciseType, VocabularyId)]
selectExercisesForDay today vocabStates (numReading, numWriting, numTyping) =
    let dueWords = filterDueForReview today vocabStates
        sortedWords = sortByPriority today (elems dueWords)
        wordIds = take (numReading + numWriting + numTyping) (map vstVocabId sortedWords)
    in [ (Reading, wid) | wid <- take numReading wordIds ]
        ++ [ (Writing, wid) | wid <- take numWriting (drop numReading wordIds) ]
        ++ [ (Typing, wid) | wid <- take numTyping (drop (numReading + numWriting) wordIds) ]

-- | Filter vocabulary that is due for review
filterDueForReview :: LocalTime -> Map VocabularyId VocabularyState -> Map VocabularyId VocabularyState
filterDueForReview today = filterWithKey (\_ state -> vstNextReviewDate state <= today)

-- | Distribute exercises across the three types
distributeExercises :: Int -> (Int, Int, Int)
distributeExercises total
    | total == 0 = (0, 0, 0)
    | total < 3 = (total, 0, 0)
    | total < 6 = (total `div` 2, (total + 1) `div` 2, 0)
    | otherwise =
        let readingCount = ceiling (fromIntegral total * 0.4 :: Double)
            writingCount = ceiling (fromIntegral total * 0.35 :: Double)
            typingCount = total - readingCount - writingCount
        in (readingCount, writingCount, typingCount)

-- | Sort vocabulary by priority (due soonest, lowest mastery first)
sortByPriority :: LocalTime -> [VocabularyState] -> [VocabularyState]
sortByPriority _ = sortBy comparePriority
  where
    comparePriority a b =
        let reviewDateCmp = comparing vstNextReviewDate a b
            masteryLevelCmp = comparing (masteryLevelToInt . vstMasteryLevel) a b
        in case reviewDateCmp of
            EQ -> masteryLevelCmp
            other -> other

-- | Convert mastery level to integer for comparison (lower = higher priority)
masteryLevelToInt :: MasteryLevel -> Int
masteryLevelToInt New = 0
masteryLevelToInt Learning = 1
masteryLevelToInt Intermediate = 2
masteryLevelToInt Mastered = 3
