{-# LANGUAGE DeriveGeneric #-}

module Output.Domain.Progress
    ( emptyUserProgress
    , updateProgress
    ) where

import Data.Map (empty)
import Output.Domain.Types (UserProgress(..), TOPIK_Level(One))
import Data.Time (LocalTime)

-- | Create an empty user progress record
emptyUserProgress :: LocalTime -> UserProgress
emptyUserProgress _now = UserProgress
    { upCurrentLevel = One
    , upDailyStreak = 0
    , upLastActivityDate = Nothing
    , upTotalWordsLearned = 0
    , upTotalWordsReviewed = 0
    , upVocabularyStates = empty
    }

-- | Update user progress after activity
updateProgress :: UserProgress -> Int -> UserProgress
updateProgress progress newWords =
    progress { upTotalWordsLearned = upTotalWordsLearned progress + newWords }
