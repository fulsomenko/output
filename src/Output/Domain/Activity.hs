{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Output.Domain.Activity
    ( ActivityEntry(..)
    , Performance(..)
    , Percentage(..)
    , newPerformance
    ) where

import Data.Text (Text)
import Data.Time (LocalTime)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Output.Domain.Types (VocabularyId, ExerciseType)

-- | Percentage score (0-100)
newtype Percentage = Percentage Double
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, Num, Real, Fractional, RealFrac)

-- | Performance metrics for an exercise
data Performance = Performance
    { perfAccuracy :: Percentage     -- 0-100 for all
    , perfTimeSpent :: Int           -- seconds
    , perfWpm :: Maybe Int           -- words per minute (typing only)
    } deriving (Show, Eq, Generic)

instance FromJSON Performance
instance ToJSON Performance

-- | Create a performance entry
newPerformance :: Percentage -> Int -> Maybe Int -> Performance
newPerformance = Performance

-- | A logged activity/exercise completion
data ActivityEntry = ActivityEntry
    { actDate :: LocalTime
    , actExerciseType :: ExerciseType
    , actVocabularyId :: Maybe VocabularyId
    , actPerformance :: Performance
    , actSuccess :: Bool
    , actNotes :: Maybe Text
    } deriving (Show, Eq, Generic)

instance FromJSON ActivityEntry
instance ToJSON ActivityEntry
