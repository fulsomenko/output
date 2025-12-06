{-# LANGUAGE DeriveGeneric #-}

module Output.Domain.Session
    ( Session(..)
    , SessionResult(..)
    , startSession
    , recordAnswer
    , currentExercise
    , isSessionComplete
    , sessionStats
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (LocalTime, diffLocalTime, nominalDiffTimeToSeconds)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Output.Domain.Types (ExerciseType)
import Output.Domain.Exercise (ExercisePrompt(..), ExerciseResult(..))

-- | A practice session containing multiple exercises
data Session = Session
    { sessionExercises :: [ExercisePrompt]  -- All exercises
    , sessionResults :: [ExerciseResult]     -- Completed results
    , sessionCurrentIndex :: Int             -- Current exercise index
    , sessionStartTime :: LocalTime          -- When session started
    , sessionExerciseStartTime :: LocalTime  -- When current exercise started
    } deriving (Show, Eq, Generic)

instance FromJSON Session
instance ToJSON Session

-- | Summary of a completed session
data SessionResult = SessionResult
    { srTotalExercises :: Int
    , srCorrectCount :: Int
    , srIncorrectCount :: Int
    , srAccuracy :: Double  -- Percentage 0-100
    , srAverageTime :: Double  -- Seconds per exercise
    , srResults :: [ExerciseResult]
    } deriving (Show, Eq, Generic)

instance FromJSON SessionResult
instance ToJSON SessionResult

-- | Start a new session with the given exercises
startSession :: [ExercisePrompt] -> LocalTime -> Session
startSession exercises now = Session
    { sessionExercises = exercises
    , sessionResults = []
    , sessionCurrentIndex = 0
    , sessionStartTime = now
    , sessionExerciseStartTime = now
    }

-- | Record an answer and advance to the next exercise
recordAnswer
    :: Session
    -> Bool          -- Was the answer correct?
    -> Text          -- User's answer
    -> LocalTime     -- Current time
    -> Maybe Double  -- WPM for typing exercises
    -> Session
recordAnswer session correct userAnswer now mWpm =
    let currentPrompt = sessionExercises session !! sessionCurrentIndex session
        timeDiff = diffLocalTime now (sessionExerciseStartTime session)
        result = ExerciseResult
            { erVocabId = epVocabId currentPrompt
            , erExerciseType = epExerciseType currentPrompt
            , erCorrect = correct
            , erUserAnswer = userAnswer
            , erExpectedAnswer = epExpectedAnswer currentPrompt
            , erTimeTaken = timeDiff
            , erWordsPerMinute = mWpm
            }
    in session
        { sessionResults = sessionResults session ++ [result]
        , sessionCurrentIndex = sessionCurrentIndex session + 1
        , sessionExerciseStartTime = now
        }

-- | Get the current exercise prompt, if any remain
currentExercise :: Session -> Maybe ExercisePrompt
currentExercise session
    | sessionCurrentIndex session < length (sessionExercises session) =
        Just $ sessionExercises session !! sessionCurrentIndex session
    | otherwise = Nothing

-- | Check if all exercises are complete
isSessionComplete :: Session -> Bool
isSessionComplete session =
    sessionCurrentIndex session >= length (sessionExercises session)

-- | Calculate session statistics
sessionStats :: Session -> SessionResult
sessionStats session =
    let results = sessionResults session
        total = length results
        correct = length $ filter erCorrect results
        accuracy = if total > 0
            then (fromIntegral correct / fromIntegral total) * 100
            else 0
        totalTime = sum $ map (realToFrac . nominalDiffTimeToSeconds . erTimeTaken) results
        avgTime = if total > 0 then totalTime / fromIntegral total else 0
    in SessionResult
        { srTotalExercises = total
        , srCorrectCount = correct
        , srIncorrectCount = total - correct
        , srAccuracy = accuracy
        , srAverageTime = avgTime
        , srResults = results
        }
