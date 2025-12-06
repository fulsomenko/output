module Output.Algorithm.StreakSpec (spec) where

import Test.Hspec
import Data.Time (LocalTime(..), fromGregorian, TimeOfDay(..))
import Output.Algorithm.Streak
    ( calculateStreak
    , isConsecutiveDay
    )
import Output.Domain.Activity (ActivityEntry(..), Performance(..), Percentage(..))
import Output.Domain.Types (ExerciseType(Reading))

spec :: Spec
spec = do
    describe "isConsecutiveDay" $ do
        it "returns True for consecutive days" $ do
            isConsecutiveDay (2024, 11, 3) (2024, 11, 2) `shouldBe` True

        it "returns False for non-consecutive days" $ do
            isConsecutiveDay (2024, 11, 3) (2024, 11, 1) `shouldBe` False

        it "handles month boundaries correctly" $ do
            isConsecutiveDay (2024, 12, 1) (2024, 11, 30) `shouldBe` True

        it "handles year boundaries correctly" $ do
            isConsecutiveDay (2025, 1, 1) (2024, 12, 31) `shouldBe` True

    describe "calculateStreak" $ do
        it "returns 0 for empty activity list" $ do
            let now = makeLocalTime 2024 11 2
            calculateStreak now [] `shouldBe` 0

        it "counts single activity as streak of 1" $ do
            let now = makeLocalTime 2024 11 2
            let activity = makeActivity now
            calculateStreak now [activity] `shouldBe` 1

        it "counts consecutive daily activities" $ do
            let now = makeLocalTime 2024 11 4
            let act1 = makeActivity (makeLocalTime 2024 11 4)
            let act2 = makeActivity (makeLocalTime 2024 11 3)
            let act3 = makeActivity (makeLocalTime 2024 11 2)
            calculateStreak now [act1, act2, act3] `shouldBe` 3

        it "breaks streak on gap in activities" $ do
            let now = makeLocalTime 2024 11 4
            let act1 = makeActivity (makeLocalTime 2024 11 4)
            let act2 = makeActivity (makeLocalTime 2024 11 2)  -- Gap on 11/3
            calculateStreak now [act1, act2] `shouldBe` 1

        it "ignores duplicate dates" $ do
            let now = makeLocalTime 2024 11 3
            let act1 = makeActivity (makeLocalTime 2024 11 3)
            let act2 = makeActivity (makeLocalTime 2024 11 3)
            let act3 = makeActivity (makeLocalTime 2024 11 2)
            calculateStreak now [act1, act2, act3] `shouldBe` 2

-- Helper functions
makeLocalTime :: Int -> Int -> Int -> LocalTime
makeLocalTime y m d = LocalTime
    (fromGregorian (fromIntegral y) m d)
    (TimeOfDay 12 0 0)

makeActivity :: LocalTime -> ActivityEntry
makeActivity time = ActivityEntry
    { actDate = time
    , actExerciseType = Reading
    , actVocabularyId = Nothing
    , actPerformance = Performance { perfAccuracy = 80, perfTimeSpent = 60, perfWpm = Nothing }
    , actSuccess = True
    , actNotes = Nothing
    }
