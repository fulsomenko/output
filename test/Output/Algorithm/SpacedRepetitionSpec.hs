module Output.Algorithm.SpacedRepetitionSpec (spec) where

import Test.Hspec
import Data.Time (LocalTime(..), fromGregorian, TimeOfDay(..))
import Output.Algorithm.SpacedRepetition
    ( calculateNextReviewDate
    , calculateIntervalDays
    , updateMasteryLevel
    )
import Output.Domain.Types (MasteryLevel(..))

spec :: Spec
spec = do
    describe "calculateIntervalDays" $ do
        it "returns 1 for first review" $ do
            calculateIntervalDays 0 0.0 `shouldBe` 1

        it "returns 3 for second review with high accuracy" $ do
            calculateIntervalDays 1 0.8 `shouldBe` 3

        it "returns 1 for second review with low accuracy" $ do
            calculateIntervalDays 1 0.5 `shouldBe` 1

        it "increases interval with more reviews and high accuracy" $ do
            let interval = calculateIntervalDays 3 0.9
            interval `shouldSatisfy` (> 3)

        it "keeps interval low for poor performance" $ do
            let interval = calculateIntervalDays 3 0.5
            interval `shouldSatisfy` (< 7)

    describe "updateMasteryLevel" $ do
        it "returns New for no reviews" $ do
            updateMasteryLevel 0 0 `shouldBe` New

        it "returns Learning for early reviews" $ do
            updateMasteryLevel 2 1 `shouldBe` Learning

        it "returns Intermediate for 70% accuracy" $ do
            updateMasteryLevel 10 7 `shouldBe` Intermediate

        it "returns Mastered for 85%+ accuracy" $ do
            updateMasteryLevel 10 9 `shouldBe` Mastered

    describe "calculateNextReviewDate" $ do
        it "schedules first review in 1 day" $ do
            let now = makeLocalTime 2024 11 2
            let nextReview = calculateNextReviewDate now 0 0
            nextReview `shouldBe` makeLocalTime 2024 11 3

        it "schedules second review in 3 days for good performance" $ do
            let now = makeLocalTime 2024 11 2
            let nextReview = calculateNextReviewDate now 1 1
            nextReview `shouldBe` makeLocalTime 2024 11 5

-- Helper function to create a LocalTime
makeLocalTime :: Int -> Int -> Int -> LocalTime
makeLocalTime y m d = LocalTime
    (fromGregorian (fromIntegral y) m d)
    (TimeOfDay 12 0 0)
