module Output.Algorithm.SRSSpec (spec) where

import Test.Hspec
import Data.Time (LocalTime(..), fromGregorian, TimeOfDay(..))

import Output.Algorithm.SRS
    ( Quality(..)
    , ratingToQuality
    , SRSResult(..)
    , SRSAlgorithm(..)
    )
import Output.Algorithm.SpacedRepetition (defaultSM2)
import Output.Domain.Types (VocabularyId(..), newVocabularyState, vstCurrentInterval, vstReviewCount)

testTime :: LocalTime
testTime = LocalTime (fromGregorian 2026 1 1) (TimeOfDay 0 0 0)

spec :: Spec
spec = do
    describe "ratingToQuality" $ do
        it "maps 1 to Again" $
            ratingToQuality 1 `shouldBe` Again
        it "maps 2 to Hard" $
            ratingToQuality 2 `shouldBe` Hard
        it "maps 3 to Good" $
            ratingToQuality 3 `shouldBe` Good
        it "maps 4 to Easy" $
            ratingToQuality 4 `shouldBe` Easy
        it "maps out-of-range (0) to Good" $
            ratingToQuality 0 `shouldBe` Good

    describe "calculateReview (SM-2)" $ do
        let establishedState = (newVocabularyState (VocabularyId 1) testTime)
                { vstCurrentInterval = 6
                , vstReviewCount = 2
                }

        it "Again resets interval to 1" $
            srsNewInterval (calculateReview defaultSM2 establishedState Again testTime)
                `shouldBe` 1

        it "Again produces strictly shorter interval than Good" $ do
            let againResult = calculateReview defaultSM2 establishedState Again testTime
                goodResult  = calculateReview defaultSM2 establishedState Good  testTime
            srsNewInterval againResult `shouldSatisfy` (< srsNewInterval goodResult)

        it "Easy produces strictly longer interval than Good" $ do
            let goodResult = calculateReview defaultSM2 establishedState Good testTime
                easyResult = calculateReview defaultSM2 establishedState Easy testTime
            srsNewInterval easyResult `shouldSatisfy` (> srsNewInterval goodResult)

        it "Hard falls strictly between Again and Good" $ do
            let againResult = calculateReview defaultSM2 establishedState Again testTime
                hardResult  = calculateReview defaultSM2 establishedState Hard  testTime
                goodResult  = calculateReview defaultSM2 establishedState Good  testTime
            srsNewInterval hardResult `shouldSatisfy` (> srsNewInterval againResult)
            srsNewInterval hardResult `shouldSatisfy` (< srsNewInterval goodResult)
