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
import Output.Domain.Types (VocabularyId(..), newVocabularyState)

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

    describe "calculateReview (SM-2)" $ do
        let state = newVocabularyState (VocabularyId 1) testTime

        it "Again produces shorter or equal interval than Good" $ do
            let againResult = calculateReview defaultSM2 state Again testTime
                goodResult  = calculateReview defaultSM2 state Good  testTime
            srsNewInterval againResult `shouldSatisfy` (<= srsNewInterval goodResult)

        it "Easy produces longer or equal interval than Good" $ do
            let goodResult = calculateReview defaultSM2 state Good testTime
                easyResult = calculateReview defaultSM2 state Easy testTime
            srsNewInterval easyResult `shouldSatisfy` (>= srsNewInterval goodResult)

        it "Hard produces shorter or equal interval than Easy" $ do
            let hardResult = calculateReview defaultSM2 state Hard testTime
                easyResult = calculateReview defaultSM2 state Easy testTime
            srsNewInterval hardResult `shouldSatisfy` (<= srsNewInterval easyResult)
