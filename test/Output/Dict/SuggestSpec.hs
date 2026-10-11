{-# LANGUAGE OverloadedStrings #-}

module Output.Dict.SuggestSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import Output.Dict.Suggest (levenshtein, suggestions)

spec :: Spec
spec = do
    describe "levenshtein" $ do
        it "is zero for identical strings" $
            levenshtein "오이" "오이" `shouldBe` 0

        it "counts a single substitution" $
            levenshtein "오이" "오다" `shouldBe` 1

        it "counts an insertion" $
            levenshtein "나무" "나무들" `shouldBe` 1

        it "counts a deletion" $
            levenshtein "나무들" "나무" `shouldBe` 1

        it "is 3 for the classic kitten/sitting pair" $
            levenshtein "kitten" "sitting" `shouldBe` 3

        it "is symmetric" $
            levenshtein "kitten" "sitting" `shouldBe` levenshtein "sitting" "kitten"

        it "equals the other string's length when one side is empty" $ do
            levenshtein "" "abc" `shouldBe` 3
            levenshtein "abc" "" `shouldBe` 3

    describe "suggestions" $ do
        it "offers a one-edit neighbour for a short mistyped query" $
            suggestions ["오이", "나무", "물", "밥"] "우이" `shouldBe` ["오이"]

        it "ranks the closest matches first" $
            suggestions ["오이김치", "오이", "물"] "오이김" `shouldBe` ["오이김치", "오이"]

        it "caps the result at five words" $
            suggestions ["마", "바", "사", "아", "자", "다", "라", "파", "하"] "마"
                `shouldBe` ["마", "바", "사", "아", "자"]

        it "returns no suggestions when nothing is close enough" $
            suggestions ["오이"] "zzzz" `shouldBe` []

        it "returns no suggestions for an empty query" $
            suggestions ["오이"] "" `shouldBe` []

        it "returns no suggestions for an empty word list" $
            suggestions ([] :: [Text]) "오이" `shouldBe` []
