{-# LANGUAGE OverloadedStrings #-}

module Output.TUI.LookupSpec (spec) where

import Test.Hspec

import Output.TUI.Types
import Output.Domain.Jamo (Jamo (..))
import Output.Dict.Krdict (KrdictSense (..), KrdictTranslation (..), KrdictWord (..))

spec :: Spec
spec = do
    describe "lookupQuery" $ do
        it "composes jamo into syllables as they are typed" $
            lookupQuery
                initialLookupState
                    { lkInput = [ Consonant 'ㄴ', Vowel 'ㅏ', Consonant 'ㅁ', Vowel 'ㅜ' ]
                    }
                `shouldBe` "나무"

        it "keeps a final consonant attached to its syllable" $
            lookupQuery
                initialLookupState
                    { lkInput = [ Consonant 'ㅎ', Vowel 'ㅏ', Consonant 'ㄹ' ]
                    }
                `shouldBe` "할"

        it "promotes a final consonant when a new vowel follows" $
            lookupQuery
                initialLookupState
                    { lkInput = [ Consonant 'ㄴ', Vowel 'ㅓ', Consonant 'ㄹ', Vowel 'ㅓ' ]
                    }
                `shouldBe` "너러"

        it "renders an empty input as empty text" $
            lookupQuery initialLookupState `shouldBe` ""

    describe "applyJamo / popJamo" $ do
        it "appends jamo in the order pressed" $
            applyJamo initialLookupState (Consonant 'ㅁ')
                `shouldBe` initialLookupState { lkInput = [ Consonant 'ㅁ' ] }

        it "pops only the last jamo" $
            let s = applyJamo (applyJamo initialLookupState (Consonant 'ㄴ')) (Vowel 'ㅓ')
            in popJamo s
                `shouldBe` initialLookupState { lkInput = [ Consonant 'ㄴ' ] }

        it "is a no-op on empty input" $
            popJamo initialLookupState `shouldBe` initialLookupState

    describe "submitLookup" $
        it "enters loading and clears any stale note" $
            submitLookup
                initialLookupState
                    { lkStatus = LookupError "boom"
                    , lkNote   = Just "stale"
                    }
                `shouldBe` initialLookupState { lkStatus = LookupLoading }

    describe "applyLookupOutcome" $ do
        it "stores the fetched words and switches to Normal mode" $
            applyLookupOutcome (Right [sampleWord]) initialLookupState
                `shouldBe` initialLookupState { lkStatus = LookupResults [sampleWord], lkMode = NormalMode }

        it "clears a stale note on success" $
            applyLookupOutcome (Right [sampleWord]) initialLookupState { lkNote = Just "stale" }
                `shouldBe` initialLookupState { lkStatus = LookupResults [sampleWord], lkMode = NormalMode }

        it "stores the error message and switches to Normal mode" $
            applyLookupOutcome (Left "network down") initialLookupState
                `shouldBe` initialLookupState { lkStatus = LookupError "network down", lkMode = NormalMode }

    it "starts idle, in Insert mode, with no input, note, cursor or saved words" $
        initialLookupState
            `shouldBe` LookupState [] LookupIdle Nothing InsertMode 0 []

    describe "lookupResults" $ do
        it "returns the word list when a result set is present" $
            lookupResults (initialLookupState { lkStatus = LookupResults [sampleWord] })
                `shouldBe` [sampleWord]

        it "returns empty for the other statuses" $ do
            lookupResults initialLookupState
                `shouldBe` []
            lookupResults (initialLookupState { lkStatus = LookupLoading })
                `shouldBe` []
            lookupResults (initialLookupState { lkStatus = LookupError "boom" })
                `shouldBe` []

    describe "moveCursor" $ do
        it "moves one step in the given direction" $
            moveCursor 1 results2 `shouldBe` results2 { lkCursor = 1 }

        it "clamps at the end of the list" $
            moveCursor 1 (results2 { lkCursor = 1 }) `shouldBe` results2 { lkCursor = 1 }

        it "clamps at the start of the list" $
            moveCursor (-1) (results2 { lkCursor = 0 }) `shouldBe` results2 { lkCursor = 0 }

        it "is a no-op when there is no result set" $
            moveCursor 1 initialLookupState `shouldBe` initialLookupState

    describe "saveHighlighted" $ do
        it "records the highlighted word and advances the cursor" $ do
            let (s', w') = saveHighlighted (results2 { lkCursor = 0 })
            w' `shouldBe` Just sampleWord
            lkCursor s' `shouldBe` 1
            lkSaved s' `shouldBe` [kwWord sampleWord]

        it "clamps the cursor when saving the last result" $
            let (s', _) = saveHighlighted (results2 { lkCursor = 1 })
            in  lkCursor s' `shouldBe` 1

        it "does not duplicate a word that is already saved" $
            let base = results2 { lkCursor = 0, lkSaved = [kwWord sampleWord] }
                (s', _) = saveHighlighted base
            in  lkSaved s' `shouldBe` [kwWord sampleWord]

        it "is a no-op when there is no result set" $
            saveHighlighted initialLookupState
                `shouldBe` (initialLookupState, Nothing)

    describe "isSavedWord" $ do
        it "is false for a word that has not been saved yet" $
            isSavedWord (results2 { lkCursor = 0 }) sampleWord `shouldBe` False

        it "is true for a word that has been saved" $
            isSavedWord (results2 { lkCursor = 0, lkSaved = [kwWord sampleWord] }) sampleWord
                `shouldBe` True

-- | A two-result fixture for exercising cursor movement and saving.
results2 :: LookupState
results2 = initialLookupState { lkStatus = LookupResults [sampleWord, word2], lkMode = NormalMode }

-- | A minimal but realistic fixture for a KRDICT entry.
sampleWord :: KrdictWord
sampleWord = KrdictWord
    { kwTargetCode    = 12345
    , kwWord          = "나무"
    , kwPronunciation = ""
    , kwWordGrade     = "초급"
    , kwPos           = "명사"
    , kwLink          = "https://krdict.korean.go.kr/search?target_code=12345"
    , kwSenses        = [ KrdictSense
                            { ksOrder       = 1
                            , ksDefinition  = "나무로 이루어진 물건"
                            , ksTranslations = [ KrdictTranslation "영어" "wood" "a hard strong fibrous substance" ]
                            }
                        ]
    }

-- | A second distinct fixture for multi-result cases.
word2 :: KrdictWord
word2 = KrdictWord
    { kwTargetCode    = 54321
    , kwWord          = "물"
    , kwPronunciation = ""
    , kwWordGrade     = "초급"
    , kwPos           = "명사"
    , kwLink          = "https://krdict.korean.go.kr/search?target_code=54321"
    , kwSenses        = [ KrdictSense
                            { ksOrder       = 1
                            , ksDefinition  = "물의 상태를 나타내는 말"
                            , ksTranslations = [ KrdictTranslation "영어" "water" "a clear colourless liquid" ]
                            }
                        ]
    }
