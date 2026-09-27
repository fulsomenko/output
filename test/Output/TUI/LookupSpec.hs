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
                `shouldBe` initialLookupState
                    { lkInput = [ Consonant 'ㅁ' ]
                    }

        it "pops only the last jamo" $
            let s = applyJamo (applyJamo initialLookupState (Consonant 'ㄴ')) (Vowel 'ㅓ')
            in popJamo s
                `shouldBe` initialLookupState
                    { lkInput = [ Consonant 'ㄴ' ]
                    }

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
        it "stores the fetched words on success" $
            applyLookupOutcome (Right [sampleWord]) initialLookupState
                `shouldBe` initialLookupState { lkStatus = LookupResults [sampleWord] }

        it "clears a stale note on success" $
            applyLookupOutcome (Right [sampleWord]) initialLookupState { lkNote = Just "stale" }
                `shouldBe` initialLookupState { lkStatus = LookupResults [sampleWord] }

        it "stores the error message on failure" $
            applyLookupOutcome (Left "network down") initialLookupState
                `shouldBe` initialLookupState { lkStatus = LookupError "network down" }

    it "starts idle with no input and no note" $
        initialLookupState
            `shouldBe` LookupState [] LookupIdle Nothing

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
