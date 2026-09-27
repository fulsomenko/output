{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the pure parts of the KRDICT integration: XML parsing,
-- part-of-speech mapping, card building and percent-encoding. None of these
-- touch the network, so they run without an API key.
module Output.Dict.KrdictSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeFile)
import System.IO (writeFile)

import Output.Domain.Types
    ( TOPIK_Level(..)
    , VocabularyCard(..)
    , WordClass(..)
    )
import Output.Dict.Krdict
    ( KrdictSense(..)
    , KrdictTranslation(..)
    , KrdictWord(..)
    , englishTranslation
    , englishTranslations
    , krdictParse
    , krdictToCard
    , krdictWordClass
    , percentEncode
    , resolveApiKey
    )

spec :: Spec
spec = do
    describe "krdictParse" $ do
        it "parses a nested KRDICT response into its word(s)" $ do
            case krdictParse sampleXml of
                Left e   -> fail $ "could not parse sample: " <> T.unpack e
                Right ws -> do
                    length ws `shouldBe` 1
                    let w = head ws
                    kwTargetCode w `shouldBe` 100
                    kwWord w `shouldBe` "나무"
                    kwWordGrade w `shouldBe` "초급"
                    kwPos w `shouldBe` "명사"
                    length (kwSenses w) `shouldBe` 2

        it "parses each sense and its language translations" $ do
            let Right ws = krdictParse sampleXml
            let [w]      = ws
            let senses   = kwSenses w
            let [s1, s2] = senses
            ksDefinition s1 `shouldBe` "줄기나 가지가 목질로 된 여러해살이 식물."
            length (ksTranslations s1) `shouldBe` 2   -- English + Japanese
            length (ksTranslations s2) `shouldBe` 1   -- Japanese only

        it "rejects content that is not KRDICT XML" $ do
            case krdictParse "not-a-krdict-response" of
                Left _   -> True
                Right _  -> False
            `shouldBe` True

    describe "englishTranslation" $ do
        it "picks the English (영어) translation when present" $ do
            let Right ws = krdictParse sampleXml
            let [w]      = ws
            englishTranslation (head (kwSenses w)) `shouldBe`
                Just (KrdictTranslation "영어" "tree"
                       "A plant with a hard stem, branches and leaves.")

        it "is Nothing when a sense has no English translation" $ do
            let Right ws = krdictParse sampleXml
            let [w]      = ws
            englishTranslation (head (tail (kwSenses w))) `shouldBe` Nothing

    describe "englishTranslations" $ do
        it "collects the English headword across every sense" $ do
            let Right ws = krdictParse sampleXml
            englishTranslations (head ws) `shouldBe` ["tree"]

    describe "krdictWordClass" $ do
        it "maps 명사 to Noun" $
            krdictWordClass "명사" `shouldBe` Noun
        it "maps 동사 to Verb" $
            krdictWordClass "동사" `shouldBe` Verb
        it "maps 형용사 to Adjective" $
            krdictWordClass "형용사" `shouldBe` Adjective
        it "matches a part of speech by key substring" $
            krdictWordClass "1. (사물) 명사" `shouldBe` Noun
        it "maps an unknown part of speech to Unknown" $
            krdictWordClass "xyz" `shouldBe` Unknown

    describe "krdictToCard" $ do
        it "builds a card with the mapped class, word and English meanings" $ do
            let Right ws = krdictParse sampleXml
            let [w]      = ws
            let card     = krdictToCard One w
            wordClass card `shouldBe` Noun
            korean card `shouldBe` "나무"
            english card `shouldBe` ["tree"]
            topicLevel card `shouldBe` One
            romanization card `shouldBe` ""

    describe "percentEncode" $ do
        it "leaves unreserved ASCII characters as-is" $
            percentEncode "abcABC123-_~." `shouldBe` "abcABC123-_~."
        it "percent-encodes spaces and symbols" $
            percentEncode "a b" `shouldBe` "a%20b"
        it "percent-encodes UTF-8 bytes for Korean text" $
            percentEncode "가" `shouldBe` "%ea%b0%80"

    describe "resolveApiKey" $ do
        it "keeps a bare key verbatim" $
            resolveApiKey "0123456789abcdef0123456789abcdef"
                `shouldBe` "0123456789abcdef0123456789abcdef"

        it "reads the key from a file when the value references one" $ do
            tmpDir <- getTemporaryDirectory
            let dir  = tmpDir ++ "/output-krdict-spec"
                path = dir ++ "/test-key"
            createDirectoryIfMissing True dir
            writeFile path "abcdef0123456789abcdef0123456789\n"
            resolveApiKey path `shouldBe` "abcdef0123456789abcdef0123456789"
            removeFile path

-- | A representative KRDICT search response. KRDICT nests its <item> elements
-- under <response><document><channel>, so this fixture also exercises the
-- parser's handling of nested containers (not just direct children).
sampleXml :: Text
sampleXml =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  <> "<response>"
  <> "<document>"
  <> "<channel>"
  <> "<item>"
  <> "<target_code>100</target_code>"
  <> "<word>나무</word>"
  <> "<word_grade>초급</word_grade>"
  <> "<pos>명사</pos>"
  <> "<link>https://krdict.korean.go.kr</link>"
  <> "<sense>"
  <> "<sense_order>1</sense_order>"
  <> "<definition>줄기나 가지가 목질로 된 여러해살이 식물.</definition>"
  <> "<translation>"
  <> "<trans_lang>영어</trans_lang>"
  <> "<trans_word>tree</trans_word>"
  <> "<trans_dfn>A plant with a hard stem, branches and leaves.</trans_dfn>"
  <> "</translation>"
  <> "<translation>"
  <> "<trans_lang>일본어</trans_lang>"
  <> "<trans_word>木</trans_word>"
  <> "<trans_dfn>茎や枝が木質の多年草。</trans_dfn>"
  <> "</translation>"
  <> "</sense>"
  <> "<sense>"
  <> "<sense_order>2</sense_order>"
  <> "<definition>재목으로 쓰기 좋은 나무.</definition>"
  <> "<translation>"
  <> "<trans_lang>일본어</trans_lang>"
  <> "<trans_word>材木</trans_word>"
  <> "<trans_dfn>材木用の木。</trans_dfn>"
  <> "</translation>"
  <> "</sense>"
  <> "</item>"
  <> "</channel>"
  <> "</document>"
  <> "</response>"
