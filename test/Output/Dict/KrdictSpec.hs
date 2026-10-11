{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Tests for the pure parts of the KRDICT integration: XML parsing,
-- part-of-speech mapping, card building and percent-encoding. None of these
-- touch the network, so they run without an API key.
module Output.Dict.KrdictSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (writeFile)

import Output.Domain.Types
    ( TOPIK_Level(..)
    , VocabularyCard(..)
    , WordClass(..)
    )
import Output.Dict.Krdict
    ( KrdictError(..)
    , KrdictSense(..)
    , KrdictTranslation(..)
    , KrdictWord(..)
    , englishTranslation
    , englishTranslations
    , krdictParse
    , krdictToCard
    , krdictWordClass
    , percentEncode
    , resolveApiKey
    , resolveApiKeyChecked
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

    describe "krdictParse CDATA" $ do
        -- The real KRDICT API wraps trans_word / trans_dfn in <![CDATA[...]]>,
        -- unlike the plain-text fixture above. This must still parse, and the
        -- English gloss must be recoverable.
        it "extracts CDATA-wrapped trans_word and trans_dfn" $ do
            case krdictParse cdataXml of
                Left e   -> fail $ "could not parse CDATA sample: " <> T.unpack e
                Right ws -> do
                    length ws `shouldBe` 1
                    let [w] = ws
                    kwWord w `shouldBe` "오이"
                    kwPos w  `shouldBe` "명사"
                    length (kwSenses w) `shouldBe` 1
                    let [s] = kwSenses w
                    length (ksTranslations s) `shouldBe` 1
                    englishTranslation s `shouldBe`
                        Just (KrdictTranslation "영어" "cucumber"
                               "A plant which has yellow flowers and long, oval-shaped, green fruits in summer, or its fruit.")

        it "keeps the comma-separated gloss intact in CDATA" $ do
            case krdictParse cdataMultiXml of
                Left e   -> fail $ "could not parse CDATA sample: " <> T.unpack e
                Right ws -> do
                    let [w] = ws
                    englishTranslations w `shouldBe` ["oikimchi, cucumber kimchi"]

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
        it "keeps a bare key verbatim" $ do
            resolved <- resolveApiKey "0123456789abcdef0123456789abcdef"
            resolved `shouldBe` "0123456789abcdef0123456789abcdef"

        it "reads the key from a file when the value references one" $ do
            -- The test-suite's deps are pinned to base-only (no directory
            -- package), so no temp dir is available. dist-newstyle (cabal's
            -- build dir, present once the suite has been built, and gitignored)
            -- is a safe scratch location; the key is written, asserted, and the
            -- scratch content is truncated (it cannot be unlinked without the
            -- directory package).
            let path = "dist-newstyle/.krdict-spec-key"
            writeFile path "abcdef0123456789abcdef0123456789\n"
            resolved <- resolveApiKey (T.pack path)
            resolved `shouldBe` "abcdef0123456789abcdef0123456789"
            writeFile path ""

    -- Validation for the secret. Each bad case produces a precise
    -- KrdictConfigError instead of an opaque KRDICT server error -- the
    -- exact failures an empty / mis-pointed KRDICT_API_KEY produces.
    describe "resolveApiKeyChecked" $ do
        let
            -- Assert a value is rejected with a precise KrdictConfigError
            -- (the message content is implementation detail; only the variant
            -- is asserted here, matching by pattern to avoid type holes).
            configErr :: Text -> Expectation
            configErr raw = do
                res <- resolveApiKeyChecked raw
                res `shouldSatisfy` \case
                    Left (KrdictConfigError _msg) -> True
                    _                             -> False

        it "accepts a bare 32-character hex key" $
            resolveApiKeyChecked "0123456789abcdef0123456789abcdef"
                `shouldReturn` (Right "0123456789abcdef0123456789abcdef")

        it "rejects an empty (or whitespace-only) value" $
            configErr "   "

        it "rejects a bare value that is not a 32-character hex key" $
            configErr "not-a-key"

        it "rejects a secret file that does not exist" $
            configErr "dist-newstyle/.krdict-spec-missing"

        it "rejects a secret file that is empty" $ do
            let path = "dist-newstyle/.krdict-spec-empty"
            writeFile path ""
            configErr (T.pack path)

        it "resolves a 32-character hex key from a file" $ do
            let path = "dist-newstyle/.krdict-spec-key"
            writeFile path "abcdef0123456789abcdef0123456789\n"
            resolveApiKeyChecked (T.pack path)
                `shouldReturn` (Right "abcdef0123456789abcdef0123456789")
            writeFile path ""

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

-- | A KRDICT response in its real wire format: the trans_word / trans_dfn
-- values are CDATA sections (the plain-text sample above is not how KRDICT
-- actually emits them, which is why the CDATA tests are separate).
cdataXml :: Text
cdataXml =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  <> "<response>"
  <> "<document>"
  <> "<channel>"
  <> "<item>"
  <> "<target_code>200</target_code>"
  <> "<word>오이</word>"
  <> "<word_grade>초급</word_grade>"
  <> "<pos>명사</pos>"
  <> "<link>https://krdict.korean.go.kr</link>"
  <> "<sense>"
  <> "<sense_order>1</sense_order>"
  <> "<definition>여름에 노란 꽃이 피고 초록색의 긴 타원형 열매가 열리는 식물. 또는 그 열매.</definition>"
  <> "<translation>"
  <> "<trans_lang>영어</trans_lang>"
  <> "<trans_word><![CDATA[cucumber]]></trans_word>"
  <> "<trans_dfn><![CDATA[A plant which has yellow flowers and long, oval-shaped, green fruits in summer, or its fruit. ]]></trans_dfn>"
  <> "</translation>"
  <> "</sense>"
  <> "</item>"
  <> "</channel>"
  <> "</document>"
  <> "</response>"

-- | A CDATA response whose gloss contains a comma (multi-word), to guard the
-- extractor against splitting on punctuation.
cdataMultiXml :: Text
cdataMultiXml =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  <> "<response>"
  <> "<document>"
  <> "<channel>"
  <> "<item>"
  <> "<target_code>201</target_code>"
  <> "<word>오이김치</word>"
  <> "<word_grade>중급</word_grade>"
  <> "<pos>명사</pos>"
  <> "<link>https://krdict.korean.go.kr</link>"
  <> "<sense>"
  <> "<sense_order>1</sense_order>"
  <> "<definition>오이를 이용하여 만든 김치.</definition>"
  <> "<translation>"
  <> "<trans_lang>영어</trans_lang>"
  <> "<trans_word><![CDATA[oikimchi, cucumber kimchi]]></trans_word>"
  <> "<trans_dfn><![CDATA[A type of Korean pickle made from cucumbers. ]]></trans_dfn>"
  <> "</translation>"
  <> "</sense>"
  <> "</item>"
  <> "</channel>"
  <> "</document>"
  <> "</response>"
