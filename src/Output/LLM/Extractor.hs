{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Output.LLM.Extractor
    ( ExtractedWord(..)
    , extractVocabFromConversation
    , isSpokenOccasion
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe, listToMaybe)
import GHC.Generics (Generic)

import Output.LLM.Client (OllamaConfig, OllamaMessage(..), callOllama)
import Output.Domain.Settings (Language, showLanguage)
import Output.Domain.Types (WordClass(..))

-- | A vocabulary word extracted from a lesson conversation.
data ExtractedWord = ExtractedWord
    { ewKorean      :: Text       -- Hangul only
    , ewWordClass   :: WordClass  -- Grammatical / morphological class
    , ewTranslation :: Text       -- In student's language
    , ewContext     :: Text       -- Why this word needs practice
    , ewStudentKnew :: Bool       -- Did the student already know it correctly?
    } deriving (Show, Eq, Generic)

instance FromJSON ExtractedWord
instance ToJSON ExtractedWord

extractionSystemPrompt :: Language -> Text
extractionSystemPrompt lang = T.unlines
    [ "You are a Korean vocabulary extraction assistant."
    , "Analyze the conversation and extract Korean words or short phrases the student should practice."
    , ""
    , "Include items where:"
    , "  - The student made an error that the teacher corrected"
    , "  - New vocabulary was introduced by the teacher"
    , "  - The student showed uncertainty or confusion"
    , ""
    , "IMPORTANT — word class identification:"
    , "For each word, identify its grammatical class using EXACTLY one of these labels:"
    , "  Noun         — standalone noun (손, 물, 책)"
    , "  Verb         — action verb in dictionary form ending in 다 (가다, 먹다)"
    , "  Adjective    — descriptive verb in dictionary form (크다, 좋다, 예쁘다)"
    , "  Adverb       — modifies verbs or adjectives (빨리, 정말, 매우)"
    , "  Particle     — grammatical particle that attaches to nouns (은/는, 이/가, 을/를)"
    , "  BoundMorpheme — prefix, suffix or Sino-Korean root that CANNOT stand alone"
    , "                  (선- as in 선생님, -님, -들, -하다 roots)"
    , "  Counter      — counting classifier (개, 명, 마리, 번)"
    , "  Determiner   — prenominal determiner (이, 그, 저, 어떤)"
    , "  Interjection — standalone exclamation (아!, 와!, 네, 아니요)"
    , "  Expression   — fixed multi-word phrase (감사합니다, 괜찮아요, 어떻게 지내세요?)"
    , "  Unknown      — when you are not certain of the class"
    , ""
    , "For BoundMorpheme: include the full compound it appears in as an example."
    , "Do NOT extract BoundMorpheme entries as if they were standalone vocabulary;"
    , "instead note the compound and explain the morpheme's role."
    , ""
    , "For EACH item output EXACTLY this block (one blank line between entries):"
    , "WORD: [Korean in Hangul — never romanization]"
    , "CLASS: [one of the labels above, exactly as written]"
    , "MEANING: [translation in " <> showLanguage lang <> "]"
    , "CONTEXT: [one sentence: why this item needs practice]"
    , "KNEW: [yes or no]"
    , ""
    , "If there is nothing to extract, output only: NO_WORDS"
    , "Do not output anything else."
    ]

-- | Call the extraction agent on recent conversation turns.
extractVocabFromConversation
    :: OllamaConfig
    -> Language
    -> [OllamaMessage]
    -> IO (Either Text [ExtractedWord])
extractVocabFromConversation cfg lang msgs = do
    let sysPrompt  = extractionSystemPrompt lang
        recentMsgs = takeLast 4 msgs
    result <- callOllama cfg sysPrompt recentMsgs
    pure $ case result of
        Left err   -> Left err
        Right text -> Right (parseExtraction text)
  where
    takeLast n xs = drop (max 0 (length xs - n)) xs

-- | Parse the structured extraction output.
parseExtraction :: Text -> [ExtractedWord]
parseExtraction txt
    | "NO_WORDS" `T.isInfixOf` txt = []
    | otherwise = mapMaybe parseBlock blocks
  where
    blocks = filter (not . T.null . T.strip) $ T.splitOn "\n\n" (T.strip txt)
    parseBlock block =
        let ls    = map T.strip (T.lines block)
            field p = T.strip . T.drop (T.length p) <$>
                      listToMaybe (filter (T.isPrefixOf p) ls)
        in case (field "WORD:", field "CLASS:", field "MEANING:", field "CONTEXT:", field "KNEW:") of
            (Just w, Just cls, Just m, Just c, Just k) | not (T.null w) -> Just ExtractedWord
                { ewKorean      = w
                , ewWordClass   = parseWordClass cls
                , ewTranslation = m
                , ewContext     = c
                , ewStudentKnew = T.toLower (T.take 3 k) == "yes"
                }
            _ -> Nothing

parseWordClass :: Text -> WordClass
parseWordClass t = case T.strip t of
    "Noun"          -> Noun
    "Verb"          -> Verb
    "Adjective"     -> Adjective
    "Adverb"        -> Adverb
    "Particle"      -> Particle
    "BoundMorpheme" -> BoundMorpheme
    "Counter"       -> Counter
    "Determiner"    -> Determiner
    "Interjection"  -> Interjection
    "Expression"    -> Expression
    _               -> Unknown

-- | True if the AI message requests spoken or listening practice.
isSpokenOccasion :: Text -> Bool
isSpokenOccasion msg = any (`T.isInfixOf` lower)
    [ "repeat after me", "listen and", "try saying", "try pronouncing"
    , "say it", "say this", "말해보세요", "따라하세요", "따라 하세요"
    ]
  where
    lower = T.toLower msg
