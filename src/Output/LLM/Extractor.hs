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

-- | A vocabulary word extracted from a lesson conversation.
data ExtractedWord = ExtractedWord
    { ewKorean      :: Text   -- Hangul only
    , ewTranslation :: Text   -- In student's language
    , ewContext     :: Text   -- Why this word needs practice
    , ewStudentKnew :: Bool   -- Did the student already know it correctly?
    } deriving (Show, Eq, Generic)

instance FromJSON ExtractedWord
instance ToJSON ExtractedWord

extractionSystemPrompt :: Language -> Text
extractionSystemPrompt lang = T.unlines
    [ "You are a vocabulary extraction assistant for Korean language learning."
    , "Analyze the following conversation between a student and their Korean teacher."
    , "Extract Korean words or short phrases the student should practice."
    , ""
    , "Include words where:"
    , "  - The student made an error that the teacher corrected"
    , "  - New vocabulary was introduced by the teacher"
    , "  - The student showed uncertainty or confusion"
    , ""
    , "For EACH word output EXACTLY this block (one blank line between entries):"
    , "WORD: [Korean word or phrase in Hangul — never romanization]"
    , "MEANING: [translation in " <> showLanguage lang <> "]"
    , "CONTEXT: [one sentence: why this word needs practice]"
    , "KNEW: [yes or no]"
    , ""
    , "If there is nothing to extract, output only: NO_WORDS"
    , "Do not output anything else."
    ]

-- | Call the extraction agent on recent conversation turns.
-- Pass the last 4 messages for context (2 user + 2 assistant turns).
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

-- | Parse the structured extraction output into a list of words.
parseExtraction :: Text -> [ExtractedWord]
parseExtraction txt
    | "NO_WORDS" `T.isInfixOf` txt = []
    | otherwise = mapMaybe parseBlock blocks
  where
    blocks = filter (not . T.null . T.strip) $ T.splitOn "\n\n" (T.strip txt)
    parseBlock block =
        let ls     = map T.strip (T.lines block)
            field p = T.strip . T.drop (T.length p) <$>
                      listToMaybe (filter (T.isPrefixOf p) ls)
        in case (field "WORD:", field "MEANING:", field "CONTEXT:", field "KNEW:") of
            (Just w, Just m, Just c, Just k) | not (T.null w) -> Just ExtractedWord
                { ewKorean      = w
                , ewTranslation = m
                , ewContext     = c
                , ewStudentKnew = T.toLower (T.take 3 k) == "yes"
                }
            _ -> Nothing

-- | True if the AI message contains a request for spoken or listening practice.
-- These turns can't be completed by text input and need native speaker interaction.
isSpokenOccasion :: Text -> Bool
isSpokenOccasion msg = any (`T.isInfixOf` lower)
    [ "repeat after me"
    , "listen and"
    , "try saying"
    , "try pronouncing"
    , "say it"
    , "say this"
    , "말해보세요"
    , "따라하세요"
    , "따라 하세요"
    ]
  where
    lower = T.toLower msg
