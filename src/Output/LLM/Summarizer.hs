{-# LANGUAGE OverloadedStrings #-}

module Output.LLM.Summarizer
    ( summarizeSession
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Output.LLM.Client (OllamaConfig, OllamaMessage(..), callOllama)
import Output.Domain.Settings (Language, showLanguage)

-- | Analyze one lesson conversation and return a session note (3-5 sentences).
-- The note covers THIS session only — it is appended to the student's note log,
-- never used to rewrite history. This guarantees no past observations are lost.
summarizeSession
    :: OllamaConfig
    -> Language
    -> [OllamaMessage]    -- Full session conversation
    -> IO (Either Text Text)
summarizeSession cfg lang msgs =
    callOllama cfg (summarizationPrompt lang) [userMsg]
  where
    userMsg = OllamaMessage { role = "user", content = formatConversation msgs }

summarizationPrompt :: Language -> Text
summarizationPrompt lang = T.unlines
    [ "You are a Korean language learning analyst."
    , "Write a brief observation note about THIS specific lesson (3-5 sentences)."
    , ""
    , "Include ALL of the following that apply:"
    , "- What was practised in this session (vocabulary topic, grammar point, etc.)"
    , "- Specific errors made — name the actual Korean words or grammar patterns"
    , "- Improvements or breakthroughs compared to what the student attempted"
    , "- Goals, motivations, or real-world contexts the student mentioned"
    , ""
    , "Rules:"
    , "- Write in " <> showLanguage lang
    , "- Be specific: cite Korean words (in Hangul) and grammar patterns you observed"
    , "- Write as a single flowing paragraph, not bullet points"
    , "- Do not start with 'The student' — vary your sentence openings"
    , "- Describe only what happened in THIS session, not a cumulative profile"
    , "- Output ONLY the paragraph, nothing else"
    ]

formatConversation :: [OllamaMessage] -> Text
formatConversation msgs = T.unlines $
    "=== Lesson Conversation ===" : map fmt msgs ++ ["==========================="]
  where
    fmt msg = speaker <> ": " <> content msg
      where
        speaker = case role msg of
            "user"      -> "Student"
            "assistant" -> "Teacher"
            r           -> r
