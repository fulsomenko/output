{-# LANGUAGE OverloadedStrings #-}

module Output.LLM.Summarizer
    ( summarizeSession
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Output.LLM.Client (OllamaConfig, OllamaMessage(..), callOllama)
import Output.Domain.Settings (Language, showLanguage)
import Output.Domain.StudentProfile (StudentProfile(..))

-- | Analyze a lesson conversation and return an updated student profile summary.
-- On first session mProfile is Nothing; subsequent sessions receive the existing
-- summary so the agent can refine and accumulate understanding over time.
summarizeSession
    :: OllamaConfig
    -> Language
    -> [OllamaMessage]      -- Full session messages
    -> Maybe StudentProfile -- Existing profile to update (Nothing = first session)
    -> IO (Either Text Text)
summarizeSession cfg lang msgs mProfile =
    callOllama cfg sysPrompt [userMsg]
  where
    sysPrompt = summarizationPrompt lang mProfile
    userMsg   = OllamaMessage { role = "user", content = formatConversation msgs }

summarizationPrompt :: Language -> Maybe StudentProfile -> Text
summarizationPrompt lang mProfile = T.unlines $
    [ "You are a Korean language learning analyst tracking a student's long-term progress."
    , "Analyze the lesson conversation below and write a standing student profile."
    , ""
    ] ++ previousBlock ++
    [ "Write the updated profile in " <> showLanguage lang <> "."
    , "Write it as if briefing a new teacher before their first lesson with this student."
    , "Include in 4-6 sentences:"
    , "- Current Korean ability level and what the student can or cannot yet do"
    , "- Specific vocabulary items or grammar patterns they struggle with"
    , "- Demonstrated strengths and areas of confidence"
    , "- Goals, motivations, or real-world contexts they mentioned (travel, work, etc.)"
    , ""
    , "Rules:"
    , "- Be specific and concrete — reference actual Korean words or patterns observed"
    , "- Write as a single flowing paragraph, not bullet points"
    , "- Do not start with 'The student' — vary your sentence openings"
    , "- Do not reference 'the previous session' — write as a standing profile"
    , "- Output ONLY the profile paragraph, nothing else"
    ]
  where
    previousBlock = case mProfile of
        Nothing -> []
        Just p  ->
            [ "Existing profile to refine (update and expand — do not simply repeat it):"
            , spSummary p
            , ""
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
