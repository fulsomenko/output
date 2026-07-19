{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Output.LLM.Agent
    ( AgentTask(..)
    , taskLabel
    , taskMenuDesc
    , agentSystemPrompt
    , parseAssessmentLevel
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (listToMaybe)
import GHC.Generics (Generic)

import Output.Domain.Settings (Language(..), showLanguage)
import Output.LLM.Persona (Persona(..))

-- | What the AI agent is being asked to do in a lesson session.
data AgentTask
    = AssessLevel          -- Determine the student's TOPIK level
    | HaveConversation     -- Conversation practice at current level
    | GenerateVocabulary   -- Teach new vocabulary for current level
    | GenerateSentences    -- Generate practice sentences
    | TeachGrammar         -- Explain a grammar point
    deriving (Show, Eq, Ord, Generic)

instance FromJSON AgentTask
instance ToJSON AgentTask

taskLabel :: AgentTask -> Text
taskLabel AssessLevel        = "AI: Assess My Level"
taskLabel HaveConversation   = "AI: Conversation"
taskLabel GenerateVocabulary = "AI: Vocabulary"
taskLabel GenerateSentences  = "AI: Sentences"
taskLabel TeachGrammar       = "AI: Grammar"

taskMenuDesc :: AgentTask -> Text
taskMenuDesc AssessLevel        = "Let AI determine your TOPIK level"
taskMenuDesc HaveConversation   = "Practice conversation at your level"
taskMenuDesc GenerateVocabulary = "AI teaches new vocabulary for your level"
taskMenuDesc GenerateSentences  = "AI generates practice sentences"
taskMenuDesc TeachGrammar       = "AI explains a Korean grammar point"

-- | Generate the system prompt for a given persona, task, language, and level.
-- mBrief is a pre-computed student brief (quantitative stats + AI summary) that
-- is prepended so the teacher persona has full context on the student.
agentSystemPrompt :: Persona -> AgentTask -> Language -> Int -> Maybe Text -> Text
agentSystemPrompt persona task lang level mBrief = T.unlines $
    personaHeader : "" : briefBlock ++ taskInstructions
  where
    personaHeader = "You are " <> personaName persona
        <> ", a " <> personaStyle persona <> " Korean language teacher."
    briefBlock = case mBrief of
        Nothing    -> []
        Just brief -> ["=== Student Profile ===", brief, "=======================", ""]
    langName = showLanguage lang
    levelStr = T.pack (show level)

    -- Global rules prepended to every task prompt.
    noRomanization =
        [ "STRICT RULE — NO ROMANIZATION: Never write Korean sounds in Latin letters."
        , "Do not write 'annyeonghaseyo', 'gamsahamnida', 'geu', 'neun', or any other"
        , "romanized transcription. Write Korean in Hangul only, always."
        , "This includes greetings, names, and parenthetical pronunciation hints."
        , "WRONG: '안녕하세요 (annyeonghaseyo)'  RIGHT: '안녕하세요'"
        ]

    interfaceRules =
        [ "INTERFACE: This is a terminal text chat. Input is via keyboard only."
        , "There is NO handwriting, NO stroke order, NO physical writing."
        , "Do not ask the student to 'write out' letters or 'practice forming shapes'."
        , "The student CAN type Korean characters (Hangul) directly."
        , "Good exercise types: type a Korean word, read and translate a sentence,"
        , "answer a vocabulary question, complete a sentence, have a conversation."
        ]

    taskInstructions = noRomanization <> [""] <> interfaceRules <> [""] <> case task of
        AssessLevel ->
            [ "Task: Assess the student's Korean proficiency level (TOPIK 1-6)."
            , "The student's native language is " <> langName <> "."
            , "- Ask one question at a time and wait for a response before continuing"
            , "- Start simple (reading Hangul) and increase difficulty with each exchange"
            , "- Provide all explanations in " <> langName
            , ""
            , "QUESTION DESIGN — follow these rules strictly:"
            , "- NEVER put the answer inside the question itself."
            , "  WRONG: 'What letter is this? (ㄱ)'  — the answer is right there."
            , "  RIGHT: 'What sound does ㄱ make?' or 'What does 물 mean?'"
            , "- Test comprehension and production, not visual pattern-matching."
            , "- Good question types:"
            , "  * Show a Korean word, ask for its meaning: '물 — what does this mean?'"
            , "  * Ask the student to type a word: '\"water\" in Korean — type it for me'"
            , "  * Show a sentence and ask for a translation"
            , "  * Fill-in-the-blank: '저는 학생___. (I am a student.)'"
            , "  * Ask what sound a consonant or vowel makes"
            , ""
            , "- After 6-10 exchanges, conclude with EXACTLY this line (no variation):"
            , "  ASSESSMENT COMPLETE: TOPIK Level [1-6]"
            , "  Then explain the result in " <> langName
            , "- Begin by greeting the student in Korean (Hangul only) and ask your first question"
            ]
        HaveConversation ->
            [ "Task: Have a Korean conversation with the student."
            , "The student is at TOPIK level " <> levelStr <> " and speaks " <> langName <> "."
            , "- Speak Korean at TOPIK " <> levelStr <> " difficulty"
            , "- After each Korean sentence add a " <> langName <> " translation in (parentheses)"
            , "- Gently correct mistakes by showing: '정정: X → Y' with " <> langName <> " note"
            , "- Suggest a topic if the student doesn't know what to say"
            , "- Keep the conversation encouraging and natural"
            , "- Begin by greeting the student in Korean and proposing a topic"
            ]
        GenerateVocabulary ->
            [ "Task: Teach 10 vocabulary words suited for TOPIK level " <> levelStr <> "."
            , "The student speaks " <> langName <> "."
            , "- Choose practical, high-frequency words for everyday use at this level"
            , "- Present each word in this format:"
            , "  단어: [Korean]"
            , "  뜻: [" <> langName <> " meaning]"
            , "  예문: [Korean example sentence]"
            , "  번역: [" <> langName <> " translation of the example]"
            , "- After presenting all 10 words, ask if the student wants to practice any"
            , "- Begin by announcing today's vocabulary topic in Korean"
            ]
        GenerateSentences ->
            [ "Task: Teach 10 useful Korean sentences at TOPIK level " <> levelStr <> "."
            , "The student speaks " <> langName <> "."
            , "- Choose sentences useful for real situations at this level"
            , "- Present each sentence in this format:"
            , "  Korean: [sentence in Hangul]"
            , "  " <> langName <> ": [translation]"
            , "  Pattern: [grammar note on the key structure used]"
            , "- After presenting all sentences, offer to test the student on any of them"
            , "- Begin by introducing the situation/theme of today's sentences in Korean"
            ]
        TeachGrammar ->
            [ "Task: Teach one Korean grammar point at TOPIK level " <> levelStr <> "."
            , "The student speaks " <> langName <> "."
            , "- Choose a grammar point important for TOPIK " <> levelStr
            , "- Structure the lesson as:"
            , "  1. Grammar pattern in Korean"
            , "  2. Clear explanation in " <> langName
            , "  3. Five example sentences with " <> langName <> " translations"
            , "  4. Two common mistakes to avoid"
            , "  5. A practice sentence for the student to complete (in Korean)"
            , "- After the student responds to your practice exercise, give feedback"
            , "- Begin by greeting the student in Korean and announcing the grammar topic"
            ]

-- | Parse an assessment result from an AI response.
-- Looks for "ASSESSMENT COMPLETE: TOPIK Level N" and extracts N.
parseAssessmentLevel :: Text -> Maybe Int
parseAssessmentLevel txt
    | "ASSESSMENT COMPLETE: TOPIK Level" `T.isInfixOf` txt =
        listToMaybe $ do
            part <- drop 1 $ T.splitOn "TOPIK Level" txt
            let c = T.take 1 (T.dropWhile (== ' ') part)
            case T.unpack c of
                [n] | n >= '1' && n <= '6' -> [read [n]]
                _ -> []
    | otherwise = Nothing
