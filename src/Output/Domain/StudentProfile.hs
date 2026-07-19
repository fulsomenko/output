{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Output.Domain.StudentProfile
    ( SessionNote(..)
    , StudentProfile(..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (LocalTime)
import GHC.Generics (Generic)

-- | A single session's observation, written by the summarizer agent.
-- Append-only: these are never rewritten so no information is lost.
data SessionNote = SessionNote
    { snDate    :: LocalTime  -- When the session ended
    , snContent :: Text       -- AI-written paragraph about THIS session only
    } deriving (Show, Eq, Generic)

instance FromJSON SessionNote
instance ToJSON SessionNote

-- | Cumulative student profile: a chronological log of session notes.
-- Each lesson appends one note. Notes are never replaced or deleted.
-- The last N notes are injected into every teacher persona's system prompt
-- so they have full context on the student's progress over time.
data StudentProfile = StudentProfile
    { spNotes       :: [SessionNote]  -- Append-only; oldest first
    , spLastUpdated :: LocalTime
    } deriving (Show, Eq, Generic)

instance FromJSON StudentProfile
instance ToJSON StudentProfile
