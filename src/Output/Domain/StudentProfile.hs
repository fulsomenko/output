{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Output.Domain.StudentProfile
    ( StudentProfile(..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (LocalTime)
import GHC.Generics (Generic)

-- | Cumulative student profile, updated by the summarization agent after each
-- lesson. Stored on disk and prepended to every teacher persona's system prompt
-- as a briefing so new sessions have full context on the student.
data StudentProfile = StudentProfile
    { spSummary     :: Text      -- AI-generated brief: ability, struggles, goals
    , spLastUpdated :: LocalTime -- When this profile was last written
    } deriving (Show, Eq, Generic)

instance FromJSON StudentProfile
instance ToJSON StudentProfile
