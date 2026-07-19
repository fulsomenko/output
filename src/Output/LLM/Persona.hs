{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Output.LLM.Persona
    ( Persona(..)
    , teacherKim
    , allPersonas
    ) where

import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- | An instructor persona. The persona's name and style are woven into
-- system prompts to give the AI a consistent character.
data Persona = Persona
    { personaName  :: Text   -- e.g. "Teacher Kim (선생님 김)"
    , personaStyle :: Text   -- e.g. "patient and structured"
    } deriving (Show, Eq, Generic)

instance FromJSON Persona
instance ToJSON Persona

-- | Default persona: a patient, structured Korean teacher.
teacherKim :: Persona
teacherKim = Persona
    { personaName  = "Teacher Kim (선생님 김)"
    , personaStyle = "patient, structured, and encouraging"
    }

-- | All available personas (extend as needed).
allPersonas :: [Persona]
allPersonas = [teacherKim]
