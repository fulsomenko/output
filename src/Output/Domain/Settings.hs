{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Output.Domain.Settings
    ( Language(..)
    , showLanguage
    , AppSettings(..)
    , defaultSettings
    ) where

import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Language = English | Swedish
    deriving (Show, Eq, Ord, Generic)

instance FromJSON Language
instance ToJSON Language

showLanguage :: Language -> Text
showLanguage English = "English"
showLanguage Swedish = "Swedish"

data AppSettings = AppSettings
    { settingsLanguage    :: Language
    , settingsKoreanLevel :: Maybe Int   -- 1-6, Nothing = not yet assessed
    , settingsOllamaHost  :: Text
    , settingsOllamaModel :: Text
    } deriving (Show, Eq, Generic)

instance FromJSON AppSettings
instance ToJSON AppSettings

defaultSettings :: AppSettings
defaultSettings = AppSettings
    { settingsLanguage    = English
    , settingsKoreanLevel = Nothing
    , settingsOllamaHost  = "http://localhost:11434"
    , settingsOllamaModel = "gemma3:4b"
    }
