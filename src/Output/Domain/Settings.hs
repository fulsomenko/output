{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Output.Domain.Settings
    ( Language(..)
    , showLanguage
    , AppSettings(..)
    , defaultSettings
    ) where

import Data.Text (Text)
import Data.Aeson (FromJSON(..), ToJSON, withObject, (.:), (.:?), (.!=))
import GHC.Generics (Generic)

data Language = English | Swedish
    deriving (Show, Eq, Ord, Generic)

instance FromJSON Language
instance ToJSON Language

showLanguage :: Language -> Text
showLanguage English = "English"
showLanguage Swedish = "Swedish"

data AppSettings = AppSettings
    { settingsLanguage     :: Language
    , settingsKoreanLevel  :: Maybe Int   -- 1-6, Nothing = not yet assessed
    , settingsOllamaHost   :: Text
    , settingsOllamaModel  :: Text
    , settingsPersonaName  :: Text        -- Instructor display name
    , settingsPersonaStyle :: Text        -- Instructor teaching style description
    } deriving (Show, Eq, Generic)

-- Custom instance so new fields degrade gracefully on old settings.json files.
instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> AppSettings
        <$> o .:  "settingsLanguage"
        <*> o .:? "settingsKoreanLevel"
        <*> (o .:? "settingsOllamaHost"   .!= "http://localhost:11434")
        <*> (o .:? "settingsOllamaModel"  .!= "gemma3:4b")
        <*> (o .:? "settingsPersonaName"  .!= defaultPersonaName)
        <*> (o .:? "settingsPersonaStyle" .!= defaultPersonaStyle)

instance ToJSON AppSettings

defaultPersonaName :: Text
defaultPersonaName = "Teacher Kim (선생님 김)"

defaultPersonaStyle :: Text
defaultPersonaStyle = "patient, structured, and encouraging"

defaultSettings :: AppSettings
defaultSettings = AppSettings
    { settingsLanguage     = English
    , settingsKoreanLevel  = Nothing
    , settingsOllamaHost   = "http://localhost:11434"
    , settingsOllamaModel  = "gemma3:4b"
    , settingsPersonaName  = defaultPersonaName
    , settingsPersonaStyle = defaultPersonaStyle
    }
