{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Output.LLM.Client
    ( OllamaMessage(..)
    , OllamaConfig(..)
    , ollamaFromSettings
    , callOllama
    ) where

import Control.Exception (try, SomeException)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client
import GHC.Generics (Generic)

import Output.Domain.Settings (AppSettings(..))

data OllamaConfig = OllamaConfig
    { ollamaHost  :: Text
    , ollamaModel :: Text
    } deriving (Show, Eq)

ollamaFromSettings :: AppSettings -> OllamaConfig
ollamaFromSettings s = OllamaConfig
    { ollamaHost  = settingsOllamaHost s
    , ollamaModel = settingsOllamaModel s
    }

data OllamaMessage = OllamaMessage
    { role    :: Text
    , content :: Text
    } deriving (Show, Eq, Generic)

instance FromJSON OllamaMessage
instance ToJSON OllamaMessage

data OllamaRequest = OllamaRequest
    { model    :: Text
    , messages :: [OllamaMessage]
    , stream   :: Bool
    } deriving (Generic)

instance ToJSON OllamaRequest

newtype OllamaResponse = OllamaResponse
    { message :: OllamaMessage
    } deriving (Generic)

instance FromJSON OllamaResponse

-- | Send a chat request to Ollama. System prompt is prepended as a system message.
-- Returns Left with error description on failure.
callOllama :: OllamaConfig -> Text -> [OllamaMessage] -> IO (Either Text Text)
callOllama cfg sysPrompt msgs = do
    result <- try go
    case result of
        Left (ex :: SomeException) ->
            pure $ Left $ "Ollama connection failed: " <> T.pack (show ex)
        Right v -> pure v
  where
    go = do
        manager <- newManager defaultManagerSettings
        let url  = T.unpack (ollamaHost cfg) <> "/api/chat"
            allMsgs = OllamaMessage { role = "system", content = sysPrompt } : msgs
            body = encode (OllamaRequest
                { model    = ollamaModel cfg
                , messages = allMsgs
                , stream   = False
                })
        req <- parseRequest url
        let req' = req
                { method         = "POST"
                , requestBody    = RequestBodyLBS body
                , requestHeaders = [("Content-Type", "application/json")]
                }
        resp <- httpLbs req' manager
        case eitherDecode (responseBody resp) of
            Left e  -> pure $ Left $ "Bad Ollama response: " <> T.pack e
            Right r -> pure $ Right $ content (message r)
