{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Output.LLM.Client
    ( OllamaMessage(..)
    , OllamaConfig(..)
    , ollamaFromSettings
    , callOllama
    , streamOllama
    ) where

import Control.Exception (try, SomeException)
import Control.Monad (unless)
import Data.Aeson
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
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

-- | Streaming response chunk from Ollama. Manual FromJSON to avoid field-name
-- collision with OllamaResponse (both have a "message" key in JSON).
data OllamaStreamChunk = OllamaStreamChunk
    { chunkContent :: Text
    , chunkDone    :: Bool
    }

instance FromJSON OllamaStreamChunk where
    parseJSON = withObject "OllamaStreamChunk" $ \o -> do
        msg  <- o .: "message"
        cnt  <- msg .: "content"
        done <- o .: "done"
        pure OllamaStreamChunk { chunkContent = cnt, chunkDone = done }

-- | Stream a chat request to Ollama token by token.
-- Calls onToken for each non-empty content delta.
-- Returns Left on error, Right with the full accumulated text on success.
streamOllama :: OllamaConfig -> Text -> [OllamaMessage] -> (Text -> IO ()) -> IO (Either Text Text)
streamOllama cfg sysPrompt msgs onToken = do
    acc <- newIORef T.empty
    result <- try (go acc)
    case result of
        Left (ex :: SomeException) ->
            pure $ Left $ "Ollama connection failed: " <> T.pack (show ex)
        Right v -> pure v
  where
    go acc = do
        manager <- newManager defaultManagerSettings
        let url     = T.unpack (ollamaHost cfg) <> "/api/chat"
            allMsgs = OllamaMessage { role = "system", content = sysPrompt } : msgs
            body    = encode (OllamaRequest
                { model    = ollamaModel cfg
                , messages = allMsgs
                , stream   = True
                })
        req <- parseRequest url
        let req' = req
                { method          = "POST"
                , requestBody     = RequestBodyLBS body
                , requestHeaders  = [("Content-Type", "application/json")]
                , responseTimeout = responseTimeoutNone
                }
        withResponse req' manager $ \resp ->
            drainBody acc (responseBody resp) ""

    drainBody acc reader buf = do
        bytes <- brRead reader
        if BS.null bytes
            then Right <$> readIORef acc
            else do
                let combined         = buf <> bytes
                    endsWithNewline  = BS.last combined == 10
                    ls               = BSC.lines combined
                    (complete, left) =
                        if endsWithNewline then (ls, "")
                        else if null ls    then ([], combined)
                        else                    (init ls, last ls)
                isDone <- processLines acc complete
                if isDone
                    then Right <$> readIORef acc
                    else drainBody acc reader left

    -- Returns True when a chunk with done=true is encountered (logical end of stream).
    -- Do not rely solely on brRead returning empty: Ollama may not close the HTTP
    -- connection immediately after the final chunk, causing brRead to block.
    processLines _   []     = pure False
    processLines acc (l:ls) = do
        done <- processLine acc l
        if done then pure True else processLines acc ls

    processLine acc line
        | BS.null line = pure False
        | otherwise    = case eitherDecode (BL.fromStrict line) of
            Left _      -> pure False  -- skip stats/malformed chunks
            Right chunk -> do
                let delta = chunkContent chunk
                unless (T.null delta) $ do
                    modifyIORef acc (<> delta)
                    onToken delta
                pure (chunkDone chunk)

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
                { method          = "POST"
                , requestBody     = RequestBodyLBS body
                , requestHeaders  = [("Content-Type", "application/json")]
                , responseTimeout = responseTimeoutNone
                }
        resp <- httpLbs req' manager
        case eitherDecode (responseBody resp) of
            Left e  -> pure $ Left $ "Bad Ollama response: " <> T.pack e
            Right r -> pure $ Right $ content (message r)
