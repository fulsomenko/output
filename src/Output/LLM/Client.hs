{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Output.LLM.Client
    ( OllamaMessage(..)
    , OllamaConfig(..)
    , ollamaFromSettings
    , callOllama
    , streamOllama
    , listModels
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
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusIsSuccessful, statusCode)
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

-- | Error body Ollama sends instead of a chat stream, e.g. an unpulled model.
newtype OllamaErrorResponse = OllamaErrorResponse Text

instance FromJSON OllamaErrorResponse where
    parseJSON = withObject "OllamaErrorResponse" $ \o ->
        OllamaErrorResponse <$> o .: "error"

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
            if statusIsSuccessful (responseStatus resp)
                then drainBody acc (responseBody resp) ""
                else do
                    errBody <- brReadAll (responseBody resp) ""
                    pure $ Left $ "Ollama error (" <> T.pack (show (statusCode (responseStatus resp)))
                        <> "): " <> describeErrorBody errBody

    brReadAll reader acc' = do
        bytes <- brRead reader
        if BS.null bytes then pure acc' else brReadAll reader (acc' <> bytes)

    describeErrorBody raw = case eitherDecode (BL.fromStrict raw) of
        Right (OllamaErrorResponse msg) -> msg
        Left _                          -> TE.decodeUtf8With TE.lenientDecode raw

    drainBody acc reader buf = do
        bytes <- brRead reader
        if BS.null bytes
            then pure $ Left "Ollama stream ended unexpectedly (no response received)"
            else do
                let combined         = buf <> bytes
                    endsWithNewline  = BS.last combined == 10
                    ls               = BSC.lines combined
                    (complete, left) =
                        if endsWithNewline then (ls, "")
                        else if null ls    then ([], combined)
                        else                    (init ls, last ls)
                result <- processLines acc complete
                case result of
                    Left err    -> pure $ Left err
                    Right True  -> Right <$> readIORef acc
                    Right False -> drainBody acc reader left

    -- Returns Right True when a chunk with done=true is encountered (logical end of
    -- stream), Right False to keep reading, or Left on an error chunk. Do not rely
    -- solely on brRead returning empty: Ollama may not close the HTTP connection
    -- immediately after the final chunk, causing brRead to block.
    processLines _   []     = pure (Right False)
    processLines acc (l:ls) = do
        result <- processLine acc l
        case result of
            Right False -> processLines acc ls
            other       -> pure other

    processLine acc line
        | BS.null line = pure (Right False)
        | otherwise    = case eitherDecode (BL.fromStrict line) of
            Right chunk -> do
                let delta = chunkContent chunk
                unless (T.null delta) $ do
                    modifyIORef acc (<> delta)
                    onToken delta
                pure (Right (chunkDone chunk))
            Left _ -> case eitherDecode (BL.fromStrict line) of
                Right (OllamaErrorResponse msg) -> pure (Left ("Ollama error: " <> msg))
                Left _                          -> pure (Right False) -- skip stats/malformed chunks

-- | A model entry as reported by Ollama's /api/tags.
newtype OllamaModelInfo = OllamaModelInfo Text

instance FromJSON OllamaModelInfo where
    parseJSON = withObject "OllamaModelInfo" $ \o ->
        OllamaModelInfo <$> o .: "name"

newtype OllamaTagsResponse = OllamaTagsResponse [OllamaModelInfo]

instance FromJSON OllamaTagsResponse where
    parseJSON = withObject "OllamaTagsResponse" $ \o ->
        OllamaTagsResponse <$> o .: "models"

-- | List model names available on the configured Ollama host.
listModels :: OllamaConfig -> IO (Either Text [Text])
listModels cfg = do
    result <- try go
    case result of
        Left (ex :: SomeException) ->
            pure $ Left $ "Ollama connection failed: " <> T.pack (show ex)
        Right v -> pure v
  where
    go = do
        manager <- newManager defaultManagerSettings
        let url = T.unpack (ollamaHost cfg) <> "/api/tags"
        req  <- parseRequest url
        resp <- httpLbs req manager
        if statusIsSuccessful (responseStatus resp)
            then case eitherDecode (responseBody resp) of
                Left e -> pure $ Left $ "Bad Ollama response: " <> T.pack e
                Right (OllamaTagsResponse models) ->
                    pure $ Right [name | OllamaModelInfo name <- models]
            else pure $ Left $ "Ollama error (" <> T.pack (show (statusCode (responseStatus resp))) <> ")"

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
