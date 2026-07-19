{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Output.Repository.Json
    ( JsonRepository(..)
    , runJsonRepository
    , loadSettings
    , saveSettings
    , loadLearnedWords
    , saveLearnedWord
    ) where

import Data.Aeson (decode, encode, FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Map (Map, fromList, toList)
import qualified Data.Map as Map
import Data.Time (LocalTime, localDay, getCurrentTime, utcToLocalTime, utc)
import Data.Maybe (mapMaybe, catMaybes)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))
import Control.Monad

import Output.Domain.Types
    ( VocabularyId(..)
    , WordClass(..)
    , VocabularyCard(..)
    , VocabularyState(..)
    , UserProgress
    , TypingProgress(..)
    , emptyTypingProgress
    , TOPIK_Level(..)
    )
import Output.Domain.Activity (ActivityEntry(..))
import Output.Domain.Progress (emptyUserProgress)
import Output.Domain.Settings (AppSettings, defaultSettings)
import Output.LLM.Extractor (ExtractedWord(..))
import Output.Repository.Class
    ( ActivityRepository(..)
    , VocabularyRepository(..)
    , UserProgressRepository(..)
    , TypingProgressRepository(..)
    )
import qualified Data.Set as Set

-- | JSON-based repository implementation
newtype JsonRepository a = JsonRepository { unJsonRepository :: IO a }
    deriving (Functor, Applicative, Monad)

-- | Run a JSON repository action
runJsonRepository :: JsonRepository a -> IO a
runJsonRepository = unJsonRepository

instance ActivityRepository JsonRepository where
    logActivity entry = JsonRepository $ do
        createDirectoryIfMissing True "data/user-data"
        let filePath = "data/user-data/activities.jsonl"
        let line = BL.append (encode entry) "\n"
        existing <- doesFileExist filePath
        if existing
            then BL.appendFile filePath line
            else BL.writeFile filePath line

    getActivitiesByDate targetDate = JsonRepository $ do
        activities <- unJsonRepository getAllActivities
        pure $ filter (\a -> localDay (actDate a) == localDay targetDate) activities

    getActivitiesByVocab vocabId = JsonRepository $ do
        activities <- unJsonRepository getAllActivities
        pure $ filter (\a -> actVocabularyId a == Just vocabId) activities

    getActivitiesInRange startDate endDate = JsonRepository $ do
        activities <- unJsonRepository getAllActivities
        pure $ filter (inRange startDate endDate) activities
      where
        inRange start end a =
            let d = actDate a
            in d >= start && d <= end

    getAllActivities = JsonRepository $ do
        let filePath = "data/user-data/activities.jsonl"
        exists <- doesFileExist filePath
        if not exists
            then pure []
            else do
                content <- BL.readFile filePath
                pure (parseActivitiesFromJsonl content)

instance VocabularyRepository JsonRepository where
    saveVocabState state = JsonRepository $ do
        createDirectoryIfMissing True "data/user-data"
        states <- unJsonRepository getAllVocabStates
        let statesList = toList states
        BL.writeFile "data/user-data/vocab-state.json" (encode statesList)

    getVocabState _ = JsonRepository $ do
        let filePath = "data/user-data/vocab-state.json"
        exists <- doesFileExist filePath
        if not exists
            then pure Nothing
            else do
                content <- BL.readFile filePath
                case decode content of
                    Just (states :: [(VocabularyId, VocabularyState)]) ->
                        pure (if null states then Nothing else Just (snd (head states)))
                    Nothing -> pure Nothing

    getAllVocabStates = JsonRepository $ do
        let filePath = "data/user-data/vocab-state.json"
        exists <- doesFileExist filePath
        if not exists
            then pure mempty
            else do
                content <- BL.readFile filePath
                case decode content of
                    Just (states :: [(VocabularyId, VocabularyState)]) ->
                        pure (fromList states)
                    Nothing -> pure mempty

    getVocabCardsForLevel level = JsonRepository $ do
        cards <- unJsonRepository getAllVocabCards
        pure $ filter (\c -> topicLevel c == level) cards

    getAllVocabCards = JsonRepository $ do
        let dir = "data/topik-vocab"
        createDirectoryIfMissing True dir
        -- Load from all level files
        allCards <- forM [One .. Six] $ \level -> do
            let filePath = dir </> ("level" <> show (fromEnum level + 1) <> ".json")
            exists <- doesFileExist filePath
            if not exists
                then pure []
                else do
                    content <- BL.readFile filePath
                    case decode content of
                        Just (cards :: [VocabularyCard]) -> pure cards
                        Nothing -> pure []
        pure (concat allCards)

    getWordsForReview now = JsonRepository $ do
        states <- unJsonRepository getAllVocabStates
        let dueStates = filter (isDue now) (toList states)
        pure (map fst dueStates)
      where
        isDue currentTime (_, vs) = vstNextReviewDate vs <= currentTime

    saveVocabCard card = JsonRepository $ do
        let level = topicLevel card
        let dir = "data/topik-vocab"
        let filePath = dir </> ("level" <> show (fromEnum level + 1) <> ".json")
        createDirectoryIfMissing True dir
        -- Load existing cards, add/update this one, save back
        existingCards <- do
            exists <- doesFileExist filePath
            if not exists
                then pure []
                else do
                    content <- BL.readFile filePath
                    pure $ maybe [] id (decode content :: Maybe [VocabularyCard])
        let updatedCards = card : filter (\c -> vocabId c /= vocabId card) existingCards
        BL.writeFile filePath (encode updatedCards)

instance UserProgressRepository JsonRepository where
    saveProgress progress = JsonRepository $ do
        createDirectoryIfMissing True "data/user-data"
        BL.writeFile "data/user-data/user-progress.json" (encode progress)

    getProgress = JsonRepository $ do
        let filePath = "data/user-data/user-progress.json"
        exists <- doesFileExist filePath
        now <- utcToLocalTime utc <$> getCurrentTime
        if not exists
            then pure (emptyUserProgress now)
            else do
                content <- BL.readFile filePath
                case decode content of
                    Just progress -> pure progress
                    Nothing -> pure (emptyUserProgress now)

-- Helper function to parse JSONL format (one JSON object per line)
parseActivitiesFromJsonl :: BL.ByteString -> [ActivityEntry]
parseActivitiesFromJsonl content =
    mapMaybe decode (BLC.lines content)

instance TypingProgressRepository JsonRepository where
    getTypingProgress = JsonRepository $ do
        let filePath = "data/user-data/typing-progress.json"
        exists <- doesFileExist filePath
        if not exists
            then pure emptyTypingProgress
            else do
                content <- BL.readFile filePath
                case decode content of
                    Just progress -> pure progress
                    Nothing -> pure emptyTypingProgress

    saveTypingProgress progress = JsonRepository $ do
        createDirectoryIfMissing True "data/user-data"
        BL.writeFile "data/user-data/typing-progress.json" (encode progress)

    markLevelCompleted levelNum = JsonRepository $ do
        progress <- unJsonRepository getTypingProgress
        let newProgress = progress { tpCompletedLevels = Set.insert levelNum (tpCompletedLevels progress) }
        unJsonRepository $ saveTypingProgress newProgress

-- | Load app settings from disk, returning defaults if the file is missing or unreadable.
loadSettings :: IO AppSettings
loadSettings = do
    let filePath = "data/user-data/settings.json"
    exists <- doesFileExist filePath
    if not exists
        then pure defaultSettings
        else do
            content <- BL.readFile filePath
            pure $ maybe defaultSettings id (decode content)

-- | Persist app settings to disk.
saveSettings :: AppSettings -> IO ()
saveSettings settings = do
    createDirectoryIfMissing True "data/user-data"
    BL.writeFile "data/user-data/settings.json" (encode settings)

-- | Load all AI-extracted vocabulary words from disk.
loadLearnedWords :: IO [VocabularyCard]
loadLearnedWords = do
    let filePath = "data/user-data/learned-words.json"
    exists <- doesFileExist filePath
    if not exists
        then pure []
        else do
            content <- BL.readFile filePath
            pure $ maybe [] id (decode content)

-- | Persist a new AI-extracted word, assigning it an ID and deduplicating.
-- Returns the saved card (with assigned ID), or Nothing if the word already exists.
saveLearnedWord :: TOPIK_Level -> ExtractedWord -> IO (Maybe VocabularyCard)
saveLearnedWord level ew = do
    existing <- loadLearnedWords
    -- Dedup: skip if Korean text already in the list (from either source)
    if ewKorean ew `elem` map korean existing
        then pure Nothing
        else do
            let existingIds = map (\c -> let VocabularyId n = vocabId c in n) existing
                maxId     = foldr max 100000 existingIds
                nextId    = VocabularyId (maxId + 1)
                newCard   = VocabularyCard
                    { vocabId             = nextId
                    , wordClass           = ewWordClass ew
                    , korean              = ewKorean ew
                    , romanization        = ""
                    , english             = [ewTranslation ew]
                    , topicLevel          = level
                    , exampleSentences    = [ewContext ew]
                    , exampleTranslations = []
                    }
            createDirectoryIfMissing True "data/user-data"
            BL.writeFile "data/user-data/learned-words.json" (encode (existing ++ [newCard]))
            pure (Just newCard)
