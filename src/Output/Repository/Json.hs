{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Output.Repository.Json
    ( JsonRepository(..)
    , runJsonRepository
    ) where

import Data.Aeson (decode, encode, FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map, fromList, toList)
import Data.Time (LocalTime)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Control.Monad

import Output.Domain.Types
    ( VocabularyId
    , VocabularyCard
    , VocabularyState
    , UserProgress
    , TOPIK_Level
    )
import Output.Domain.Activity (ActivityEntry)
import Output.Domain.Progress (emptyUserProgress)
import Output.Repository.Class
    ( ActivityRepository(..)
    , VocabularyRepository(..)
    , UserProgressRepository(..)
    )

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

    getActivitiesByDate _ = JsonRepository $ do
        let filePath = "data/user-data/activities.jsonl"
        exists <- doesFileExist filePath
        if not exists
            then pure []
            else do
                content <- BL.readFile filePath
                pure (parseActivitiesFromJsonl content)

    getActivitiesByVocab _ = JsonRepository $ do
        activities <- unJsonRepository getAllActivities
        pure activities  -- Filter would happen here in real implementation

    getActivitiesInRange _ _ = JsonRepository $ do
        activities <- unJsonRepository getAllActivities
        pure activities  -- Filter would happen here

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

    getVocabCardsForLevel _ = JsonRepository $ do
        cards <- unJsonRepository getAllVocabCards
        pure cards  -- Filter would happen here

    getAllVocabCards = JsonRepository $ do
        let dir = "data/topik-vocab"
        createDirectoryIfMissing True dir
        -- This would load from multiple JSON files per TOPIK level
        pure []  -- Placeholder

    getWordsForReview _ = JsonRepository $ do
        states <- unJsonRepository getAllVocabStates
        pure (map fst (toList states))  -- Placeholder: would filter by date

    saveVocabCard _card = JsonRepository $ do
        -- Save to appropriate level file
        pure ()

instance UserProgressRepository JsonRepository where
    saveProgress progress = JsonRepository $ do
        createDirectoryIfMissing True "data/user-data"
        BL.writeFile "data/user-data/user-progress.json" (encode progress)

    getProgress = JsonRepository $ do
        let filePath = "data/user-data/user-progress.json"
        exists <- doesFileExist filePath
        if not exists
            then error "User progress file not found - initialize with emptyUserProgress"
            else do
                content <- BL.readFile filePath
                case decode content of
                    Just progress -> pure progress
                    Nothing -> error "Failed to parse user progress"

-- Helper function to parse JSONL format
parseActivitiesFromJsonl :: BL.ByteString -> [ActivityEntry]
parseActivitiesFromJsonl _ = []  -- Placeholder implementation
