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
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Map (Map, fromList, toList)
import qualified Data.Map as Map
import Data.Time (LocalTime, localDay, utcToLocalTime, utc)
import qualified Data.Time as Time
import Data.Maybe (mapMaybe, catMaybes)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))
import Control.Monad

import Output.Domain.Types
    ( VocabularyId
    , VocabularyCard(..)
    , VocabularyState(..)
    , UserProgress
    , TypingProgress(..)
    , emptyTypingProgress
    , TOPIK_Level(..)
    )
import Output.Domain.Activity (ActivityEntry(..))
import Output.Domain.Progress (emptyUserProgress)
import Output.Repository.Class
    ( ActivityRepository(..)
    , VocabularyRepository(..)
    , UserProgressRepository(..)
    , TypingProgressRepository(..)
    , SentenceRepository(..)
    )
import Output.Domain.Sentence (Sentence(..), SentenceId(..))
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
        now <- utcToLocalTime utc <$> Time.getCurrentTime
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

instance SentenceRepository JsonRepository where
    getAllSentences = JsonRepository $ do
        let filePath = "data/user-data/sentences.json"
        exists <- doesFileExist filePath
        if not exists
            then pure []
            else do
                content <- BL.readFile filePath
                case decode content of
                    Just sentences -> pure sentences
                    Nothing -> pure []

    getSentence targetId = JsonRepository $ do
        sentences <- unJsonRepository getAllSentences
        pure $ find (\s -> sentenceId s == targetId) sentences
      where
        find _ [] = Nothing
        find p (x:xs) = if p x then Just x else find p xs

    saveSentence sentence = JsonRepository $ do
        createDirectoryIfMissing True "data/user-data"
        sentences <- unJsonRepository getAllSentences
        let updated = sentence : filter (\s -> sentenceId s /= sentenceId sentence) sentences
        BL.writeFile "data/user-data/sentences.json" (encode updated)

    nextSentenceId = JsonRepository $ do
        sentences <- unJsonRepository getAllSentences
        let maxId = if null sentences
                    then 0
                    else maximum $ map (\s -> let SentenceId n = sentenceId s in n) sentences
        pure $ SentenceId (maxId + 1)

    getCurrentTime = JsonRepository Time.getCurrentTime
