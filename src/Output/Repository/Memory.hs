{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Output.Repository.Memory
    ( MemoryRepository
    , newMemoryRepository
    , runMemoryRepository
    ) where

import Data.Map (Map, empty, insert, lookup, toList)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Time (LocalTime)
import Prelude hiding (lookup)

import Output.Domain.Types
    ( VocabularyId
    , VocabularyCard
    , VocabularyState(..)
    , UserProgress
    )
import Output.Domain.Activity (ActivityEntry)
import Output.Domain.Progress (emptyUserProgress)
import Output.Repository.Class
    ( ActivityRepository(..)
    , VocabularyRepository(..)
    , UserProgressRepository(..)
    )

-- | In-memory repository for testing
data MemoryStore = MemoryStore
    { activities :: IORef [ActivityEntry]
    , vocabularyStates :: IORef (Map VocabularyId VocabularyState)
    , vocabularyCards :: IORef [VocabularyCard]
    , userProgress :: IORef UserProgress
    }

newtype MemoryRepository a = MemoryRepository { unMemoryRepository :: MemoryStore -> IO a }
    deriving (Functor)

instance Applicative MemoryRepository where
    pure x = MemoryRepository (\_ -> pure x)
    (MemoryRepository f) <*> (MemoryRepository a) = MemoryRepository $ \store -> do
        fn <- f store
        val <- a store
        pure (fn val)

instance Monad MemoryRepository where
    (MemoryRepository m) >>= f = MemoryRepository $ \store -> do
        val <- m store
        unMemoryRepository (f val) store

-- | Create a new empty memory repository
newMemoryRepository :: LocalTime -> IO MemoryStore
newMemoryRepository now = do
    acts <- newIORef []
    vocabs <- newIORef empty
    cards <- newIORef []
    prog <- newIORef (emptyUserProgress now)
    pure $ MemoryStore acts vocabs cards prog

-- | Run a memory repository action
runMemoryRepository :: MemoryStore -> MemoryRepository a -> IO a
runMemoryRepository store (MemoryRepository f) = f store

instance ActivityRepository MemoryRepository where
    logActivity entry = MemoryRepository $ \store -> do
        modifyIORef (activities store) (++ [entry])

    getActivitiesByDate _ = MemoryRepository $ \store -> do
        readIORef (activities store)

    getActivitiesByVocab _ = MemoryRepository $ \store -> do
        readIORef (activities store)

    getActivitiesInRange _ _ = MemoryRepository $ \store -> do
        readIORef (activities store)

    getAllActivities = MemoryRepository $ \store -> do
        readIORef (activities store)

instance VocabularyRepository MemoryRepository where
    saveVocabState state = MemoryRepository $ \store -> do
        modifyIORef (vocabularyStates store) (insert (vstVocabId state) state)

    getVocabState vid = MemoryRepository $ \store -> do
        states <- readIORef (vocabularyStates store)
        pure (lookup vid states)

    getAllVocabStates = MemoryRepository $ \store -> do
        readIORef (vocabularyStates store)

    getVocabCardsForLevel _ = MemoryRepository $ \store -> do
        readIORef (vocabularyCards store)

    getAllVocabCards = MemoryRepository $ \store -> do
        readIORef (vocabularyCards store)

    getWordsForReview _ = MemoryRepository $ \store -> do
        states <- readIORef (vocabularyStates store)
        pure (map fst (toList states))

    saveVocabCard card = MemoryRepository $ \store -> do
        modifyIORef (vocabularyCards store) (++ [card])

instance UserProgressRepository MemoryRepository where
    saveProgress prog = MemoryRepository $ \store -> do
        writeIORef (userProgress store) prog

    getProgress = MemoryRepository $ \store -> do
        readIORef (userProgress store)
