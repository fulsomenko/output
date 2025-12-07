{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Output.Domain.TypingWord
    ( -- * Types
      TypingWord(..)
    , TypingVocabFile(..)
      -- * Loading
    , loadTypingWords
    , loadAllTypingWords
      -- * Filtering
    , filterByKeys
    , wordsForLevel
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Aeson (FromJSON(..), ToJSON, eitherDecodeFileStrict, withObject, (.:))
import GHC.Generics (Generic)
import System.FilePath ((</>))
import System.Directory (doesFileExist)

import Output.Domain.Jamo

-- | A word for typing practice
data TypingWord = TypingWord
    { twKorean :: Text           -- The Korean word
    , twEnglish :: Text          -- English translation
    , twRomanization :: Text     -- Romanized pronunciation
    , twJamo :: [Jamo]           -- Pre-decomposed jamo sequence
    , twKeysUsed :: Set Jamo     -- Set of unique jamo in this word
    , twLevel :: Int             -- Source level (1-8)
    } deriving (Eq, Show, Generic)

instance FromJSON TypingWord
instance ToJSON TypingWord

-- | Raw word data from JSON
data RawTypingWord = RawTypingWord
    { rtwKorean :: Text
    , rtwEnglish :: Text
    , rtwRomanization :: Text
    } deriving (Eq, Show, Generic)

instance FromJSON RawTypingWord where
    parseJSON = withObject "RawTypingWord" $ \v -> RawTypingWord
        <$> v .: "korean"
        <*> v .: "english"
        <*> v .: "romanization"

instance ToJSON RawTypingWord where

-- | JSON file structure for typing vocabulary
data TypingVocabFile = TypingVocabFile
    { tvfLevel :: Int
    , tvfName :: Text
    , tvfDescription :: Text
    , tvfWords :: [RawTypingWord]
    } deriving (Eq, Show, Generic)

instance FromJSON TypingVocabFile where
    parseJSON = withObject "TypingVocabFile" $ \v -> TypingVocabFile
        <$> v .: "level"
        <*> v .: "name"
        <*> v .: "description"
        <*> v .: "words"

instance ToJSON TypingVocabFile where

-- | Convert raw word to TypingWord with computed jamo
rawToTypingWord :: Int -> RawTypingWord -> TypingWord
rawToTypingWord level raw = TypingWord
    { twKorean = rtwKorean raw
    , twEnglish = rtwEnglish raw
    , twRomanization = rtwRomanization raw
    , twJamo = jamos
    , twKeysUsed = Set.fromList jamos
    , twLevel = level
    }
  where
    jamos = decomposeKorean (rtwKorean raw)

-- | Load typing words for a specific level
loadTypingWords :: FilePath -> Int -> IO [TypingWord]
loadTypingWords dataDir level = do
    let filename = levelFilename level
        filepath = dataDir </> "typing-vocab" </> filename
    exists <- doesFileExist filepath
    if not exists
        then return []
        else do
            result <- eitherDecodeFileStrict filepath
            case result of
                Left err -> do
                    putStrLn $ "Error loading typing vocab: " ++ err
                    return []
                Right vocabFile ->
                    return $ map (rawToTypingWord level) (tvfWords vocabFile)

-- | Get the full filename for each level
levelFilename :: Int -> String
-- Level 1 sub-levels (11-19) use format: level1.X-name.json
levelFilename 11 = "level1.1-drills.json"
levelFilename 12 = "level1.2-cv.json"
levelFilename 13 = "level1.3-cv-pairs.json"
levelFilename 14 = "level1.4-cvc-n.json"
levelFilename 15 = "level1.5-cvc-m.json"
levelFilename 16 = "level1.6-cvc-ng.json"
levelFilename 17 = "level1.7-cvc-l.json"
levelFilename 18 = "level1.8-cvc-mixed.json"
levelFilename 19 = "level1.9-words.json"
-- Main levels use format: levelN-name.json
levelFilename n = "level" ++ show n ++ "-" ++ levelSuffix n ++ ".json"

-- | Get the filename suffix for main levels
levelSuffix :: Int -> String
levelSuffix 1 = "jamo"
levelSuffix 2 = "cv"
levelSuffix 3 = "toprow"
levelSuffix 4 = "bottomrow"
levelSuffix 5 = "batchim"
levelSuffix 6 = "double"
levelSuffix 7 = "complex"
levelSuffix 8 = "full"
levelSuffix _ = "unknown"

-- | Load all typing words from all levels
loadAllTypingWords :: FilePath -> IO [TypingWord]
loadAllTypingWords dataDir = do
    -- Load sub-levels (11-19) and main levels (2-8)
    -- Skip level 1 since it's replaced by sub-levels
    let allLevelNumbers = [11..19] ++ [2..8]
    wordLists <- mapM (loadTypingWords dataDir) allLevelNumbers
    return $ concat wordLists

-- | Filter words by available keys (only include words using subset of given keys)
filterByKeys :: Set Jamo -> [TypingWord] -> [TypingWord]
filterByKeys availableKeys = filter canType
  where
    canType word = twKeysUsed word `Set.isSubsetOf` availableKeys

-- | Get words appropriate for a given level
wordsForLevel :: Int -> [TypingWord] -> [TypingWord]
wordsForLevel level = filter (\w -> twLevel w <= level)
