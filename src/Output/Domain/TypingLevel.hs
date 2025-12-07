{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Output.Domain.TypingLevel
    ( -- * Level Types
      TypingLevel(..)
    , UnlockCriteria(..)
    , LevelStats(..)
      -- * All Levels
    , allLevels
    , mainLevels
    , getLevel
    , getLevelByNumber
      -- * Sub-Levels
    , getSubLevels
    , hasSubLevels
    , level1SubLevels
      -- * Level Queries
    , keysForLevel
    , newKeysAtLevel
    , isLevelUnlocked
    , defaultUnlockCriteria
      -- * Level Names
    , levelName
    , levelDescription
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Output.Domain.Jamo

-- | A typing practice level
data TypingLevel = TypingLevel
    { tlNumber :: Int           -- Level number (1-8)
    , tlName :: Text            -- Display name
    , tlDescription :: Text     -- What this level teaches
    , tlKeys :: Set Jamo        -- All keys available at this level
    , tlNewKeys :: Set Jamo     -- Keys introduced at this level
    , tlUnlockCriteria :: UnlockCriteria
    } deriving (Eq, Show, Generic)

instance FromJSON TypingLevel
instance ToJSON TypingLevel

-- | Criteria to unlock the next level
data UnlockCriteria = UnlockCriteria
    { ucMinAccuracy :: Double   -- Minimum accuracy percentage (e.g., 85.0)
    , ucMinCompletions :: Int   -- Number of times level must be completed
    , ucMinWPM :: Double        -- Minimum words per minute
    } deriving (Eq, Show, Generic)

instance FromJSON UnlockCriteria
instance ToJSON UnlockCriteria

-- | Statistics for a level
data LevelStats = LevelStats
    { lsCompletions :: Int
    , lsBestAccuracy :: Double
    , lsBestWPM :: Double
    , lsAverageAccuracy :: Double
    , lsAverageWPM :: Double
    , lsTotalTimeMs :: Int
    } deriving (Eq, Show, Generic)

instance FromJSON LevelStats
instance ToJSON LevelStats

-- | Default unlock criteria
defaultUnlockCriteria :: UnlockCriteria
defaultUnlockCriteria = UnlockCriteria
    { ucMinAccuracy = 85.0
    , ucMinCompletions = 3
    , ucMinWPM = 10.0
    }

-- | Level 1 Sub-levels (11-19): Comprehensive Home Row Curriculum
-- These provide a structured path through all home row syllable combinations

-- | Level 1.1: Finger Drills (warmup)
level1_1 :: TypingLevel
level1_1 = TypingLevel
    { tlNumber = 11
    , tlName = "1.1 Finger Drills"
    , tlDescription = "Build muscle memory with progressive finger exercises"
    , tlKeys = homeRowJamo
    , tlNewKeys = homeRowJamo
    , tlUnlockCriteria = defaultUnlockCriteria { ucMinAccuracy = 70.0, ucMinWPM = 3.0 }
    }

-- | Level 1.2: All CV Syllables (20 syllables)
level1_2 :: TypingLevel
level1_2 = TypingLevel
    { tlNumber = 12
    , tlName = "1.2 CV Syllables"
    , tlDescription = "All 20 consonant-vowel combinations: 마 머 모 미, 나 너 노 니..."
    , tlKeys = homeRowJamo
    , tlNewKeys = Set.empty
    , tlUnlockCriteria = defaultUnlockCriteria { ucMinAccuracy = 75.0, ucMinWPM = 5.0 }
    }

-- | Level 1.3: CV Syllable Pairs (2-syllable words)
level1_3 :: TypingLevel
level1_3 = TypingLevel
    { tlNumber = 13
    , tlName = "1.3 CV Pairs"
    , tlDescription = "Two-syllable combinations: 나라, 머리, 하나, 오리..."
    , tlKeys = homeRowJamo
    , tlNewKeys = Set.empty
    , tlUnlockCriteria = defaultUnlockCriteria { ucMinAccuracy = 78.0, ucMinWPM = 6.0 }
    }

-- | Level 1.4: CVC with ㄴ batchim
level1_4 :: TypingLevel
level1_4 = TypingLevel
    { tlNumber = 14
    , tlName = "1.4 CVC (ㄴ)"
    , tlDescription = "CVC syllables with ㄴ final: 만 먼 몬 민, 난 넌 논 닌..."
    , tlKeys = homeRowJamo
    , tlNewKeys = Set.empty
    , tlUnlockCriteria = defaultUnlockCriteria { ucMinAccuracy = 80.0, ucMinWPM = 6.0 }
    }

-- | Level 1.5: CVC with ㅁ batchim
level1_5 :: TypingLevel
level1_5 = TypingLevel
    { tlNumber = 15
    , tlName = "1.5 CVC (ㅁ)"
    , tlDescription = "CVC syllables with ㅁ final: 맘 멈 몸 밈, 남 넘 놈 님..."
    , tlKeys = homeRowJamo
    , tlNewKeys = Set.empty
    , tlUnlockCriteria = defaultUnlockCriteria { ucMinAccuracy = 80.0, ucMinWPM = 6.0 }
    }

-- | Level 1.6: CVC with ㅇ batchim
level1_6 :: TypingLevel
level1_6 = TypingLevel
    { tlNumber = 16
    , tlName = "1.6 CVC (ㅇ)"
    , tlDescription = "CVC syllables with ㅇ final: 망 멍 몽 밍, 낭 넝 농 닝..."
    , tlKeys = homeRowJamo
    , tlNewKeys = Set.empty
    , tlUnlockCriteria = defaultUnlockCriteria { ucMinAccuracy = 80.0, ucMinWPM = 6.0 }
    }

-- | Level 1.7: CVC with ㄹ batchim
level1_7 :: TypingLevel
level1_7 = TypingLevel
    { tlNumber = 17
    , tlName = "1.7 CVC (ㄹ)"
    , tlDescription = "CVC syllables with ㄹ final: 말 멀 몰 밀, 날 널 놀 닐..."
    , tlKeys = homeRowJamo
    , tlNewKeys = Set.empty
    , tlUnlockCriteria = defaultUnlockCriteria { ucMinAccuracy = 80.0, ucMinWPM = 6.0 }
    }

-- | Level 1.8: Mixed CVC Review
level1_8 :: TypingLevel
level1_8 = TypingLevel
    { tlNumber = 18
    , tlName = "1.8 CVC Mixed"
    , tlDescription = "Mixed practice with all batchim types"
    , tlKeys = homeRowJamo
    , tlNewKeys = Set.empty
    , tlUnlockCriteria = defaultUnlockCriteria { ucMinAccuracy = 82.0, ucMinWPM = 7.0 }
    }

-- | Level 1.9: Multi-syllable Words
level1_9 :: TypingLevel
level1_9 = TypingLevel
    { tlNumber = 19
    , tlName = "1.9 Home Row Words"
    , tlDescription = "Real words using all home row combinations: 인간, 나란히, 온라인..."
    , tlKeys = homeRowJamo
    , tlNewKeys = Set.empty
    , tlUnlockCriteria = defaultUnlockCriteria { ucMinAccuracy = 85.0, ucMinWPM = 8.0 }
    }

-- | Legacy Level 1 (kept for compatibility but now points users to sub-levels)
level1 :: TypingLevel
level1 = TypingLevel
    { tlNumber = 1
    , tlName = "Home Row Jamo"
    , tlDescription = "Learn the home row keys: ㅁ ㄴ ㅇ ㄹ ㅎ ㅗ ㅓ ㅏ ㅣ"
    , tlKeys = homeRowJamo
    , tlNewKeys = homeRowJamo
    , tlUnlockCriteria = defaultUnlockCriteria { ucMinAccuracy = 80.0, ucMinWPM = 5.0 }
    }

-- | Level 2: Simple CV Syllables
level2 :: TypingLevel
level2 = TypingLevel
    { tlNumber = 2
    , tlName = "Simple Syllables"
    , tlDescription = "Combine consonants and vowels: 나 너 아 오 이"
    , tlKeys = homeRowJamo
    , tlNewKeys = Set.empty  -- Same keys, but now forming syllables
    , tlUnlockCriteria = defaultUnlockCriteria { ucMinWPM = 8.0 }
    }

-- | Level 3: Top Row
level3 :: TypingLevel
level3 = TypingLevel
    { tlNumber = 3
    , tlName = "Top Row"
    , tlDescription = "Add top row: ㅂ ㅈ ㄷ ㄱ ㅅ ㅛ ㅕ ㅑ ㅐ ㅔ"
    , tlKeys = Set.union homeRowJamo topRowJamo
    , tlNewKeys = topRowJamo
    , tlUnlockCriteria = defaultUnlockCriteria
    }

-- | Level 4: Bottom Row
level4 :: TypingLevel
level4 = TypingLevel
    { tlNumber = 4
    , tlName = "Bottom Row"
    , tlDescription = "Add bottom row: ㅋ ㅌ ㅊ ㅍ ㅠ ㅜ ㅡ"
    , tlKeys = Set.unions [homeRowJamo, topRowJamo, bottomRowJamo]
    , tlNewKeys = bottomRowJamo
    , tlUnlockCriteria = defaultUnlockCriteria
    }

-- | Level 5: Batchim (Final Consonants)
level5 :: TypingLevel
level5 = TypingLevel
    { tlNumber = 5
    , tlName = "Batchim"
    , tlDescription = "Practice final consonants (받침): 한 문 강 물"
    , tlKeys = Set.unions [homeRowJamo, topRowJamo, bottomRowJamo]
    , tlNewKeys = Set.empty  -- Same keys, new syllable structure
    , tlUnlockCriteria = defaultUnlockCriteria { ucMinAccuracy = 88.0 }
    }

-- | Level 6: Double Consonants
level6 :: TypingLevel
level6 = TypingLevel
    { tlNumber = 6
    , tlName = "Double Consonants"
    , tlDescription = "Add shift keys: ㅃ ㅉ ㄸ ㄲ ㅆ"
    , tlKeys = Set.unions [homeRowJamo, topRowJamo, bottomRowJamo, allDoubleConsonants]
    , tlNewKeys = allDoubleConsonants
    , tlUnlockCriteria = defaultUnlockCriteria { ucMinAccuracy = 85.0 }
    }

-- | Level 7: Complex Vowels
level7 :: TypingLevel
level7 = TypingLevel
    { tlNumber = 7
    , tlName = "Complex Vowels"
    , tlDescription = "Compound vowels: ㅘ ㅙ ㅚ ㅝ ㅞ ㅟ ㅢ"
    , tlKeys = Set.unions [homeRowJamo, topRowJamo, bottomRowJamo, allDoubleConsonants, complexVowels]
    , tlNewKeys = complexVowels
    , tlUnlockCriteria = defaultUnlockCriteria { ucMinAccuracy = 85.0 }
    }
  where
    complexVowels = Set.fromList $ map Vowel "ㅘㅙㅚㅝㅞㅟㅢ"

-- | Level 8: Full Vocabulary
level8 :: TypingLevel
level8 = TypingLevel
    { tlNumber = 8
    , tlName = "Full Vocabulary"
    , tlDescription = "Mixed practice with all keys and vocabulary"
    , tlKeys = Set.unions [allConsonants, allVowels, allDoubleConsonants]
    , tlNewKeys = Set.empty
    , tlUnlockCriteria = UnlockCriteria
        { ucMinAccuracy = 90.0
        , ucMinCompletions = 5
        , ucMinWPM = 20.0
        }
    }

-- | All typing levels (sub-levels first, then main progression)
allLevels :: [TypingLevel]
allLevels =
    -- Level 1 sub-levels (the main learning path)
    [ level1_1, level1_2, level1_3, level1_4, level1_5
    , level1_6, level1_7, level1_8, level1_9
    -- Main progression levels (after mastering home row)
    , level2, level3, level4, level5, level6, level7, level8
    ]

-- | Main levels (shown in top-level selector)
mainLevels :: [TypingLevel]
mainLevels = [level1, level2, level3, level4, level5, level6, level7, level8]

-- | Sub-levels for Level 1 (Home Row)
level1SubLevels :: [TypingLevel]
level1SubLevels = [level1_1, level1_2, level1_3, level1_4, level1_5
                  , level1_6, level1_7, level1_8, level1_9]

-- | Get sub-levels for a parent level
getSubLevels :: Int -> [TypingLevel]
getSubLevels 1 = level1SubLevels
getSubLevels _ = []  -- Other levels have no sub-levels yet

-- | Check if a level has sub-levels
hasSubLevels :: Int -> Bool
hasSubLevels n = not (null (getSubLevels n))

-- | Get level by number (searches all levels including main levels)
getLevel :: Int -> Maybe TypingLevel
getLevel n = case filter (\l -> tlNumber l == n) (allLevels ++ mainLevels) of
    (level:_) -> Just level
    [] -> Nothing

-- | Alias for getLevel
getLevelByNumber :: Int -> Maybe TypingLevel
getLevelByNumber = getLevel

-- | Get all keys available at a given level
keysForLevel :: Int -> Set Jamo
keysForLevel n = case getLevel n of
    Just level -> tlKeys level
    Nothing -> Set.empty

-- | Get keys newly introduced at a given level
newKeysAtLevel :: Int -> Set Jamo
newKeysAtLevel n = case getLevel n of
    Just level -> tlNewKeys level
    Nothing -> Set.empty

-- | Check if a level is unlocked based on previous level stats
isLevelUnlocked :: Int -> Maybe LevelStats -> Bool
isLevelUnlocked 1 _ = True  -- Level 1 always unlocked
isLevelUnlocked n Nothing = False  -- No stats = locked
isLevelUnlocked n (Just stats) =
    case getLevel (n - 1) of
        Nothing -> False
        Just prevLevel ->
            let criteria = tlUnlockCriteria prevLevel
            in lsBestAccuracy stats >= ucMinAccuracy criteria
               && lsCompletions stats >= ucMinCompletions criteria
               && lsBestWPM stats >= ucMinWPM criteria

-- | Get display name for level
levelName :: Int -> Text
levelName n = case getLevel n of
    Just level -> "Level " <> T.pack (show n) <> ": " <> tlName level
    Nothing -> "Unknown Level"

-- | Get description for level
levelDescription :: Int -> Text
levelDescription n = case getLevel n of
    Just level -> tlDescription level
    Nothing -> ""
