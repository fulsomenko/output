{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Output.Domain.Types
    ( TOPIK_Level(..)
    , VocabularyId(..)
    , WordClass(..)
    , VocabularyCard(..)
    , ExerciseType(..)
    , MasteryLevel(..)
    , VocabularyState(..)
    , defaultEaseFactor
    , newVocabularyState
    , UserProgress(..)
      -- * Typing Progress
    , TypingProgress(..)
    , emptyTypingProgress
    ) where

import Data.Text (Text)
import Data.Time (LocalTime)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Aeson (FromJSON(..), ToJSON(..), FromJSONKey, ToJSONKey, object, withObject, (.:), (.:?), (.!=), (.=))
import GHC.Generics (Generic)

-- | TOPIK proficiency levels
data TOPIK_Level = One | Two | Three | Four | Five | Six
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON TOPIK_Level
instance ToJSON TOPIK_Level

-- | Unique identifier for vocabulary cards
newtype VocabularyId = VocabularyId Int
    deriving (Eq, Ord, Show, Generic)

instance FromJSON VocabularyId
instance ToJSON VocabularyId
instance FromJSONKey VocabularyId
instance ToJSONKey VocabularyId

-- | Grammatical / morphological class of a Korean word.
-- BoundMorpheme covers prefixes, suffixes and Sino-Korean roots that cannot
-- stand alone as words; they should be drilled through compounds, not in isolation.
data WordClass
    = Noun          -- 명사   e.g. 손, 물
    | Verb          -- 동사   e.g. 가다, 먹다
    | Adjective     -- 형용사 (descriptive verb) e.g. 크다, 좋다
    | Adverb        -- 부사   e.g. 빨리, 정말
    | Particle      -- 조사   e.g. 은/는, 이/가
    | BoundMorpheme -- 의존형태소 prefix/suffix/root, cannot stand alone e.g. 선-, -님
    | Counter       -- 수분류사 counting word e.g. 개, 명, 마리
    | Determiner    -- 관형사  e.g. 이, 그, 저
    | Interjection  -- 감탄사  e.g. 아!, 와!
    | Expression    -- 표현    fixed phrase e.g. 감사합니다, 괜찮아요
    | Unknown       -- fallback when class is unclear
    deriving (Show, Eq, Ord, Generic)

instance FromJSON WordClass
instance ToJSON WordClass

-- | A Korean vocabulary card with context
data VocabularyCard = VocabularyCard
    { vocabId             :: VocabularyId
    , wordClass           :: WordClass
    , korean              :: Text
    , romanization        :: Text
    , english             :: [Text]
    , topicLevel          :: TOPIK_Level
    , exampleSentences    :: [Text]
    , exampleTranslations :: [Text]
    } deriving (Show, Eq, Generic)

-- wordClass defaults to Noun for existing TOPIK JSON files that lack the field.
instance FromJSON VocabularyCard where
    parseJSON = withObject "VocabularyCard" $ \o -> VocabularyCard
        <$> o .:  "vocabId"
        <*> (o .:? "wordClass" .!= Noun)
        <*> o .:  "korean"
        <*> (o .:? "romanization" .!= "")
        <*> o .:  "english"
        <*> o .:  "topicLevel"
        <*> (o .:? "exampleSentences"    .!= [])
        <*> (o .:? "exampleTranslations" .!= [])

instance ToJSON VocabularyCard

-- | Types of exercises users can complete
data ExerciseType = Reading | Writing | Typing
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON ExerciseType
instance ToJSON ExerciseType

-- | Mastery levels for a vocabulary word
data MasteryLevel = New | Learning | Intermediate | Mastered
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON MasteryLevel
instance ToJSON MasteryLevel

-- | Current state of a single vocabulary word
data VocabularyState = VocabularyState
    { vstVocabId :: VocabularyId
    , vstMasteryLevel :: MasteryLevel
    , vstLastReviewDate :: Maybe LocalTime
    , vstNextReviewDate :: LocalTime
    , vstReviewCount :: Int
    , vstCorrectCount :: Int
    , vstIncorrectCount :: Int
    , vstEaseFactor :: Double       -- SM-2 ease factor (default 2.5)
    , vstCurrentInterval :: Integer -- Current interval in days
    } deriving (Show, Eq, Generic)

instance FromJSON VocabularyState
instance ToJSON VocabularyState

-- | Default ease factor for SM-2 algorithm
defaultEaseFactor :: Double
defaultEaseFactor = 2.5

-- | Create initial vocabulary state for a new card
newVocabularyState :: VocabularyId -> LocalTime -> VocabularyState
newVocabularyState vid now = VocabularyState
    { vstVocabId = vid
    , vstMasteryLevel = New
    , vstLastReviewDate = Nothing
    , vstNextReviewDate = now  -- Due immediately
    , vstReviewCount = 0
    , vstCorrectCount = 0
    , vstIncorrectCount = 0
    , vstEaseFactor = defaultEaseFactor
    , vstCurrentInterval = 0
    }

-- | Overall user progress and state
data UserProgress = UserProgress
    { upCurrentLevel :: TOPIK_Level
    , upDailyStreak :: Int
    , upLastActivityDate :: Maybe LocalTime
    , upTotalWordsLearned :: Int
    , upTotalWordsReviewed :: Int
    , upVocabularyStates :: Map VocabularyId VocabularyState
    } deriving (Show, Eq, Generic)

instance FromJSON UserProgress
instance ToJSON UserProgress

-- | Progress tracking for typing practice levels
data TypingProgress = TypingProgress
    { tpCompletedLevels :: Set Int  -- Set of level numbers completed at least once
    } deriving (Show, Eq, Generic)

-- | Custom JSON instance for extensible format:
-- { "typingPractice": { "completedLevels": [11, 12, 13] } }
instance ToJSON TypingProgress where
    toJSON progress = object
        [ "typingPractice" .= object
            [ "completedLevels" .= Set.toList (tpCompletedLevels progress)
            ]
        ]

instance FromJSON TypingProgress where
    parseJSON = withObject "TypingProgress" $ \v -> do
        tp <- v .: "typingPractice"
        levels <- tp .: "completedLevels"
        pure $ TypingProgress (Set.fromList levels)

-- | Empty typing progress (nothing completed)
emptyTypingProgress :: TypingProgress
emptyTypingProgress = TypingProgress Set.empty
