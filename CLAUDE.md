# CLAUDE.md

## Project: Output

Korean language learning CLI/TUI. Brick-based. Spaced repetition (SM-2). Features vocabulary drills (reading/writing) and typing practice with Hangul jamo composition.

## Build & Run

```bash
cabal build
cabal run output -- --tui   # TUI mode (default: CLI)
cabal test
```

## Architecture

```
src/Output/
  Domain/           -- Pure domain types (no IO)
    Types.hs        -- VocabularyCard, UserProgress, TypingProgress, TOPIK_Level
    Exercise.hs     -- ExercisePrompt, ExerciseResult, generateExercisePrompt
    Session.hs      -- Session state, SessionResult
    Activity.hs     -- ActivityEntry, Performance logging
    Jamo.hs         -- Korean letter system, QWERTY→Hangul mapping, syllable composition
    TypingLevel.hs  -- 8 main levels + 9 sub-levels, unlock criteria
    TypingWord.hs   -- TypingWord, vocabulary loading from JSON
    TypingExercise.hs -- TypingPrompt, validation, WPM calculation
    Progress.hs     -- UserProgress helpers

  Algorithm/        -- Pure algorithms
    SpacedRepetition.hs  -- SM-2: calculateNextReviewDate, calculateIntervalDays
    Streak.hs            -- calculateStreak from activity history
    ExerciseGen.hs       -- generateDailyExercises, selectExercisesForDay

  Repository/       -- Persistence layer (typeclass + implementations)
    Class.hs        -- ActivityRepository, VocabularyRepository, UserProgressRepository
    Json.hs         -- JsonRepository: file-based JSON/JSONL persistence
    Memory.hs       -- MemoryRepository: IORef-based (for testing)

  TUI/              -- Brick terminal UI
    Types.hs        -- AppState, Screen, DrillState, TypingState, Name
    App.hs          -- Brick app definition, runTUI, attribute map
    Draw.hs         -- drawUI dispatcher, screen renderers
    Events.hs       -- handleEvent router, per-screen handlers
    Widgets/
      Keyboard.hs       -- Visual Korean keyboard with finger coloring
      TypingPractice.hs -- Typing UI, level selector

  CLI.hs            -- Command-line interface
  App.hs            -- Shared app logic

app/Main.hs         -- Entry point: args → TUI or CLI
test/               -- Hspec tests for algorithms
data/
  topik-vocab/      -- level1.json through level6.json (VocabularyCard[])
  typing-vocab/     -- level1.1-drills.json through level8-full.json
  user-data/        -- Runtime: user-progress.json, typing-progress.json, activities.jsonl
```

## Key Types

```haskell
-- Domain/Types.hs
data TOPIK_Level = One | Two | Three | Four | Five | Six
newtype VocabularyId = VocabularyId Int
data VocabularyCard = VocabularyCard
    { vocabId, korean, romanization, english :: [Text], topicLevel, exampleSentences, exampleTranslations }
data VocabularyState = VocabularyState
    { vstVocabId, vstMasteryLevel, vstLastReviewDate, vstNextReviewDate, vstReviewCount, vstCorrectCount, vstIncorrectCount }
data MasteryLevel = New | Learning | Intermediate | Mastered
data UserProgress = UserProgress
    { upCurrentLevel, upDailyStreak, upLastActivityDate, upTotalWordsLearned, upTotalWordsReviewed, upVocabularyStates :: Map VocabularyId VocabularyState }
data TypingProgress = TypingProgress { tpCompletedLevels :: Set Int }

-- Domain/Jamo.hs
data Jamo = Consonant Char | Vowel Char | DoubleConsonant Char
data ComposeState = Empty | HasInitial Jamo | HasSyllable Jamo Jamo | HasFinal Jamo Jamo Jamo
-- qwertyToJamo :: Char -> Maybe Jamo  (2-beolsik keyboard mapping)
-- composeJamoSequence :: [Jamo] -> Text  (IME-like syllable composition)

-- TUI/Types.hs
data Screen = MainMenuScreen | DrillScreen | TypingPracticeScreen | TypingLevelSelectScreen | ProgressScreen | HelpScreen | QuitConfirmScreen
data Name = MenuList | ExerciseInput | TypingInput | KeyboardView | LevelSelector | ...
data AppState = AppState
    { asScreen, asDrill :: Maybe DrillState, asTyping :: Maybe TypingState
    , asMenuIndex, asVocabCards, asProgress, asMessage, asTypingWords, asTypingProgress }
data TypingState = TypingState
    { typLevel, typPrompts, typCurrentIndex, typTypedJamo :: [Jamo], typCharStatuses :: [CharStatus], typStats, ... }
```

## SM-2 Algorithm (Algorithm/SpacedRepetition.hs)

```haskell
calculateIntervalDays :: Int -> Double -> Integer
-- reviewCount < 1       → 1 day
-- reviewCount == 1, rate >= 0.7 → 3 days
-- otherwise             → ceiling(baseInterval * easeFactor)
--   where easeFactor = 2.5 - (5 - quality) * (0.08 + (5 - quality) * 0.02)
--         quality = clamp 0 5 (correctRate * 5)

updateMasteryLevel :: Int -> Int -> MasteryLevel
-- 0 reviews       → New
-- < 3 reviews     → Learning
-- rate >= 0.85    → Mastered
-- rate >= 0.65    → Intermediate
-- otherwise       → Learning
```

## Repository Pattern

```haskell
-- Repository/Class.hs (typeclasses)
class Monad m => VocabularyRepository m where
    getAllVocabCards :: m [VocabularyCard]
    getVocabCardsForLevel :: TOPIK_Level -> m [VocabularyCard]
    saveVocabState :: VocabularyState -> m ()
    ...

-- Repository/Json.hs (production implementation)
newtype JsonRepository a = JsonRepository { unJsonRepository :: IO a }
runJsonRepository :: JsonRepository a -> IO a

-- File paths (hardcoded):
-- data/topik-vocab/level<N>.json     -- N = 1-6
-- data/typing-vocab/level<N>-*.json  -- N = 2-8, or level1.<N>-*.json for sub-levels
-- data/user-data/user-progress.json
-- data/user-data/typing-progress.json
-- data/user-data/activities.jsonl   -- JSONL format (one entry per line)
```

## Brick Patterns

```haskell
-- TUI/App.hs
app :: App AppState AppEvent Name
app = App { appDraw = drawUI, appHandleEvent = handleEvent, appChooseCursor = neverShowCursor, ... }

-- drawUI dispatches by asScreen, returns [Widget Name]
-- handleEvent routes by asScreen to per-screen handlers
-- Event handlers use: get, modify, put, liftIO (for repository calls)

-- Attribute names: titleAttr, menuSelectedAttr, promptAttr, correctAttr, incorrectAttr, hintAttr
```

## Conventions

- `Text` over `String`
- Explicit exports in all modules
- `newtype` wrappers for IDs: `VocabularyId`, `Percentage`
- All domain types derive `Generic`, `FromJSON`, `ToJSON`
- Repository abstraction: typeclasses in `Class.hs`, implementations separate
- Pure algorithms in `Algorithm/` — no IO
- Tests use Hspec

## Data Files (JSON via Aeson)

```json
// data/topik-vocab/level1.json
[{ "vocabId": 1, "korean": "안녕하세요", "romanization": "annyeonghaseyo",
   "english": ["Hello"], "topicLevel": "One", "exampleSentences": [...] }]

// data/typing-vocab/level1.1-drills.json
{ "level": 11, "name": "Finger Drills", "description": "...",
  "words": [{ "korean": "ㅁㅁㅁ", "english": "left pinky drill", "romanization": "mmm" }] }

// data/user-data/typing-progress.json
{ "typingPractice": { "completedLevels": [11, 12] } }
```

## Tests (Hspec)

```bash
cabal test
# Tests Algorithm/SpacedRepetition.hs and Algorithm/Streak.hs
# test/Main.hs composes specs from test/Output/Algorithm/*Spec.hs
```

### TDD Workflow (mandatory — Red → Green → Refactor)

1. **Red**: Write a failing test that specifies the expected behavior. Present tests to the user for review before implementing anything.
2. **Green**: Write the minimum implementation needed to make the test pass. Do not over-engineer at this step.
3. **Refactor**: Clean up implementation and tests without breaking anything. This step is not optional.
4. No feature or fix is complete until all tests pass and the refactor step is done.

**Test naming:** Names are living documentation. Use the pattern `describe/it` with descriptive strings, e.g. `describe "calculateIntervalDays" $ it "returns 1 day for first review"`. Avoid generic names like `test1`.

**Test placement:** Pure domain and algorithm logic goes in `test/Output/Algorithm/` or `test/Output/Domain/` as inline Hspec specs. Repository behaviour uses `MemoryRepository` (IORef-based) — no real file I/O needed in tests.

## Don't

- No orphan instances
- No `unsafePerformIO`
- No stringly-typed data
- Avoid `IO` in `Domain/` and `Algorithm/`
- Avoid partial functions (`head`, `!!`) — use safe alternatives
