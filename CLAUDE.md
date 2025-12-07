# CLAUDE.md

## Project: Output

Korean language learning CLI/TUI. Brick-based. Spaced repetition (SM-2).

## Build

```bash
stack build
stack exec output
stack test
```

## Architecture

```
src/
  Core/       -- Pure logic, no IO
  Data/       -- Parsing, persistence
  UI/         -- Brick app, widgets, events
app/Main.hs   -- Entry point
data/         -- CSV/TOML drill files
```

## Conventions

- `Text` over `String`
- Explicit exports
- No partial functions (`head`, `!!` → use `NonEmpty`, safe indexing)
- `newtype` for domain types
- MTL-style for effects when needed
- Lens for nested state updates in Brick

## Key Types

```haskell
data Exercise = VocabExercise VocabDrill | ParticleExercise ParticleDrill | ...
data Card     = Card { exercise :: Exercise, interval :: Int, easiness :: Double, ... }
data AppState = AppState { screen :: Screen, cards :: [Card], stats :: SessionStats }
```

## Brick Patterns

- `AppState` is the single source of truth
- `drawUI :: AppState -> [Widget Name]` is pure
- `handleEvent` returns `EventM Name (Next AppState)`
- Use `continue`, `halt`, `suspendAndResume`

## Data Files

- `data/vocab.csv` — vocabulary (korean,english,level,hint)
- `data/particles.toml` — fill-in-the-blank drills
- Parsed with `cassava` (CSV) and `tomland` (TOML)

## SM-2 Algorithm

```haskell
updateCard :: Card -> Quality -> Card  -- Quality ∈ [0..5]
-- interval: 1 → 6 → EF * prev
-- easiness: EF' = EF + (0.1 - (5-q) * (0.08 + (5-q) * 0.02))
```

## Don't

- No orphan instances
- No `unsafePerformIO` (except tests)
- No stringly-typed data
- Avoid `IO` in `Core/`
