# Output

A Korean language learning app focused on **output** (reading, writing, typing). Uses spaced repetition to guide you from TOPIK beginner to proficiency.

## Why Output?

- **Spaced Repetition**: SM-2 algorithm schedules reviews when you're about to forget
- **TOPIK Aligned**: Learn vocabulary structured by official TOPIK levels (1-6)
- **Three Exercise Types**: Reading comprehension, translation writing, and Hangul touch-typing
- **Daily Activity Tracking**: See your streak, accuracy trends, and progress toward mastery
- **Type-Safe**: Built in Haskell for correctness; invalid states are impossible at compile time

## Quick Start

### Prerequisites
- [Nix](https://nixos.org/download.html) (for reproducible environment)
- Or: GHC 9.10+, Cabal 3.10+

### Build & Run

```bash
# Using Nix (recommended)
nix develop
cabal build
cabal run output -- help

# Or without Nix
cabal update
cabal build
cabal run output -- help
```

## Project Status

✅ **Phase 1 - Core Foundation (Complete)**
- Domain types & data model
- Spaced repetition algorithm (SM-2)
- Daily exercise generation
- Streak calculation from activity logs
- JSON persistence layer
- CLI skeleton
- 20 TOPIK Level 1 vocabulary words seeded

🚧 **Phase 2 - Exercise & UI (In Progress)**
- Interactive reading exercises
- Translation/writing exercises
- Hangul input practice
- TUI with menu navigation

📋 **Phase 3 - Polish & Advanced**
- Exam simulation mode
- Progress reports & exports
- Keyboard layout support (2-beol, 3-beol)
- Difficulty ramping between TOPIK levels

## Architecture

```
src/Output/
├── Domain/          # Core types & data structures
├── Repository/      # Abstract data persistence (JSON, extensible to DB)
├── Algorithm/       # Spaced rep, exercise generation, streak calc
└── CLI/TUI         # User interfaces
```

Key design: **Activity logs are the source of truth**. Everything (scheduling, stats, progress) derives from logged exercises.

## Development

Run tests:
```bash
cabal test
```

REPL:
```bash
cabal repl
```

Format code:
```bash
cabal exec stylish-haskell -- -i src/**/*.hs test/**/*.hs
```

## Data Format

**Vocabulary**: `data/topik-vocab/level{1-6}.json`
```json
{
  "vocabId": {"unVocabularyId": 1},
  "korean": "안녕하세요",
  "romanization": "annyeonghaseyo",
  "english": ["Hello", "Hi"],
  "topicLevel": "One",
  "exampleSentences": ["..."],
  "exampleTranslations": ["..."]
}
```

**User Data**: `data/user-data/` (not committed)
- `activities.jsonl` - exercise log
- `vocabulary-state.json` - word mastery states
- `user-progress.json` - user profile

## Next Steps

1. Implement exercise runners (reading, writing, typing)
2. Build TUI with `brick`
3. Wire spaced rep into daily exercise selection
4. Add Korean input handling

## License

Apache License 2.0 - See [LICENSE](LICENSE) file for details

## Contributing

All contributions welcome! Fork, submit PRs, open issues.

---

Built with Haskell, Cabal, and ❤️ for Korean language learners.
