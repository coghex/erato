# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Erato is a controlled English grammar parser built with GF (Grammatical Framework) and a Haskell runtime. It uses a generated lexicon from Wiktionary/Kaikki data and provides a REPL for parsing English sentences into an AST.

## Build & Run Commands

```bash
# Build the GF grammar (required before tests or running)
gf -make -path=.:~/build/gf-rgl/src:~/build/gf-rgl/src/english -output-dir=Grammar Grammar/EratoAbs.gf Grammar/EratoEng.gf

# Build Haskell code
cabal build

# Run all tests (HSpec)
cabal test

# Run the REPL
cabal run
```

Grammar must be compiled to `.pgf` files in `Grammar/` before tests will work. Tests will print "Missing Grammar/*.pgf files; run gf -make first." if grammars aren't compiled.

The `cabal.project` file has `allow-newer` rules for the `gf` package's compatibility with newer GHC.

## Architecture

**Dual Grammar Strategy:** The parser tries a controlled grammar (`EratoAbs`/`EratoEng`) first, then falls back to a broad-coverage grammar (`AllEngAbs`) if that fails.

**Processing Pipeline:** Input text → GF parser → PGF expression → validation (morphology/agreement) → AST (`Parser.AST`) → pretty-printed output (`Parser.Pretty`)

### Key Modules (in `src/`)

- **Parser.GFParser** — Loads grammar bundles (`.pgf` files), drives parsing with controlled + fallback grammars
- **Parser.AST** — Core data types: `Sentence`, `NounPhrase`, `VerbPhrase` with full feature coverage (tense, polarity, person, number, case)
- **Parser.Translate** — Converts GF S-expressions to AST; handles agreement checking and pronoun case validation
- **Parser.Pretty** — Tree-structured pretty printer for AST display
- **Engine.Core.Queue** — STM-based thread-safe queue for async command processing

### Entry Point (`app/Main.hs`)

REPL with two threads (reader + processor). Commands: `:t <sentence>` (parse), `:d` (show AST), `:q` (quit).

### Tests (`test/`)

18 HSpec test modules in `test/Test/`, each receiving a shared `GrammarBundle` loaded once in `test/Main.hs` via `Test.Utils.loadTestGrammars`. Test helpers: `shouldParse`, `shouldReject`, `shouldParseAs`.

### Lexicon Generation (`scripts/`)

`build_lexicon.py` parses Kaikki JSONL Wiktionary dump, ranks by Zipf frequency, and generates GF abstract/concrete grammar files. Requires Python 3 with `wordfreq` and `PyYAML`.

## Code Style

- Uses `GHC2024` language standard and `{-# LANGUAGE Strict, UnicodeSyntax #-}` (unicode arrows `→`, `∷` throughout)
- `-Wall` enabled via cabal common stanza

## Dev Notes

- Coordination is implemented via `MkSCoord`; RGL lacks direct VP coordination; `mkTemp t ASimul` used for VPS
- The GF grammar files (`EratoAbs.gf`, `EratoEng.gf`) are auto-generated from the lexicon script — edit the Python script or grammar templates, not the `.gf` files directly
