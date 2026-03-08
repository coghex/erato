# Copilot Instructions

## Build and test

- Build everything with `cabal build all`.
- Run the full test suite with `cabal test erato-tests`.
- Run a targeted Hspec group with `cabal test erato-tests --test-options='--match "Pronouns"'`; the match strings come from the `describe` labels in `test/Test/*.hs`.
- Run the REPL with `cabal run erato`. The REPL commands are `:t <sentence>`, `:d`, and `:q`.
- Rebuild the GF grammar after editing `Grammar/EratoAbs.gf` or `Grammar/EratoEng.gf`:
  ```bash
  gf -make -path=.:~/build/gf-rgl/src:~/build/gf-rgl/src/english -output-dir=Grammar Grammar/EratoAbs.gf Grammar/EratoEng.gf
  ```
- Regenerate the lexicon and the generated GF sections with:
  ```bash
  python3 -m pip install -r scripts/lexicon_requirements.txt
  python3 scripts/build_lexicon.py --kaikki data/enwiktionary.jsonl.gz --max 100000 --yaml lexicon.yaml --abs Grammar/EratoAbs.gf --eng Grammar/EratoEng.gf
  ```
- There is no dedicated lint command; the cabal build uses `-Wall`, so `cabal build all` is the closest project-wide check.

## High-level architecture

- This project is a controlled-English pipeline built from two layers:
  - GF grammars in `Grammar/` define the surface language.
  - Haskell modules in `src/` load compiled `.pgf` files, parse GF expressions into an internal AST, and render that AST into the output language.
- `app/Main.hs` is a small REPL. It loads both `Grammar/EratoAbs.pgf` and `Grammar/AllEngAbs.pgf`, then uses a queue (`Engine.Core.Queue`) to separate input handling from parse/translate work.
- `Parser.GFParser` owns grammar loading and parse entrypoints. `parseControlled` uses the controlled grammar plus `validateExpr`; `parseFallbackAllEng` uses the broader fallback grammar when the controlled parse fails.
- `Parser.Translate` is the core bridge between GF and Haskell. It converts `showExpr` output into a small s-expression model, matches specific GF constructors (`MkS`, `Pred`, `UseV`, `UseV2`, `RelCN`, `MkSCoord`, `UttImp`, etc.), enforces agreement rules, and builds the `Parser.AST` types.
- `Parser.AST` is the canonical internal model. Most feature work lands here and in `Parser.Translate`, not in the REPL.
- `Parser.Pretty` powers `:d` by printing the last successful AST as a tree.
- The fallback grammar is `Grammar/AllEngAbs.gf`, while the controlled grammar is `Grammar/EratoAbs.gf` + `Grammar/EratoEng.gf`. The compiled `.pgf` files are already checked in, and the tests/loaders depend on those compiled artifacts being present.
- Tests in `test/` are integration-style grammar checks. `test/Main.hs` loads the same grammar bundle the REPL uses, and each `test/Test/*.hs` module covers one grammar feature area.

## Repository-specific conventions

- Every Haskell module currently starts with `{-# LANGUAGE Strict, UnicodeSyntax #-}` and uses Unicode syntax in type signatures and arrows. Follow that existing style in new modules.
- Treat `Grammar/EratoAbs.gf` and `Grammar/EratoEng.gf` as partly generated files. Only edit the hand-written grammar around the `BEGIN/END LEXICON` markers manually; regenerate the marked sections with `scripts/build_lexicon.py`.
- Grammar changes usually require matching Haskell changes. If you introduce new GF constructors or change existing tree shapes, update `Parser.Translate` so `exprToSentence` and `validateExpr` still recognize them.
- Unknown proper nouns are intentionally supported through `SymbPN`/`UsePN`; preserve that path when changing parse validation or noun-phrase handling.
- Tests use `Test.Utils` helpers (`shouldParse`, `shouldReject`, `shouldParseAs`) and pass a shared `GrammarBundle` into each module's `spec`. Keep new grammar coverage in feature-focused test modules rather than adding ad hoc assertions to `test/Main.hs`.
