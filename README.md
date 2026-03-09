# Erato

A compact, controlled English grammar and parser built in GF with a generated lexicon and a Haskell runtime.

## Requirements

- GF (Grammatical Framework)
- Haskell toolchain (cabal)
- Python 3 (for lexicon generation)
- Wordfreq + PyYAML (Python packages)

## Build & Run (Grammar + Tests)

```bash
# build grammar
gf -make -path=.:~/build/gf-rgl/src:~/build/gf-rgl/src/english -output-dir=Grammar Grammar/EratoAbs.gf Grammar/EratoEng.gf

# run tests
cabal test

# run the REPL
cabal run
```

## Corpus Testing (user-supplied .txt files)

Use the corpus runner to measure controlled/fallback parse coverage over real text files without changing the existing Hspec suite.

```bash
# single files
cabal run erato-corpus -- --file corpora/book1.txt --file corpora/book2.txt

# recursively scan a directory of .txt files
cabal run erato-corpus -- --dir corpora/children

# enforce minimum rates for CI-style checks
cabal run erato-corpus -- \
  --dir corpora/children \
  --max-sentences 2000 \
  --stop-after-unparsed 100 \
  --min-controlled-rate 0.60 \
  --min-total-rate 0.90 \
  --show-failures 30
```

The runner reports:
- total sentence count,
- controlled parse rate,
- fallback parse rate,
- unparsed rate,
- first N unparsed examples with file/line context.

Useful speed controls:
- `--max-sentences N` to cap how much text is processed,
- `--stop-after-unparsed N` to bail out early once enough failures are seen.

## Generate Lexicon (from Kaikki/Wiktionary)

### Python environment setup (full process)

```bash
python3 -m venv sandbox
source sandbox/bin/activate
pip3 install PyYAML
pip3 install wordfreq
```

### Build lexicon + regenerate grammar

```bash
python3 scripts/build_lexicon.py \
  --kaikki data/enwiktionary.jsonl.gz \
  --max 100000 \
  --yaml lexicon.yaml \
  --abs Grammar/EratoAbs.gf \
  --eng Grammar/EratoEng.gf

gf -make -path=.:~/build/gf-rgl/src:~/build/gf-rgl/src/english -output-dir=Grammar Grammar/EratoAbs.gf Grammar/EratoEng.gf
```

> The Kaikki file is a large JSONL dump. Keep it outside the repo and add to `.gitignore`.

---

dev note:
Coordination implemented via MkSCoord
RGL lacks direct VP coordination
mkTemp t ASimul used for VPS

## Checklist

### ✅ Implemented
- [x] Pronoun subject parsing (e.g., “I run”)
- [x] Pronoun object parsing (e.g., “the man eats her”)
- [x] Rejects object case as subject (e.g., “them eats the food”)
- [x] Negation parsing (e.g., “the dog does not run”)
- [x] Plural agreement parsing (e.g., “the dogs run”)
- [x] Agreement mismatch rejection (e.g., “the dogs runs”)
- [x] Lexicon generation from Kaikki (Wiktionary) + wordfreq ranking
- [x] Generated lexicon integrated into GF grammar
- [x] Coordination (NP/VP/S conjunction)
- [x] Adverbial modifiers
- [x] Relative clauses

### ☐ To implement
- [ ] Proper‑noun inference at runtime
- [x] Additional verb tenses (past/perfect)
- [x] Question forms (yes/no + basic wh-questions)
- [x] negation inside RCs (e.g., “the dog that does not run is big”)
- [x] possessives (determiners + basic 's genitives)
- [x] infinitival + that-clause complements
- [x] quantifiers (e.g., “every/all/many/no dog(s) ...”)
```
