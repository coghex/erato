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

### ☐ To implement
- [ ] Proper‑noun inference at runtime
- [ ] Additional verb tenses (past/perfect)
- [ ] Question forms
- [ ] Relative clauses
- [ ] Adverbial modifiers
- [ ] Coordination (NP/VP/S conjunction)
```
