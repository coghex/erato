python3.14 scripts/build_lexicon.py \
  --kaikki data/enwiktionary.jsonl.gz \
  --max 100000 \

gf -make -path=.:~/build/gf-rgl/src:~/build/gf-rgl/src/english -output-dir=Grammar Grammar/EratoAbs.gf Grammar/EratoEng.gf
