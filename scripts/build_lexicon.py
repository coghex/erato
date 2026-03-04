#!/usr/bin/env python3
import argparse, gzip, json, re
from dataclasses import dataclass

import yaml
from wordfreq import zipf_frequency

LEMMA_RE = re.compile(r"^[a-z]+$")

STOPLIST = {
    "i","me","you","he","him","she","her","it","we","us","they","them",
    "a","an","the","this","that","these","those","my","your","his","her",
    "its","our","their","mine","yours","hers","ours","theirs"
}

SINGULAR_S_NOUNS = {
    "news","series","species","physics","mathematics","economics","politics","ethics",
    "aerobics","measles","mumps","gymnastics","electronics"
}

@dataclass
class VerbInfo:
    transitive: bool = False
    third_singular: str | None = None
    past: str | None = None
    past_participle: str | None = None
    present_participle: str | None = None

@dataclass
class NounInfo:
    plural: str | None = None

@dataclass
class LemmaInfo:
    noun: NounInfo | None = None
    verb: VerbInfo | None = None
    adj: bool = False

def is_single_token(lemma: str) -> bool:
    return " " not in lemma and "-" not in lemma and "_" not in lemma

def normalize_lemma(lemma: str) -> str | None:
    lemma = lemma.strip()
    if not lemma:
        return None
    if not lemma.islower():
        return None
    if not is_single_token(lemma):
        return None
    if not LEMMA_RE.match(lemma):
        return None
    if lemma in STOPLIST:
        return None
    return lemma

def iter_jsonl(path):
    opener = gzip.open if path.endswith(".gz") else open
    with opener(path, "rt", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                yield json.loads(line)
            except json.JSONDecodeError:
                continue

def find_verb_forms(entry):
    forms = entry.get("forms") or []
    third = None
    past = None
    pp = None
    prp = None
    for f in forms:
        form = f.get("form")
        tags = set(f.get("tags") or [])
        if not form:
            continue
        if {"third-person", "singular", "present"} <= tags:
            third = third or form
        if "past" in tags and past is None:
            past = form
        if {"past", "participle"} <= tags and pp is None:
            pp = form
        if {"present", "participle"} <= tags and prp is None:
            prp = form
    return third, past, pp, prp

def find_noun_plural(entry):
    forms = entry.get("forms") or []
    for f in forms:
        form = f.get("form")
        tags = set(f.get("tags") or [])
        if not form:
            continue
        if "plural" in tags:
            return form
    return None

def detect_transitive(entry):
    senses = entry.get("senses") or []
    for s in senses:
        tags = set(s.get("tags") or [])
        if "transitive" in tags:
            return True
    return False

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--kaikki", required=True)
    ap.add_argument("--max", type=int, default=100000)
    ap.add_argument("--yaml", default="lexicon.yaml")
    ap.add_argument("--abs", default="Grammar/EratoAbs.gf")
    ap.add_argument("--eng", default="Grammar/EratoEng.gf")
    args = ap.parse_args()

    lemmas = {}

    for entry in iter_jsonl(args.kaikki):
        if entry.get("lang") != "English":
            continue
        lemma = entry.get("word")
        lemma = normalize_lemma(lemma or "")
        if lemma is None:
            continue

        pos = (entry.get("pos") or "").lower()
        if pos == "proper noun":
            continue
        if pos not in {"noun", "verb", "adjective"}:
            continue

        info = lemmas.get(lemma) or LemmaInfo()
        if pos == "noun":
            if info.noun is None:
                info.noun = NounInfo()
            if info.noun.plural is None:
                info.noun.plural = find_noun_plural(entry)
        elif pos == "verb":
            if info.verb is None:
                info.verb = VerbInfo()
            third, past, pp, prp = find_verb_forms(entry)
            if third and info.verb.third_singular is None:
                info.verb.third_singular = third
            if past and info.verb.past is None:
                info.verb.past = past
            if pp and info.verb.past_participle is None:
                info.verb.past_participle = pp
            if prp and info.verb.present_participle is None:
                info.verb.present_participle = prp
            if detect_transitive(entry):
                info.verb.transitive = True
        elif pos == "adjective":
            info.adj = True

        lemmas[lemma] = info

    ranked = sorted(
        lemmas.items(),
        key=lambda kv: zipf_frequency(kv[0], "en"),
        reverse=True
    )
    ranked = ranked[: args.max]

    nouns = []
    verbs = []
    adjs = []

    for lemma, info in ranked:
        if info.noun:
            # drop likely plural-as-lemma nouns (e.g., "dogs")
            if lemma.endswith("s") and not info.noun.plural and lemma not in SINGULAR_S_NOUNS:
                pass
            else:
                nouns.append({
                    "lemma": lemma,
                    "plural": info.noun.plural
                })
        if info.verb:
            # skip likely inflected-verb lemmas like "runs" if no 3sg form
            if info.verb.third_singular is None and lemma.endswith("s"):
                continue
            verbs.append({
                "lemma": lemma,
                "third_singular": info.verb.third_singular,
                "past": info.verb.past,
                "past_participle": info.verb.past_participle,
                "present_participle": info.verb.present_participle,
                "transitive": info.verb.transitive,
            })
        if info.adj:
            adjs.append(lemma)

    data = {"nouns": nouns, "verbs": verbs, "adjectives": adjs}

    with open(args.yaml, "w", encoding="utf-8") as f:
        yaml.safe_dump(data, f, sort_keys=False, allow_unicode=True)

    generate_gf(data, args.abs, args.eng)

    print(f"[ok] wrote {args.yaml}")
    print(f"[ok] updated {args.abs} and {args.eng}")

def generate_gf(data, abs_path, eng_path):
    def replace_block(text, start, end, new_block):
        s = text.index(start) + len(start)
        e = text.index(end)
        return text[:s] + "\n" + new_block + "\n" + text[e:]

    funs = []
    for n in data["nouns"]:
        base = n["lemma"]
        funs.append(f"    {base}_N    : N ;")
        if n["plural"]:
            funs.append(f"    {base}Pl_N  : N ;")
    for v in data["verbs"]:
        base = v["lemma"]
        # emit both V and V2 if transitive appears
        funs.append(f"    {base}_V    : V ;")
        if v["third_singular"]:
            funs.append(f"    {base}S_V   : V ;")
        if v["transitive"]:
            funs.append(f"    {base}_V2   : V2 ;")
            if v["third_singular"]:
                funs.append(f"    {base}S_V2  : V2 ;")
    for a in data["adjectives"]:
        funs.append(f"    {a}_A    : A ;")

    with open(abs_path, "r", encoding="utf-8") as f:
        abs_text = f.read()
    abs_text = replace_block(
        abs_text,
        "-- BEGIN LEXICON FUNS (generated)",
        "-- END LEXICON FUNS (generated)",
        "\n".join(funs)
    )
    with open(abs_path, "w", encoding="utf-8") as f:
        f.write(abs_text)

    lins = []
    for n in data["nouns"]:
        base = n["lemma"]
        lins.append(
            f"""    {base}_N  = lin CN {{
      s = table {{
        ParamX.Sg => table {{ResEng.Nom => "{base}" ; ResEng.Gen => "{base}"}} ;
        ParamX.Pl => table {{ResEng.Nom => "{base}" ; ResEng.Gen => "{base}"}}
      }} ;
      g = ResEng.Neutr ;
      lock_CN = {{}}
    }} ;
"""
        )
        if n["plural"]:
            pl = n["plural"]
            lins.append(
                f"""    {base}Pl_N = lin CN {{
      s = table {{
        ParamX.Sg => table {{ResEng.Nom => "{pl}" ; ResEng.Gen => "{pl}"}} ;
        ParamX.Pl => table {{ResEng.Nom => "{pl}" ; ResEng.Gen => "{pl}"}}
      }} ;
      g = ResEng.Neutr ;
      lock_CN = {{}}
    }} ;
"""
            )
    for v in data["verbs"]:
        base = v["lemma"]
        past = v["past"] or (base + "ed")
        pp = v["past_participle"] or past
        prp = v["present_participle"] or (base + "ing")

        lins.append(f'    {base}_V  = mkBaseV "{base}" "{past}" "{pp}" "{prp}" ;')
        if v["third_singular"]:
            lins.append(f'    {base}S_V = mk3sgV "{v["third_singular"]}" "{past}" "{pp}" "{prp}" ;')

        if v["transitive"]:
            lins.append(f'    {base}_V2  = dirV2 (mkBaseV "{base}" "{past}" "{pp}" "{prp}") ;')
            if v["third_singular"]:
                lins.append(f'    {base}S_V2 = dirV2 (mk3sgV "{v["third_singular"]}" "{past}" "{pp}" "{prp}") ;')

    for a in data["adjectives"]:
        lins.append(f'    {a}_A  = mkA "{a}" ;')

    with open(eng_path, "r", encoding="utf-8") as f:
        eng_text = f.read()
    eng_text = replace_block(
        eng_text,
        "-- BEGIN LEXICON LINS (generated)",
        "-- END LEXICON LINS (generated)",
        "\n".join(lins)
    )
    with open(eng_path, "w", encoding="utf-8") as f:
        f.write(eng_text)

if __name__ == "__main__":
    main()
