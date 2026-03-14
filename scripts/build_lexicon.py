#!/usr/bin/env python3
import argparse, gzip, json, re
from dataclasses import dataclass
from pathlib import Path

import yaml
from wordfreq import zipf_frequency

TOKEN_RE = re.compile(r"^[a-z]+(?:-[a-z]+)*$")
GF_HYPHEN_MARKER = "_H_"

STOPLIST = {
    "i","me","you","he","him","she","her","it","we","us","they","them",
    "a","an","the","this","that","these","those","my","your","his","her",
    "its","our","their","mine","yours","hers","ours","theirs","some"
}

STRUCTURAL_LEMMAS = {
    # Grammar-supplied function words and relation markers.
    "and","or","but","if","as",
    "in","on","with","under","to","from","for","by","before","after",
    "not","no","there","who","that",
}

AUXILIARY_LEMMAS = {
    # Closed-class verbal material that should not be regenerated as ordinary
    # lexical nouns/verbs; these create unwanted ambiguities like
    # "the dog will run" being read as a present-tense transitive clause.
    "be","am","is","are","was","were","been","being",
    "do","does","did","done","doing",
    "have","has","had",
    "will","would","can","could","shall","should","may","might","must",
}

BLOCKED_LEMMAS = STOPLIST | STRUCTURAL_LEMMAS | AUXILIARY_LEMMAS

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

def normalize_form(form: str | None) -> str | None:
    if form is None:
        return None
    form = form.strip()
    if not form:
        return None
    if not form.islower():
        raise ValueError(f"manual form must be lowercase: {form!r}")
    if not is_single_token(form):
        raise ValueError(f"manual form must be a single token: {form!r}")
    if not TOKEN_RE.match(form):
        raise ValueError(f"manual form must match {TOKEN_RE.pattern}: {form!r}")
    return form

def is_single_token(lemma: str) -> bool:
    return " " not in lemma and "_" not in lemma

def normalize_lemma(lemma: str) -> str | None:
    lemma = lemma.strip()
    if not lemma:
        return None
    if not lemma.islower():
        return None
    if not is_single_token(lemma):
        return None
    if not TOKEN_RE.match(lemma):
        return None
    if lemma in BLOCKED_LEMMAS:
        return None
    return lemma

def gf_lexeme_name(lemma: str) -> str:
    return lemma.replace("-", GF_HYPHEN_MARKER)

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

def empty_extras():
    return {
        "include_lemmas": [],
        "nouns": [],
        "verbs": [],
        "adjectives": [],
    }

def load_extras(path: str | None):
    extras = empty_extras()
    if not path:
        return extras

    extras_path = Path(path)
    if not extras_path.exists():
        return extras

    with extras_path.open("r", encoding="utf-8") as f:
        raw = yaml.safe_load(f) or {}

    for lemma in raw.get("include_lemmas", []):
        normalized = normalize_lemma(str(lemma))
        if normalized is None:
            raise ValueError(f"invalid include_lemmas entry: {lemma!r}")
        extras["include_lemmas"].append(normalized)

    for entry in raw.get("nouns", []):
        if isinstance(entry, str):
            entry = {"lemma": entry}
        lemma = normalize_lemma(str(entry.get("lemma", "")))
        if lemma is None:
            raise ValueError(f"invalid manual noun lemma: {entry!r}")
        extras["nouns"].append({
            "lemma": lemma,
            "plural": normalize_form(entry.get("plural")),
        })

    for entry in raw.get("verbs", []):
        if isinstance(entry, str):
            entry = {"lemma": entry}
        lemma = normalize_lemma(str(entry.get("lemma", "")))
        if lemma is None:
            raise ValueError(f"invalid manual verb lemma: {entry!r}")
        extras["verbs"].append({
            "lemma": lemma,
            "third_singular": normalize_form(entry.get("third_singular")),
            "past": normalize_form(entry.get("past")),
            "past_participle": normalize_form(entry.get("past_participle")),
            "present_participle": normalize_form(entry.get("present_participle")),
            "transitive": bool(entry.get("transitive", False)),
        })

    for entry in raw.get("adjectives", []):
        lemma = entry["lemma"] if isinstance(entry, dict) else entry
        normalized = normalize_lemma(str(lemma))
        if normalized is None:
            raise ValueError(f"invalid manual adjective lemma: {entry!r}")
        extras["adjectives"].append(normalized)

    extras["include_lemmas"] = list(dict.fromkeys(extras["include_lemmas"]))
    extras["adjectives"] = list(dict.fromkeys(extras["adjectives"]))
    return extras

def merge_overrides(base_items, extra_items, key):
    merged = list(base_items)
    index_by_key = {item[key]: idx for idx, item in enumerate(merged)}
    for item in extra_items:
        item_key = item[key]
        if item_key in index_by_key:
            merged[index_by_key[item_key]] = item
        else:
            index_by_key[item_key] = len(merged)
            merged.append(item)
    return merged

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--kaikki", required=True)
    ap.add_argument("--max", type=int, default=100000)
    ap.add_argument("--extras", default="data/lexicon_extras.yaml")
    ap.add_argument("--yaml", default="lexicon.yaml")
    ap.add_argument("--abs", default="Grammar/EratoAbs.gf")
    ap.add_argument("--eng", default="Grammar/EratoEng.gf")
    args = ap.parse_args()

    extras = load_extras(args.extras)
    lemmas = {}

    for entry in iter_jsonl(args.kaikki):
        if entry.get("lang") != "English":
            continue
        lemma = entry.get("word")
        lemma = normalize_lemma(lemma or "")
        if lemma is None:
            continue

        pos = (entry.get("pos") or "").lower().strip()
        if pos == "proper noun":
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

        elif pos == "adjective" or pos == "adj" or pos.startswith("adj"):
            info.adj = True

        else:
            continue

        lemmas[lemma] = info

    ranked = sorted(
        lemmas.items(),
        key=lambda kv: zipf_frequency(kv[0], "en"),
        reverse=True
    )
    selected_lemmas = {lemma for lemma, _ in ranked[: args.max]}
    missing_include_lemmas = []
    for lemma in extras["include_lemmas"]:
        if lemma in lemmas:
            selected_lemmas.add(lemma)
        else:
            missing_include_lemmas.append(lemma)
    ranked = [(lemma, info) for lemma, info in ranked if lemma in selected_lemmas]

    nouns = []
    verbs = []
    adjs = []

    for lemma, info in ranked:
        if info.noun:
            if lemma.endswith("s") and not info.noun.plural and lemma not in SINGULAR_S_NOUNS:
                pass
            else:
                nouns.append({
                    "lemma": lemma,
                    "plural": info.noun.plural
                })
        if info.verb:
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

    # de-dup
    def dedup(items, key):
        seen = set()
        out = []
        for item in items:
            k = item[key]
            if k in seen:
                continue
            seen.add(k)
            out.append(item)
        return out

    nouns = dedup(nouns, "lemma")
    verbs = dedup(verbs, "lemma")
    adjs = list(dict.fromkeys(adjs))

    nouns = merge_overrides(nouns, extras["nouns"], "lemma")
    verbs = merge_overrides(verbs, extras["verbs"], "lemma")
    adjs = list(dict.fromkeys(adjs + extras["adjectives"]))

    data = {"nouns": nouns, "verbs": verbs, "adjectives": adjs}

    with open(args.yaml, "w", encoding="utf-8") as f:
        yaml.safe_dump(data, f, sort_keys=False, allow_unicode=True)

    generate_gf(data, args.abs, args.eng)

    if args.extras and Path(args.extras).exists():
        print(f"[ok] loaded extras from {args.extras}")
    if missing_include_lemmas:
        print("[warn] include_lemmas missing from Kaikki: " + ", ".join(missing_include_lemmas))
    print(f"[ok] wrote {args.yaml}")
    print(f"[ok] updated {args.abs} and {args.eng}")

def generate_gf(data, abs_path, eng_path):
    def replace_block(text, start, end, new_block):
        s = text.index(start) + len(start)
        e = text.index(end)
        return text[:s] + "\n" + new_block + "\n" + text[e:]

    funs = []
    for n in data["nouns"]:
        base = gf_lexeme_name(n["lemma"])
        funs.append(f"    {base}_N    : N ;")
        if n["plural"]:
            funs.append(f"    {base}Pl_N  : N ;")
    for v in data["verbs"]:
        base = gf_lexeme_name(v["lemma"])
        funs.append(f"    {base}_V    : V ;")
        if v["third_singular"]:
            funs.append(f"    {base}S_V   : V ;")
        if v["transitive"]:
            funs.append(f"    {base}_V2   : V2 ;")
            if v["third_singular"]:
                funs.append(f"    {base}S_V2  : V2 ;")
    for a in data["adjectives"]:
        a = gf_lexeme_name(a)
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
        lemma = n["lemma"]
        base = gf_lexeme_name(lemma)
        lins.append(
            f"""    {base}_N  = lin CN {{
      s = table {{
        ParamX.Sg => table {{ResEng.Nom => "{lemma}" ; ResEng.Gen => "{lemma}"}} ;
        ParamX.Pl => table {{ResEng.Nom => "{lemma}" ; ResEng.Gen => "{lemma}"}}
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
        lemma = v["lemma"]
        base = gf_lexeme_name(lemma)
        past = v["past"] or (lemma + "ed")
        pp = v["past_participle"] or past
        prp = v["present_participle"] or (lemma + "ing")

        lins.append(f'    {base}_V  = mk5V "{lemma}" "{lemma}" "{past}" "{pp}" "{prp}" ;')
        if v["third_singular"]:
            lins.append(f'    {base}S_V = mk5V "{v["third_singular"]}" "{v["third_singular"]}" "{past}" "{pp}" "{prp}" ;')

        if v["transitive"]:
            lins.append(f'    {base}_V2  = dirV2 (mk5V "{lemma}" "{lemma}" "{past}" "{pp}" "{prp}") ;')
            if v["third_singular"]:
                lins.append(f'    {base}S_V2 = dirV2 (mk5V "{v["third_singular"]}" "{v["third_singular"]}" "{past}" "{pp}" "{prp}") ;')

    for a in data["adjectives"]:
        base = gf_lexeme_name(a)
        lins.append(f'    {base}_A  = mkA "{a}" ;')

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
