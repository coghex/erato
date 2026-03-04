{-# LANGUAGE Strict, UnicodeSyntax #-}

module Parser.Translate
  ( exprToSentence
  , exprProperNouns
  , translateSentence
  , translateFallback
  ) where

import PGF (Expr, showExpr)
import Data.Char (isAlpha, isSpace, toLower)
import Parser.AST

-- Public-API-only: parse from showExpr
exprToSentence ∷ Expr → Maybe Sentence
exprToSentence expr = do
  sexp <- parseSExp (showExpr [] expr)
  parseSentence sexp

exprProperNouns ∷ Expr → [String]
exprProperNouns expr =
  case parseSExp (showExpr [] expr) of
    Nothing   -> []
    Just sexp -> collectSymbPN sexp

collectSymbPN ∷ SExp → [String]
collectSymbPN (List [Atom "SymbPN", StrLit s]) = [s]
collectSymbPN (List xs) = concatMap collectSymbPN xs
collectSymbPN _ = []

-- S-expression model for parsing showExpr output
data SExp
  = Atom String
  | StrLit String
  | List [SExp]
  deriving (Eq, Show)

data VerbForm = BaseForm | ThirdSingular
  deriving (Eq, Show)

parseSentence ∷ SExp → Maybe Sentence
parseSentence (List [Atom "UttS", s]) =
  parseSentence s
parseSentence (List [Atom "MkS", t, p, cl]) = do
  tense'    <- parseTense t
  polarity' <- parsePolarity p
  (subj, vp, vform) <- parseCl cl
  if agreementOk tense' polarity' subj vform
    then pure (Sentence tense' polarity' subj vp)
    else Nothing
parseSentence _ = Nothing

parseTense ∷ SExp → Maybe Tense
parseTense (Atom "TPres") = Just Present
parseTense (Atom "TPast") = Just Past
parseTense (Atom "TFut")  = Just Future
parseTense _              = Nothing

parsePolarity ∷ SExp → Maybe Polarity
parsePolarity (Atom "PPos") = Just Positive
parsePolarity (Atom "PNeg") = Just Negative
parsePolarity (Atom "UncNeg") = Just Negative
parsePolarity _             = Nothing

parseCl ∷ SExp → Maybe (NounPhrase, VerbPhrase, VerbForm)
parseCl (List [Atom "Pred", np, vp]) = do
  subj <- parseNP Subjective np
  (vps, vf) <- parseVP vp
  pure (subj, vps, vf)
parseCl _ = Nothing

parseNP ∷ PronounCase → SExp → Maybe NounPhrase
parseNP _ (List [Atom "DetCN", det, n]) = do
  (detText, detNum) <- parseDetInfo det
  (nounNum, adjs, noun) <- parseN n
  case detNum of
    Just dnum | dnum /= nounNum -> Nothing
    _ -> pure (CommonNoun (Just detText) adjs noun (maybe nounNum id detNum))
parseNP _ (List [Atom "UseN", n]) = do
  (num, adjs, noun) <- parseN n
  pure (CommonNoun Nothing adjs noun num)
parseNP _ (List [Atom "UsePN", pn]) = ProperNoun <$> parsePN pn
parseNP c (List [Atom "UsePron", pr]) = do
  (p, n) <- parsePron pr
  pure (Pronoun p n c)
parseNP _ _ = Nothing

parseN ∷ SExp → Maybe (Number, [String], String)
parseN (List [Atom "AdjCN", a, n]) = do
  adj <- parseA a
  (num, adjs, noun) <- parseN n
  pure (num, adj : adjs, noun)
parseN (Atom a)
  | Just base <- stripSuffix "Pl_N" a = Just (Plural, [], base)
  | Just base <- stripSuffix "_N" a   = Just (Singular, [], base)
parseN _ = Nothing

parseVP ∷ SExp → Maybe (VerbPhrase, VerbForm)
parseVP (List [Atom "UseV", v]) = do
  (lemma, vf) <- parseV v
  pure (Intransitive lemma, vf)
parseVP (List [Atom "UseV2", v2, np]) = do
  (lemma, vf) <- parseV2 v2
  obj <- parseNP Objective np
  pure (Transitive lemma obj, vf)
parseVP _ = Nothing

parseDetInfo ∷ SExp → Maybe (String, Maybe Number)
parseDetInfo (Atom "the_Det")   = Just ("the", Just Singular)
parseDetInfo (Atom "a_Det")     = Just ("a", Just Singular)
parseDetInfo (Atom "thePl_Det") = Just ("the", Just Plural)
parseDetInfo (Atom "aPl_Det")   = Just ("some", Just Plural)
parseDetInfo _                  = Nothing

parseV ∷ SExp → Maybe (String, VerbForm)
parseV (Atom a)
  | Just base <- stripSuffix "S_V" a  = Just (base, ThirdSingular)
  | Just base <- stripSuffix "_V" a   = Just (base, BaseForm)
parseV _ = Nothing

parseV2 ∷ SExp → Maybe (String, VerbForm)
parseV2 (Atom a)
  | Just base <- stripSuffix "S_V2" a = Just (base, ThirdSingular)
  | Just base <- stripSuffix "_V2" a  = Just (base, BaseForm)
parseV2 _ = Nothing

parseA ∷ SExp → Maybe String
parseA (Atom a)
  | Just base <- stripSuffix "_A" a = Just base
parseA _ = Nothing

parsePN ∷ SExp → Maybe String
parsePN (List [Atom "MkPN", pn]) = parsePN pn
parsePN (List [Atom "SymbPN", StrLit s]) = Just s
parsePN (List [Atom "MkName", StrLit s]) = Just s
parsePN (StrLit s) = Just s
parsePN _ = Nothing

parsePron ∷ SExp → Maybe (Person, Number)
parsePron (Atom "i_Pron")    = Just (First, Singular)
parsePron (Atom "we_Pron")   = Just (First, Plural)
parsePron (Atom "you_Pron")  = Just (Second, Singular)
parsePron (Atom "he_Pron")   = Just (Third, Singular)
parsePron (Atom "she_Pron")  = Just (Third, Singular)
parsePron (Atom "it_Pron")   = Just (Third, Singular)
parsePron (Atom "they_Pron") = Just (Third, Plural)
parsePron _ = Nothing

stripSuffix ∷ String → String → Maybe String
stripSuffix suffix s =
  let n = length s - length suffix
  in if n > 0 && drop n s == suffix
       then Just (take n s)
       else Nothing

agreementOk ∷ Tense → Polarity → NounPhrase → VerbForm → Bool
agreementOk Present Positive subj vform =
  case (isThirdSingular subj, vform) of
    (True, ThirdSingular) -> True
    (False, BaseForm)     -> True
    _                     -> False
agreementOk Present Negative _ vform =
  vform == BaseForm
agreementOk _ _ _ _ = True

isThirdSingular ∷ NounPhrase → Bool
isThirdSingular (ProperNoun _) = True
isThirdSingular (CommonNoun _ _ _ Singular) = True
isThirdSingular (Pronoun Third Singular _) = True
isThirdSingular _ = False

translateSentence ∷ Sentence → String
translateSentence s =
  unwords
    [ renderTense (tense s)
    , renderPolarity (polarity s)
    , renderNP (subject s)
    , renderVP (verb s)
    ]

translateFallback ∷ String → String
translateFallback input =
  unwords (map fantasyToken (words input))

renderTense ∷ Tense → String
renderTense Present = "ta"
renderTense Past    = "na"
renderTense Future  = "va"

renderPolarity ∷ Polarity → String
renderPolarity Positive = "ke"
renderPolarity Negative = "no"

renderNP ∷ NounPhrase → String
renderNP (ProperNoun s) = fantasyToken s
renderNP (Pronoun p n c) = fantasyToken (renderPronoun p n c)
renderNP (CommonNoun det adjs noun _) =
  unwords (map fantasyToken (maybe [] pure det ++ reverse adjs ++ [noun]))

renderVP ∷ VerbPhrase → String
renderVP (Intransitive v) =
  fantasyToken v
renderVP (Transitive v obj) =
  unwords [fantasyToken v, renderNP obj]

renderPronoun ∷ Person → Number → PronounCase → String
renderPronoun First Singular Subjective = "i"
renderPronoun First Singular Objective  = "me"
renderPronoun First Plural   Subjective = "we"
renderPronoun First Plural   Objective  = "us"
renderPronoun Second _       _          = "you"
renderPronoun Third Singular Subjective = "he"
renderPronoun Third Singular Objective  = "him"
renderPronoun Third Plural   Subjective = "they"
renderPronoun Third Plural   Objective  = "them"

fantasyToken ∷ String → String
fantasyToken s =
  let base = map toLower (filter isAlpha s)
      seed = foldl (\acc c -> acc * 33 + fromEnum c) 7 base
      sylls = ["ka","ri","so","na","tu","mi","la","vo","xe","du"]
      pick i = sylls !! (i `mod` length sylls)
  in pick seed <> pick (seed `div` 7)

-- S-expression parsing for showExpr output

parseSExp ∷ String → Maybe SExp
parseSExp input = do
  (sexps, rest) <- parseMany (tokenize input)
  if null rest
    then case sexps of
           [sexp] -> Just sexp
           _      -> Just (List sexps)
    else Nothing

parseMany ∷ [Token] → Maybe ([SExp], [Token])
parseMany = go []
  where
    go acc [] = Just (reverse acc, [])
    go acc (TokRParen:_) = Nothing
    go acc ts = do
      (sexp, rest) <- parseOne ts
      go (sexp:acc) rest

parseOne ∷ [Token] → Maybe (SExp, [Token])
parseOne [] = Nothing
parseOne (TokLParen:ts) = go [] ts
  where
    go acc (TokRParen:rest) = Just (List (reverse acc), rest)
    go acc ts' = do
      (sexp, rest) <- parseOne ts'
      go (sexp:acc) rest
parseOne (TokStr s:ts) = Just (StrLit s, ts)
parseOne (TokAtom a:ts) = Just (Atom a, ts)
parseOne (TokRParen:_) = Nothing

data Token
  = TokLParen
  | TokRParen
  | TokAtom String
  | TokStr String
  deriving (Eq, Show)

tokenize ∷ String → [Token]
tokenize [] = []
tokenize (c:cs)
  | isSpace c = tokenize cs
  | c == '('  = TokLParen : tokenize cs
  | c == ')'  = TokRParen : tokenize cs
  | c == '"'  = let (s, rest) = span (/= '"') cs
                in TokStr s : tokenize (drop 1 rest)
  | otherwise =
      let (atom, rest) = span isAtomChar (c:cs)
      in TokAtom atom : tokenize rest

isAtomChar ∷ Char → Bool
isAtomChar c = not (isSpace c) && c /= '(' && c /= ')'
