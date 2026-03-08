{-# LANGUAGE Strict, UnicodeSyntax #-}

module Parser.Translate
  ( exprToSentence
  , exprProperNouns
  , validateExpr
  , translateSentence
  , translateFallback
  ) where

import PGF hiding (Token)
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
parseSentence (List [Atom "UttQS", qs]) =
  parseQuestion qs
parseSentence (List [Atom "UttImp", p, imp]) = do
  polarity' <- parsePolarity p
  (vp, _) <- parseImp imp
  pure (Imperative polarity' vp)
parseSentence (List [Atom "MkS", t, p, cl]) = do
  tense'    <- parseTense t
  polarity' <- parsePolarity p
  parseClAsSentence tense' polarity' cl
parseSentence (List [Atom "MkSCoord", t, p, np, conj, vp1, vp2]) = do
  tense'    <- parseTense t
  polarity' <- parsePolarity p
  subj <- parseNP Subjective np
  c <- parseConj conj
  (v1, vf1) <- parseVP vp1
  (v2, vf2) <- parseVP vp2
  if agreementOk tense' polarity' subj vf1
     && agreementOk tense' polarity' subj vf2
    then pure (Sentence tense' polarity' subj (CoordVP c v1 v2))
    else Nothing
parseSentence _ = Nothing

parseQuestion ∷ SExp → Maybe Sentence
parseQuestion (List [Atom "MkQS", t, p, cl]) = do
  tense' <- parseTense t
  polarity' <- parsePolarity p
  (subj, vp, vform) <- parseCl cl
  if questionAgreementOk vp vform
    then pure (Question tense' polarity' subj vp)
    else Nothing
parseQuestion _ = Nothing

parseClAsSentence ∷ Tense → Polarity → SExp → Maybe Sentence
parseClAsSentence tense' polarity' (List [Atom "ExistPred", np]) = do
  obj <- parseNP Objective np
  pure (Existential tense' polarity' obj)
parseClAsSentence tense' polarity' cl = do
  (subj, vp, vform) <- parseCl cl
  if skipAgreement vp || agreementOk tense' polarity' subj vform
    then pure (Sentence tense' polarity' subj vp)
    else Nothing

parseImp ∷ SExp → Maybe (VerbPhrase, VerbForm)
parseImp (List [Atom "MkImp", vp]) = parseVP vp
parseImp _ = Nothing

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

parseN ∷ SExp → Maybe (Number, [String], String, Maybe RelClause)
parseN (List [Atom "AdjCN", a, n]) = do
  adj <- parseA a
  (num, adjs, noun, rel) <- parseN n
  pure (num, adj : adjs, noun, rel)
parseN (List [Atom "RelCN", n, rc]) = do
  (num, adjs, noun, rel) <- parseN n
  rc' <- parseRelClause rc
  case rel of
    Nothing -> pure (num, adjs, noun, Just rc')
    Just _  -> Nothing
parseN (Atom a)
  | Just base <- stripSuffix "Pl_N" a = Just (Plural, [], base, Nothing)
  | Just base <- stripSuffix "_N" a   = Just (Singular, [], base, Nothing)
parseN _ = Nothing

parseRelClause ∷ SExp → Maybe RelClause
parseRelClause (List [Atom "RelVP", rp, vp]) = do
  _ <- parseRP rp
  (vps, _) <- parseVP vp
  pure (RelVP vps)
parseRelClause (List [Atom "NegRelVP", rp, vp]) = do
  _ <- parseRP rp
  (vps, _) <- parseVP vp
  pure (NegRelVP vps)
parseRelClause (List [Atom "RelV2", rp, np, v2]) = do
  _ <- parseRP rp
  subj <- parseNP Subjective np
  (lemma, _) <- parseV2 v2
  pure (RelV2 lemma subj)
parseRelClause (List [Atom "NegRelV2", rp, np, v2]) = do
  _ <- parseRP rp
  subj <- parseNP Subjective np
  (lemma, _) <- parseV2 v2
  pure (NegRelV2 lemma subj)
parseRelClause _ = Nothing

parseAdv ∷ SExp → Maybe AdvPhrase
parseAdv (List [Atom "PrepNP", prep, np]) = do
  prepTxt <- parsePrep prep
  obj <- parseNP Objective np
  pure (PrepPhrase prepTxt obj)
parseAdv _ = Nothing

parsePrep ∷ SExp → Maybe String
parsePrep (Atom a)
  | Just base <- stripSuffix "_Prep" a = Just base
parsePrep _ = Nothing

parseRP ∷ SExp → Maybe ()
parseRP (Atom "who_RP")  = Just ()
parseRP (Atom "that_RP") = Just ()
parseRP _ = Nothing

parseDetInfo ∷ SExp → Maybe (String, Maybe Number)
parseDetInfo (Atom "the_Det")   = Just ("the", Just Singular)
parseDetInfo (Atom "a_Det")     = Just ("a", Just Singular)
parseDetInfo (Atom "thePl_Det") = Just ("the", Just Plural)
parseDetInfo (Atom "aPl_Det")   = Just ("some", Just Plural)
parseDetInfo (Atom "every_Det") = Just ("every", Just Singular)
parseDetInfo (Atom "this_Det")  = Just ("this", Just Singular)
parseDetInfo (Atom "that_Det")  = Just ("that", Just Singular)
parseDetInfo (Atom "these_Det") = Just ("these", Just Plural)
parseDetInfo (Atom "those_Det") = Just ("those", Just Plural)
parseDetInfo (Atom "my_Det")    = Just ("my", Just Singular)
parseDetInfo (Atom "myPl_Det")  = Just ("my", Just Plural)
parseDetInfo (Atom "your_Det")  = Just ("your", Just Singular)
parseDetInfo (Atom "yourPl_Det") = Just ("your", Just Plural)
parseDetInfo (Atom "his_Det")   = Just ("his", Just Singular)
parseDetInfo (Atom "hisPl_Det") = Just ("his", Just Plural)
parseDetInfo (Atom "her_Det")   = Just ("her", Just Singular)
parseDetInfo (Atom "herPl_Det") = Just ("her", Just Plural)
parseDetInfo (Atom "its_Det")   = Just ("its", Just Singular)
parseDetInfo (Atom "itsPl_Det") = Just ("its", Just Plural)
parseDetInfo (Atom "our_Det")   = Just ("our", Just Singular)
parseDetInfo (Atom "ourPl_Det") = Just ("our", Just Plural)
parseDetInfo (Atom "their_Det") = Just ("their", Just Singular)
parseDetInfo (Atom "theirPl_Det") = Just ("their", Just Plural)
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
parsePron (Atom "youPl_Pron") = Just (Second, Plural)
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

skipAgreement ∷ VerbPhrase → Bool
skipAgreement (Copula _) = True
skipAgreement (Passive _) = True
skipAgreement (Progressive _) = True
skipAgreement _ = False

questionAgreementOk ∷ VerbPhrase → VerbForm → Bool
questionAgreementOk vp vform =
  skipAgreement vp || vform == BaseForm

agreementOk ∷ Tense → Polarity → NounPhrase → VerbForm → Bool
agreementOk Present Positive subj vform =
  case (isThirdSingular subj, vform) of
    (True, ThirdSingular) -> True
    (False, BaseForm)     -> True
    _                     -> False
-- Once English tense is expressed with do-support or a tense auxiliary, the
-- lexical verb is expected to stay in the base form.
agreementOk Present Negative _ vform =
  vform == BaseForm
-- Past and future forms are already encoded in the surrounding GF structure,
-- so the lexical verb shape does not vary with the subject in this AST.
agreementOk Past Positive _ _ = True
agreementOk Past Negative _ _ = True
agreementOk Future Positive _ _ = True
agreementOk Future Negative _ _ = True

translateSentence ∷ Sentence → String
translateSentence (Sentence t p subj vp) =
  unwords
    [ renderTense t
    , renderPolarity p
    , renderNP subj
    , renderVP vp
    ]
translateSentence (Question t p subj vp) =
  unwords
    [ fantasyToken "question"
    , renderTense t
    , renderPolarity p
    , renderNP subj
    , renderVP vp
    ]
translateSentence (Existential t p np) =
  unwords
    [ renderTense t
    , renderPolarity p
    , fantasyToken "there"
    , renderNP np
    ]
translateSentence (Imperative p vp) =
  unwords
    [ renderPolarity p
    , renderVP vp
    ]

translateFallback ∷ String → String
translateFallback input =
  unwords (map fantasyToken (words input))

parseNP ∷ PronounCase → SExp → Maybe NounPhrase
parseNP cas (List [Atom "ConjNP", conj, np1, np2]) = do
  c <- parseConj conj
  n1 <- parseNP cas np1
  n2 <- parseNP cas np2
  pure (CoordNP c n1 n2)
parseNP _ (List [Atom "DetCN", det, n]) = do
  (detText, detNum) <- parseDetInfo det
  (nounNum, adjs, noun, rel) <- parseN n
  let finalNum = maybe nounNum id detNum
  case detNum of
    Just dnum | dnum /= nounNum -> Nothing
    _ -> pure (CommonNoun (Just detText) adjs noun finalNum rel)
parseNP _ (List [Atom "UseN", n]) = do
  (num, adjs, noun, rel) <- parseN n
  pure (CommonNoun Nothing adjs noun num rel)
parseNP _ (List [Atom "UsePN", pn]) = ProperNoun <$> parsePN pn
parseNP c (List [Atom "UsePron", pr]) = do
  (p, n) <- parsePron pr
  pure (Pronoun p n c)
parseNP _ _ = Nothing

parseVP ∷ SExp → Maybe (VerbPhrase, VerbForm)
parseVP (List [Atom "AdvVP", vp, adv]) = do
  (baseVP, vf) <- parseVP vp
  advp <- parseAdv adv
  pure (VPWithAdv baseVP advp, vf)
parseVP (List [Atom "UseV", v]) = do
  (lemma, vf) <- parseV v
  pure (Intransitive lemma, vf)
parseVP (List [Atom "UseV2", v2, np]) = do
  (lemma, vf) <- parseV2 v2
  obj <- parseNP Objective np
  pure (Transitive lemma obj, vf)
parseVP (List [Atom "UseAP", a]) = do
  adj <- parseA a
  pure (Copula adj, ThirdSingular)
parseVP (List [Atom "PassV2VP", v2]) = do
  (lemma, _) <- parseV2 v2
  pure (Passive lemma, BaseForm)
parseVP (List [Atom "ProgressVP", vp]) = do
  (baseVP, _) <- parseVP vp
  pure (Progressive baseVP, BaseForm)
parseVP _ = Nothing

parseConj ∷ SExp → Maybe Conj
parseConj (Atom "and_Conj") = Just And
parseConj (Atom "or_Conj")  = Just Or
parseConj _ = Nothing

renderNP ∷ NounPhrase → String
renderNP (CoordNP c a b) =
  unwords [renderNP a, renderConj c, renderNP b]
renderNP (ProperNoun s) = fantasyToken s
renderNP (Pronoun p n c) = fantasyToken (renderPronoun p n c)
renderNP (CommonNoun det adjs noun _ rel) =
  let base = unwords (map fantasyToken (maybe [] pure det ++ adjs ++ [noun]))
  in case rel of
       Nothing -> base
       Just rc -> unwords [base, renderRelClause rc]

renderVP ∷ VerbPhrase → String
renderVP (CoordVP c a b) =
  unwords [renderVP a, renderConj c, renderVP b]
renderVP (Intransitive v) = fantasyToken v
renderVP (Transitive v obj) = unwords [fantasyToken v, renderNP obj]
renderVP (Copula adj) = unwords [fantasyToken "be", fantasyToken adj]
renderVP (Passive v) = unwords [fantasyToken "be", fantasyToken v]
renderVP (Progressive vp) = unwords [fantasyToken "be", renderVP vp]
renderVP (VPWithAdv vp adv) = unwords [renderVP vp, renderAdv adv]

renderConj ∷ Conj → String
renderConj And = fantasyToken "and"
renderConj Or  = fantasyToken "or"

isThirdSingular ∷ NounPhrase → Bool
isThirdSingular (CoordNP _ _ _) = False
isThirdSingular (ProperNoun _) = True
isThirdSingular (CommonNoun _ _ _ Singular _) = True
isThirdSingular (Pronoun Third Singular _) = True
isThirdSingular _ = False

renderTense ∷ Tense → String
renderTense Present = "ta"
renderTense Past    = "na"
renderTense Future  = "va"

renderPolarity ∷ Polarity → String
renderPolarity Positive = "ke"
renderPolarity Negative = "no"

renderRelClause ∷ RelClause → String
renderRelClause (RelVP vp) =
  unwords [fantasyToken "who", renderVP vp]
renderRelClause (NegRelVP vp) =
  unwords [fantasyToken "who", fantasyToken "not", renderVP vp]
renderRelClause (RelV2 v obj) =
  unwords [fantasyToken "that", fantasyToken v, renderNP obj]
renderRelClause (NegRelV2 v obj) =
  unwords [fantasyToken "that", fantasyToken "not", fantasyToken v, renderNP obj]

renderAdv ∷ AdvPhrase → String
renderAdv (PrepPhrase prep np) =
  unwords [fantasyToken prep, renderNP np]

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
    go _   (TokRParen:_) = Nothing
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

-- | Parse the Expr once, return the Sentence and all SymbPN strings together
validateExpr ∷ Expr → Maybe (Sentence, [String])
validateExpr expr = do
  sexp <- parseSExp (showExpr [] expr)
  sentence <- parseSentence sexp
  pure (sentence, collectSymbPN sexp)
