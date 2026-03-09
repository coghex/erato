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

data ParsedQuestion
  = ParsedPolarQuestion NounPhrase VerbPhrase
  | ParsedWhQuestion WhClause
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
parseQuestion (List [Atom "MkQS", t, p, qcl]) = do
  tense' <- parseTense t
  polarity' <- parsePolarity p
  parsed <- parseQCl qcl
  pure $
    case parsed of
      ParsedPolarQuestion subj vp -> Question tense' polarity' subj vp
      ParsedWhQuestion whClause   -> WhQuestion tense' polarity' whClause
parseQuestion _ = Nothing

parseQCl ∷ SExp → Maybe ParsedQuestion
parseQCl (List [Atom "QuestCl", cl]) = do
  (subj, vp, vform) <- parseCl cl
  if questionAgreementOk vp vform
    then pure (ParsedPolarQuestion subj vp)
    else Nothing
parseQCl (List [Atom "QuestVP", ip, vp]) = do
  qword <- parseQuestionIP ip
  (vps, vform) <- parseVP vp
  if subjectQuestionAgreementOk vps vform
    then pure (ParsedWhQuestion (SubjectWh qword vps))
    else Nothing
parseQCl (List [Atom "QuestV2", ip, np, v2]) = do
  qword <- parseQuestionIP ip
  subj <- parseNP Subjective np
  (lemma, vform) <- parseV2 v2
  if vform == BaseForm
    then pure (ParsedWhQuestion (ObjectWh qword subj lemma))
    else Nothing
parseQCl (List [Atom "QuestIDetVP", idet, n, vp]) = do
  qword <- parseQuestionIDet idet
  queried <- parseQuestionDetNP idet n
  (vps, vform) <- parseVP vp
  if subjectQuestionAgreementOk vps vform
    then pure (ParsedWhQuestion (SubjectDetWh qword queried vps))
    else Nothing
parseQCl (List [Atom "QuestIDetV2", idet, n, np, v2]) = do
  qword <- parseQuestionIDet idet
  queried <- parseQuestionDetNP idet n
  subj <- parseNP Subjective np
  (lemma, vform) <- parseV2 v2
  if vform == BaseForm
    then pure (ParsedWhQuestion (ObjectDetWh qword queried subj lemma))
    else Nothing
parseQCl (List [Atom "QuestIAdv", iadv, cl]) = do
  qword <- parseQuestionIAdv iadv
  (subj, vp, vform) <- parseCl cl
  if questionAgreementOk vp vform
    then pure (ParsedWhQuestion (AdvWh qword subj vp))
    else Nothing
parseQCl cl = do
  (subj, vp, vform) <- parseCl cl
  if questionAgreementOk vp vform
    then pure (ParsedPolarQuestion subj vp)
    else Nothing

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
parseTense (Atom "TPerf") = Just Perfect
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
parseAdv (List [Atom "AdvSubjS", subj, s]) = do
  subjTxt <- parseSubj subj
  clause <- parseSentence s
  pure (ClausePhrase subjTxt clause)
parseAdv _ = Nothing

parsePrep ∷ SExp → Maybe String
parsePrep (Atom a)
  | Just base <- stripSuffix "_Prep" a = Just base
parsePrep _ = Nothing

parseSubj ∷ SExp → Maybe String
parseSubj (Atom "because_Subj") = Just "because"
parseSubj (Atom "if_Subj")      = Just "if"
parseSubj (Atom "when_Subj")    = Just "when"
parseSubj _                     = Nothing

parseRP ∷ SExp → Maybe ()
parseRP (Atom "who_RP")  = Just ()
parseRP (Atom "that_RP") = Just ()
parseRP _ = Nothing

parseQuestionIP ∷ SExp → Maybe QuestionWord
parseQuestionIP (Atom "who_IP")  = Just Who
parseQuestionIP (Atom "what_IP") = Just What
parseQuestionIP _ = Nothing

parseQuestionIAdv ∷ SExp → Maybe QuestionWord
parseQuestionIAdv (Atom "where_IAdv") = Just Where
parseQuestionIAdv (Atom "when_IAdv")  = Just When
parseQuestionIAdv (Atom "why_IAdv")   = Just Why
parseQuestionIAdv (Atom "how_IAdv")   = Just How
parseQuestionIAdv _ = Nothing

parseQuestionIDet ∷ SExp → Maybe QuestionWord
parseQuestionIDet (Atom "whichSg_IDet") = Just Which
parseQuestionIDet (Atom "whichPl_IDet") = Just Which
parseQuestionIDet (Atom "howMany_IDet") = Just HowMany
parseQuestionIDet _ = Nothing

parseQuestionDetInfo ∷ SExp → Maybe (String, Maybe Number)
parseQuestionDetInfo (Atom "whichSg_IDet") = Just ("which", Just Singular)
parseQuestionDetInfo (Atom "whichPl_IDet") = Just ("which", Just Plural)
parseQuestionDetInfo (Atom "howMany_IDet") = Just ("how many", Just Plural)
parseQuestionDetInfo _ = Nothing

parseQuestionDetNP ∷ SExp → SExp → Maybe NounPhrase
parseQuestionDetNP idet n = do
  (detText, detNum) <- parseQuestionDetInfo idet
  (nounNum, adjs, noun, rel) <- parseN n
  let finalNum = maybe nounNum id detNum
  case detNum of
    Just dnum | dnum /= nounNum -> Nothing
    _ -> pure (CommonNoun (Just detText) adjs noun finalNum rel)

parseDetInfo ∷ SExp → Maybe (String, Maybe Number)
parseDetInfo (Atom "the_Det")   = Just ("the", Just Singular)
parseDetInfo (Atom "a_Det")     = Just ("a", Just Singular)
parseDetInfo (Atom "thePl_Det") = Just ("the", Just Plural)
parseDetInfo (Atom "aPl_Det")   = Just ("some", Just Plural)
parseDetInfo (Atom "every_Det") = Just ("every", Just Singular)
parseDetInfo (Atom "all_Det")   = Just ("all", Just Plural)
parseDetInfo (Atom "many_Det")  = Just ("many", Just Plural)
parseDetInfo (Atom "no_Det")    = Just ("no", Nothing)
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

parseVV ∷ SExp → Maybe String
parseVV (Atom a)
  | Just base <- stripSuffix "_VV" a = Just base
parseVV _ = Nothing

parseV2V ∷ SExp → Maybe String
parseV2V (Atom a)
  | Just base <- stripSuffix "_V2V" a = Just base
parseV2V _ = Nothing

parseVS ∷ SExp → Maybe String
parseVS (Atom a)
  | Just base <- stripSuffix "_VS" a = Just base
parseVS _ = Nothing

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
skipAgreement (Perfective _) = True
skipAgreement (VVComplement _ _) = True
skipAgreement (V2VComplement _ _ _) = True
skipAgreement (VSComplement _ _) = True
skipAgreement (VPWithAdv vp _) = skipAgreement vp
skipAgreement (CoordVP _ a b) = skipAgreement a && skipAgreement b
skipAgreement _ = False

questionAgreementOk ∷ VerbPhrase → VerbForm → Bool
questionAgreementOk vp vform =
  skipAgreement vp || vform == BaseForm

subjectQuestionAgreementOk ∷ VerbPhrase → VerbForm → Bool
subjectQuestionAgreementOk vp vform =
  skipAgreement vp || vform == ThirdSingular || vform == BaseForm

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
-- Present-perfect uses an agreeing auxiliary with a non-finite lexical verb.
agreementOk Perfect Positive _ vform = vform == BaseForm
agreementOk Perfect Negative _ vform = vform == BaseForm

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
translateSentence (WhQuestion t p whClause) =
  unwords
    [ fantasyToken "question"
    , renderTense t
    , renderPolarity p
    , renderWhClause whClause
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
parseNP _ (List [Atom "PossSgNP", possessor, n]) = do
  owner <- parseNP Subjective possessor
  (nounNum, adjs, noun, rel) <- parseN n
  if possessorAllowed owner && nounNum == Singular && quantifierAdjAgreementOk nounNum adjs
    then pure (PossessedNoun owner adjs noun Singular rel)
    else Nothing
parseNP _ (List [Atom "PossPlNP", possessor, n]) = do
  owner <- parseNP Subjective possessor
  (nounNum, adjs, noun, rel) <- parseN n
  if possessorAllowed owner && nounNum == Plural && quantifierAdjAgreementOk nounNum adjs
    then pure (PossessedNoun owner adjs noun Plural rel)
    else Nothing
parseNP _ (List [Atom "DetCN", det, n]) = do
  (detText, detNum) <- parseDetInfo det
  (nounNum, adjs, noun, rel) <- parseN n
  let finalNum = maybe nounNum id detNum
  case detNum of
    Just dnum | dnum /= nounNum -> Nothing
    _ | quantifierAdjAgreementOk finalNum adjs ->
          pure (CommonNoun (Just detText) adjs noun finalNum rel)
      | otherwise -> Nothing
parseNP _ (List [Atom "UseN", n]) = do
  (num, adjs, noun, rel) <- parseN n
  if quantifierAdjAgreementOk num adjs
    then pure (CommonNoun Nothing adjs noun num rel)
    else Nothing
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
parseVP (List [Atom "ComplVV", vv, vp]) = do
  verb <- parseVV vv
  (compVP, _) <- parseVP vp
  pure (VVComplement verb compVP, BaseForm)
parseVP (List [Atom "ComplV2V", v2v, np, vp]) = do
  verb <- parseV2V v2v
  obj <- parseNP Objective np
  (compVP, _) <- parseVP vp
  pure (V2VComplement verb obj compVP, BaseForm)
parseVP (List [Atom "ComplVS", vs, s]) = do
  verb <- parseVS vs
  clause <- parseSentence s
  pure (VSComplement verb clause, BaseForm)
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
parseVP (List [Atom "PerfectVP", vp]) = do
  (baseVP, _) <- parseVP vp
  pure (Perfective baseVP, BaseForm)
parseVP _ = Nothing

parseConj ∷ SExp → Maybe Conj
parseConj (Atom "and_Conj") = Just And
parseConj (Atom "or_Conj")  = Just Or
parseConj _ = Nothing

possessorAllowed ∷ NounPhrase → Bool
possessorAllowed (CommonNoun Nothing _ _ _ _) = False
possessorAllowed _ = True

quantifierAdjAgreementOk ∷ Number → [String] → Bool
quantifierAdjAgreementOk Singular adjs =
  not (any (`elem` ["all", "many"]) adjs)
quantifierAdjAgreementOk Plural _ = True

renderNP ∷ NounPhrase → String
renderNP (CoordNP c a b) =
  unwords [renderNP a, renderConj c, renderNP b]
renderNP (ProperNoun s) = fantasyToken s
renderNP (Pronoun p n c) = fantasyToken (renderPronoun p n c)
renderNP (PossessedNoun owner adjs noun _ rel) =
  let base = unwords ([renderNP owner, fantasyToken "poss"] ++ map fantasyToken (adjs ++ [noun]))
  in case rel of
       Nothing -> base
       Just rc -> unwords [base, renderRelClause rc]
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
renderVP (VVComplement v vp) =
  if isModalAux v
    then unwords [fantasyToken v, renderVP vp]
    else unwords [fantasyToken v, renderInfinitiveVP vp]
renderVP (V2VComplement v obj vp) =
  unwords [fantasyToken v, renderNP obj, renderInfinitiveVP vp]
renderVP (VSComplement v sentence) =
  unwords [fantasyToken v, fantasyToken "that", renderEmbeddedSentence sentence]
renderVP (Copula adj) = unwords [fantasyToken "be", fantasyToken adj]
renderVP (Passive v) = unwords [fantasyToken "be", fantasyToken v]
renderVP (Progressive vp) = unwords [fantasyToken "be", renderVP vp]
renderVP (Perfective vp) = unwords [fantasyToken "have", renderVP vp]
renderVP (VPWithAdv vp adv) = unwords [renderVP vp, renderAdv adv]

renderInfinitiveVP ∷ VerbPhrase → String
renderInfinitiveVP vp =
  unwords [fantasyToken "to", renderVP vp]

renderEmbeddedSentence ∷ Sentence → String
renderEmbeddedSentence = translateSentence

renderWhClause ∷ WhClause → String
renderWhClause (SubjectWh qword vp) =
  unwords [renderQuestionWord qword, renderVP vp]
renderWhClause (ObjectWh qword subj verb) =
  unwords [renderQuestionWord qword, renderNP subj, fantasyToken verb]
renderWhClause (SubjectDetWh _ queried vp) =
  unwords [renderNP queried, renderVP vp]
renderWhClause (ObjectDetWh _ queried subj verb) =
  unwords [renderNP queried, renderNP subj, fantasyToken verb]
renderWhClause (AdvWh qword subj vp) =
  unwords [renderQuestionWord qword, renderNP subj, renderVP vp]

renderQuestionWord ∷ QuestionWord → String
renderQuestionWord Who = fantasyToken "who"
renderQuestionWord What = fantasyToken "what"
renderQuestionWord Where = fantasyToken "where"
renderQuestionWord When = fantasyToken "when"
renderQuestionWord Why = fantasyToken "why"
renderQuestionWord How = fantasyToken "how"
renderQuestionWord Which = fantasyToken "which"
renderQuestionWord HowMany = unwords [fantasyToken "how", fantasyToken "many"]

renderConj ∷ Conj → String
renderConj And = fantasyToken "and"
renderConj Or  = fantasyToken "or"

isThirdSingular ∷ NounPhrase → Bool
isThirdSingular (CoordNP _ _ _) = False
isThirdSingular (ProperNoun _) = True
isThirdSingular (PossessedNoun _ _ _ Singular _) = True
isThirdSingular (CommonNoun _ _ _ Singular _) = True
isThirdSingular (Pronoun Third Singular _) = True
isThirdSingular _ = False

renderTense ∷ Tense → String
renderTense Present = "ta"
renderTense Past    = "na"
renderTense Future  = "va"
renderTense Perfect = "sa"

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
renderAdv (ClausePhrase subj sentence) =
  unwords [fantasyToken subj, renderEmbeddedSentence sentence]

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

isModalAux ∷ String → Bool
isModalAux verb =
  verb `elem` ["can", "could", "may", "might", "must", "shall", "should", "will", "would"]

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
