{-# LANGUAGE Strict, UnicodeSyntax #-}

module Parser.Translate
  ( exprToSentence
  , exprProperNouns
  , validateExpr
  , translateSentence
  , translateFallback
  ) where

import Control.Applicative ((<|>))
import Data.Char (isAlpha, isSpace, toLower)
import Data.List (isPrefixOf, isSuffixOf)
import PGF hiding (Token)
import Parser.AST

-- Prefer decoding PGF Expr directly; fall back to showExpr parsing for any
-- shapes we do not recognize yet.
exprToSentence ∷ Expr → Maybe Sentence
exprToSentence expr = do
  sexp <- decodeExpr expr
  parseSentence sexp

exprProperNouns ∷ Expr → [String]
exprProperNouns expr =
  case decodeExpr expr of
    Nothing   -> []
    Just sexp -> collectSymbPN sexp

collectSymbPN ∷ SExp → [String]
collectSymbPN (List [Atom "SymbPN", StrLit s]) = [s]
collectSymbPN (List xs) = concatMap collectSymbPN xs
collectSymbPN _ = []

decodeExpr ∷ Expr → Maybe SExp
decodeExpr expr =
  exprToSExp expr <|> parseSExp (showExpr [] expr)

exprToSExp ∷ Expr → Maybe SExp
exprToSExp expr =
  case () of
    _
      | Just value <- unStr expr ->
          Just (StrLit value)
      | Just value <- unInt expr ->
          Just (Atom (show value))
      | Just value <- unFloat expr ->
          Just (Atom (show value))
      | Just (cid, args) <- unApp expr -> do
          sexps <- traverse exprToSExp args
          pure $
            if null args
              then Atom (showCId cid)
              else List (Atom (showCId cid) : sexps)
      | otherwise ->
          Nothing

-- S-expression model for parsing showExpr output
data SExp
  = Atom String
  | StrLit String
  | List [SExp]
  deriving (Eq, Show)

data VerbForm = BaseForm | ThirdSingular
  deriving (Eq, Show)

gfHyphenMarker ∷ String
gfHyphenMarker = "_H_"

decodeLexemeStem ∷ String → String
decodeLexemeStem [] = []
decodeLexemeStem stem
  | gfHyphenMarker `isPrefixOf` stem =
      '-' : decodeLexemeStem (drop (length gfHyphenMarker) stem)
  | otherwise =
      let c : rest = stem
      in c : decodeLexemeStem rest

data ParsedQuestion
  = ParsedPolarQuestion NounPhrase VerbPhrase
  | ParsedWhQuestion WhClause
  deriving (Eq, Show)

parseSentence ∷ SExp → Maybe Sentence
parseSentence (List [Atom "UttS", s]) =
  parseSentence s
parseSentence (List [Atom "UttQS", qs]) =
  parseQuestion qs
parseSentence (List [Atom "UttImp", p, List [Atom "FareTheeWellVocImp", np]]) = do
  polarity' <- parsePolarity p
  addressee <- parseNP Subjective np
  let verbPhrase =
        VPWithAdv
          (Transitive "fare" (Pronoun Second Singular Objective))
          (LexicalAdv "well")
  pure (Vocative (Imperative polarity' verbPhrase) addressee)
parseSentence (List [Atom "UttImp", p, List [Atom "AdvFareTheeWellVocImp", adv, np]]) = do
  polarity' <- parsePolarity p
  advPhrase <- parseAdv adv
  addressee <- parseNP Subjective np
  let verbPhrase =
        VPWithAdv
          (VPWithAdv
            (Transitive "fare" (Pronoun Second Singular Objective))
            (LexicalAdv "well"))
          advPhrase
  pure (Vocative (Imperative polarity' verbPhrase) addressee)
parseSentence (List [Atom "UttImp", p, List [Atom "VocNPImp", vp, np]]) = do
  polarity' <- parsePolarity p
  (verbPhrase, _) <- parseVP vp
  addressee <- parseNP Subjective np
  pure (Vocative (Imperative polarity' verbPhrase) addressee)
parseSentence (List [Atom "UttImp", p, imp]) = do
  polarity' <- parsePolarity p
  (vp, _) <- parseImp imp
  pure (Imperative polarity' vp)
parseSentence (List [Atom "VocativeUtt", utt, np]) = do
  sentence <- parseSentence utt
  addressee <- parseNP Subjective np
  pure (Vocative sentence addressee)
parseSentence (List [Atom "OhLeadUtt", utt]) = do
  sentence <- parseSentence utt
  pure (SentenceWithAdv sentence (LexicalAdv "oh"))
parseSentence (List [Atom "MkS", t, p, cl]) = do
  tense'    <- parseTense t
  polarity' <- parsePolarity p
  parseClAsSentence tense' polarity' cl
parseSentence (List [Atom "FrontAdvS", adv, s]) = do
  adv' <- parseAdv adv
  sentence <- parseSentence s
  pure (SentenceWithAdv sentence adv')
parseSentence (List [Atom "WouldThatS", s]) = do
  sentence <- parseSentence s
  pure (SentenceWithAdv sentence (LexicalAdv "would that"))
parseSentence (List [Atom "ByHowMuchSoMuchS", s1, s2]) = do
  _ <- parseSentence s1
  secondSentence <- parseSentence s2
  let byHowMuch =
        PrepPhrase "by" (CommonNoun Nothing ["much"] "how" Singular Nothing)
      bySoMuch =
        PrepPhrase "by" (CommonNoun Nothing ["much"] "so" Singular Nothing)
  pure (SentenceWithAdv (SentenceWithAdv secondSentence bySoMuch) byHowMuch)
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
parseQuestion (List [Atom "MkQSIAdvCoord", t, p, iadv, np, conj, vp1, vp2]) = do
  tense' <- parseTense t
  polarity' <- parsePolarity p
  (qword, frontedAdvs) <- parseQuestionIAdv iadv
  subj <- parseNP Subjective np
  c <- parseConj conj
  (vpa, vf1) <- parseVP vp1
  (vpb, vf2) <- parseVP vp2
  let baseVp = CoordVP c vpa vpb
      vp = foldl VPWithAdv baseVp frontedAdvs
  if questionAgreementOk vpa vf1 && questionAgreementOk vpb vf2
    then pure (WhQuestion tense' polarity' (AdvWh qword subj vp))
    else Nothing
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
  (qword, frontedAdvs) <- parseQuestionIAdv iadv
  (subj, vp, vform) <- parseCl cl
  let vp' = foldl VPWithAdv vp frontedAdvs
  if questionAgreementOk vp' vform
    then pure (ParsedWhQuestion (AdvWh qword subj vp'))
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
parseImp (List [Atom "CoordImp", conj, vp1, vp2]) = do
  c <- parseConj conj
  (left, _) <- parseVP vp1
  (right, _) <- parseVP vp2
  pure (CoordVP c left right, BaseForm)
parseImp _ = Nothing

parseTense ∷ SExp → Maybe Tense
parseTense (Atom "TPres") = Just Present
parseTense (Atom "TPast") = Just Past
parseTense (Atom "TFut")  = Just Future
parseTense (Atom "TCond") = Just Conditional
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
parseN (List [Atom "AdjPCN", ap, n]) = do
  (num, adjs, noun, rel) <- parseN n
  case parsePostpositiveAP ap of
    Just adj ->
      pure (num, adjs, noun, addRelClause rel (PostAdj adj))
    Nothing -> do
      adjTokens <- flattenPrenominalAP =<< parseAP ap
      pure (num, adjTokens ++ adjs, noun, rel)
parseN (List [Atom "PrepCN", n, prep, np]) = do
  (num, adjs, noun, rel) <- parseN n
  prepTxt <- parsePrep prep
  obj <- parseNP Objective np
  pure (num, adjs, noun, addRelClause rel (RelPrep prepTxt obj))
parseN (List [Atom "PrepQSCN", n, prep, qs]) = do
  (num, adjs, noun, rel) <- parseN n
  prepTxt <- parsePrep prep
  clause <- parseQuestion qs
  pure (num, adjs, noun, addRelClause rel (RelPrepSentence prepTxt clause))
parseN (List [Atom "RelCN", n, rc]) = do
  (num, adjs, noun, rel) <- parseN n
  rc' <- parseRelClause rc
  pure (num, adjs, noun, addRelClause rel rc')
parseN (Atom a)
  | Just base <- stripSuffix "Pl_N" a = Just (Plural, [], decodeLexemeStem base, Nothing)
  | Just base <- stripSuffix "_N" a   = Just (Singular, [], decodeLexemeStem base, Nothing)
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
parseRelClause (List [Atom "RelFutureAdVV2", rp, np, _, v2]) = do
  _ <- parseRP rp
  subj <- parseNP Subjective np
  (lemma, _) <- parseV2 v2
  pure (RelV2 lemma subj)
parseRelClause (List [Atom "NegRelV2", rp, np, v2]) = do
  _ <- parseRP rp
  subj <- parseNP Subjective np
  (lemma, _) <- parseV2 v2
  pure (NegRelV2 lemma subj)
parseRelClause (List [Atom "RelWhoseBe", n, np]) = do
  (_, _, noun, _) <- parseN n
  subj <- parseNP Subjective np
  pure (RelWhoseBe noun subj)
parseRelClause (List [Atom "RelPrepWhomS", prep, s]) = do
  prepTxt <- parsePrep prep
  sentence <- parseSentence s
  whSentence <- sentenceToWhomPrepQuestion sentence
  pure (RelPrepSentence prepTxt whSentence)
parseRelClause (List [Atom "AndRC", rc1, rc2]) = do
  left <- parseRelClause rc1
  right <- parseRelClause rc2
  pure (RelChain left right)
parseRelClause (List [Atom "ButRC", rc1, rc2]) = do
  left <- parseRelClause rc1
  right <- parseRelClause rc2
  pure (RelChain left right)
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
parseAdv (List [Atom "AdvA", a]) =
  LexicalAdv <$> parseA a
parseAdv (List [Atom "ModAdv", ada, adv]) = do
  modifier <- parseAdA ada
  base <- parseAdv adv
  pure (ModifiedAdv modifier base)
parseAdv (List [Atom "AndAdv", adv1, adv2]) = do
  left <- parseAdv adv1
  right <- parseAdv adv2
  pure (CoordAdv And left right)
parseAdv (Atom "atLeast_Adv") =
  Just (LexicalAdv "at least")
parseAdv (Atom a)
  | Just base <- stripSuffix "_Adv" a = Just (LexicalAdv base)
parseAdv _ = Nothing

parseAdV ∷ SExp → Maybe AdvPhrase
parseAdV (Atom a)
  | Just base <- stripSuffix "_AdV" a = Just (LexicalAdv base)
parseAdV _ = Nothing

parsePrep ∷ SExp → Maybe String
parsePrep (Atom "asAffording_Prep") = Just "as affording"
parsePrep (Atom "asWellAs_Prep") = Just "as well as"
parsePrep (Atom "asTouching_Prep") = Just "as touching"
parsePrep (Atom a)
  | Just base <- stripSuffix "_Prep" a = Just base
parsePrep _ = Nothing

parseSubj ∷ SExp → Maybe String
parseSubj (Atom "because_Subj") = Just "because"
parseSubj (Atom "if_Subj")      = Just "if"
parseSubj (Atom "when_Subj")    = Just "when"
parseSubj (Atom "while_Subj")   = Just "while"
parseSubj (Atom "where_Subj")   = Just "where"
parseSubj _                     = Nothing

parseRP ∷ SExp → Maybe ()
parseRP (Atom "who_RP")  = Just ()
parseRP (Atom "which_RP") = Just ()
parseRP (Atom "that_RP") = Just ()
parseRP _ = Nothing

parseQuestionIP ∷ SExp → Maybe QuestionWord
parseQuestionIP (Atom "who_IP")  = Just Who
parseQuestionIP (Atom "what_IP") = Just What
parseQuestionIP _ = Nothing

sentenceToWhomPrepQuestion ∷ Sentence → Maybe Sentence
sentenceToWhomPrepQuestion (Sentence tense polarity subj vp) =
  Just (WhQuestion tense polarity (AdvWh Who subj vp))
sentenceToWhomPrepQuestion _ = Nothing

parseQuestionIAdv ∷ SExp → Maybe (QuestionWord, [AdvPhrase])
parseQuestionIAdv (Atom "where_IAdv") = Just (Where, [])
parseQuestionIAdv (Atom "when_IAdv")  = Just (When, [])
parseQuestionIAdv (Atom "why_IAdv")   = Just (Why, [])
parseQuestionIAdv (Atom "how_IAdv")   = Just (How, [])
parseQuestionIAdv (Atom "howMuch_IAdv") = Just (HowMuch, [])
parseQuestionIAdv (List [Atom "ModIAdv", iadv, adv]) = do
  (qword, advs) <- parseQuestionIAdv iadv
  advp <- parseAdv adv
  pure (qword, advs ++ [advp])
parseQuestionIAdv _ = Nothing

parseQuestionIDet ∷ SExp → Maybe QuestionWord
parseQuestionIDet (Atom "whichSg_IDet") = Just Which
parseQuestionIDet (Atom "whichPl_IDet") = Just Which
parseQuestionIDet (Atom "howMany_IDet") = Just HowMany
parseQuestionIDet (Atom "howMuch_IDet") = Just HowMuch
parseQuestionIDet _ = Nothing

parseQuestionDetInfo ∷ SExp → Maybe (String, Maybe Number)
parseQuestionDetInfo (Atom "whichSg_IDet") = Just ("which", Just Singular)
parseQuestionDetInfo (Atom "whichPl_IDet") = Just ("which", Just Plural)
parseQuestionDetInfo (Atom "howMany_IDet") = Just ("how many", Just Plural)
parseQuestionDetInfo (Atom "howMuch_IDet") = Just ("how much", Nothing)
parseQuestionDetInfo _ = Nothing

parseQuestionDetNP ∷ SExp → SExp → Maybe NounPhrase
parseQuestionDetNP idet n = do
  (detText, detNum) <- parseQuestionDetInfo idet
  (nounNum, adjs, noun, rel) <- parseN n
  let finalNum = maybe nounNum id detNum
  case detNum of
    Just dnum | dnum /= nounNum -> Nothing
    _ | quantifierAdjNounAgreementOk finalNum adjs noun
      , questionDeterminerNounCountabilityOk detText noun ->
          pure (CommonNoun (Just detText) adjs noun finalNum rel)
      | otherwise -> Nothing

parseDetInfo ∷ SExp → Maybe (String, Maybe Number)
parseDetInfo (Atom "the_Det")   = Just ("the", Just Singular)
parseDetInfo (Atom "a_Det")     = Just ("a", Just Singular)
parseDetInfo (Atom "thePl_Det") = Just ("the", Just Plural)
parseDetInfo (Atom "aPl_Det")   = Just ("some", Just Plural)
parseDetInfo (Atom "every_Det") = Just ("every", Just Singular)
parseDetInfo (Atom "all_Det")   = Just ("all", Just Plural)
parseDetInfo (Atom "many_Det")  = Just ("many", Just Plural)
parseDetInfo (Atom "more_Det")  = Just ("more", Nothing)
parseDetInfo (Atom "fewer_Det") = Just ("fewer", Just Plural)
parseDetInfo (Atom "less_Det")  = Just ("less", Just Singular)
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
  | Just base <- stripSuffix "S_V" a  = Just (decodeLexemeStem base, ThirdSingular)
  | Just base <- stripSuffix "_V" a   = Just (decodeLexemeStem base, BaseForm)
parseV _ = Nothing

parseV2 ∷ SExp → Maybe (String, VerbForm)
parseV2 (Atom a)
  | Just base <- stripSuffix "S_V2" a
  , decodeLexemeStem base == "gulp-down" = Just ("gulp down", ThirdSingular)
  | Just base <- stripSuffix "_V2" a
  , decodeLexemeStem base == "gulp-down" = Just ("gulp down", BaseForm)
  | Just base <- stripSuffix "S_V2" a
  , decodeLexemeStem base == "clear-out" = Just ("clear out", ThirdSingular)
  | Just base <- stripSuffix "_V2" a
  , decodeLexemeStem base == "clear-out" = Just ("clear out", BaseForm)
  | Just base <- stripSuffix "S_V2" a = Just (decodeLexemeStem base, ThirdSingular)
  | Just base <- stripSuffix "_V2" a  = Just (decodeLexemeStem base, BaseForm)
parseV2 _ = Nothing

parseVV ∷ SExp → Maybe String
parseVV (Atom a)
  | Just base <- stripSuffix "_VV" a = Just (decodeLexemeStem base)
parseVV _ = Nothing

parseV2V ∷ SExp → Maybe String
parseV2V (Atom a)
  | Just base <- stripSuffix "_V2V" a = Just (decodeLexemeStem base)
parseV2V _ = Nothing

parseVS ∷ SExp → Maybe String
parseVS (Atom a)
  | Just base <- stripSuffix "_VS" a = Just (decodeLexemeStem base)
parseVS _ = Nothing

parseA ∷ SExp → Maybe String
parseA (Atom a)
  | Just base <- stripSuffix "_A" a = Just (decodeLexemeStem base)
parseA _ = Nothing

parseAP ∷ SExp → Maybe AdjPhrase
parseAP (List [Atom "PositA", a]) =
  BareAdj <$> parseA a
parseAP (List [Atom "PostPositA", a]) =
  BareAdj <$> parseA a
parseAP (List [Atom "ModAP", ada, ap]) = do
  modifier <- parseAdA ada
  base <- parseAP ap
  pure (ModifiedAdj modifier base)
parseAP (List [Atom "ConjAP", conj, ap1, ap2]) = do
  c <- parseConj conj
  left <- parseAP ap1
  right <- parseAP ap2
  pure (CoordAdj c left right)
parseAP expr =
  BareAdj <$> parseA expr

parsePostpositiveAP ∷ SExp → Maybe AdjPhrase
parsePostpositiveAP (List [Atom "PostPositA", a]) = do
  adj <- parseA a
  if isAllowedPostpositiveAdj adj
    then pure (BareAdj adj)
    else Nothing
parsePostpositiveAP (List [Atom "ModAP", ada, ap]) = do
  modifier <- parseAdA ada
  base <- parsePostpositiveAP ap
  pure (ModifiedAdj modifier base)
parsePostpositiveAP (List [Atom "ConjAP", conj, ap1, ap2]) = do
  c <- parseConj conj
  left <- parsePostpositiveAP ap1
  right <- parsePostpositiveAP ap2
  pure (CoordAdj c left right)
parsePostpositiveAP _ = Nothing

flattenPrenominalAP ∷ AdjPhrase → Maybe [String]
flattenPrenominalAP (BareAdj adj) = Just [adj]
flattenPrenominalAP (ModifiedAdj modifier base) =
  (modifier :) <$> flattenPrenominalAP base
flattenPrenominalAP (CoordAdj And left right) =
  (++) <$> flattenPrenominalAP left <*> flattenPrenominalAP right
flattenPrenominalAP (CoordAdj Or left right) = do
  leftTokens <- flattenPrenominalAP left
  rightTokens <- flattenPrenominalAP right
  pure (leftTokens ++ ["or"] ++ rightTokens)

isAllowedPostpositiveAdj ∷ String → Bool
isAllowedPostpositiveAdj adj =
  map toLower adj `elem` ["whatsoever", "sacred", "profane", "authentic"]

addRelClause ∷ Maybe RelClause → RelClause → Maybe RelClause
addRelClause Nothing newRel = Just newRel
addRelClause (Just rel) newRel = Just (RelChain rel newRel)

parseAdA ∷ SExp → Maybe String
parseAdA (Atom "much_AdA") = Just "much"
parseAdA (Atom "far_AdA") = Just "far"
parseAdA (Atom "however_AdA") = Just "however"
parseAdA (Atom "slightly_AdA") = Just "slightly"
parseAdA (Atom "less_AdA") = Just "less"
parseAdA (Atom "more_AdA") = Just "more"
parseAdA (Atom "too_AdA") = Just "too"
parseAdA (Atom "not_AdA") = Just "not"
parseAdA (Atom "altogether_AdA") = Just "altogether"
parseAdA _ = Nothing

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
parsePron (Atom "ye_Pron")   = Just (Second, Plural)
parsePron (Atom "thee_Pron") = Just (Second, Singular)
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
skipAgreement (CopulaNP _) = True
skipAgreement (CopulaAdv _) = True
skipAgreement (SeemAdj _) = True
skipAgreement (SeemNP _) = True
skipAgreement (SeemAdv _) = True
skipAgreement (FeelAdj _) = True
skipAgreement (GrowAdj _) = True
skipAgreement (GoAdj _) = True
skipAgreement (Passive _) = True
skipAgreement (Progressive _) = True
skipAgreement (Perfective _) = True
skipAgreement (VVComplement _ _) = True
skipAgreement (V2VComplement _ _ _) = True
skipAgreement (VSComplement _ _) = True
skipAgreement (PassiveVSComplement _ _) = True
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
agreementOk Conditional Positive _ _ = True
agreementOk Conditional Negative _ _ = True
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
translateSentence (SentenceWithAdv sentence adv) =
  unwords
    [ renderAdv adv
    , translateSentence sentence
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
translateSentence (Vocative sentence np) =
  unwords
    [ translateSentence sentence
    , renderNP np
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
  if possessorAllowed owner && nounNum == Singular && quantifierAdjNounAgreementOk nounNum adjs noun
    then pure (PossessedNoun owner adjs noun Singular rel)
    else Nothing
parseNP _ (List [Atom "PossPlNP", possessor, n]) = do
  owner <- parseNP Subjective possessor
  (nounNum, adjs, noun, rel) <- parseN n
  if possessorAllowed owner && nounNum == Plural && quantifierAdjNounAgreementOk nounNum adjs noun
    then pure (PossessedNoun owner adjs noun Plural rel)
    else Nothing
parseNP _ (List [Atom "DetCN", det, n]) = do
  (detText, detNum) <- parseDetInfo det
  (nounNum, adjs, noun, rel) <- parseN n
  let finalNum = maybe nounNum id detNum
  case detNum of
    Just dnum | dnum /= nounNum -> Nothing
    _ | quantifierAdjNounAgreementOk finalNum adjs noun
      , determinerNounCountabilityOk detText noun ->
          pure (CommonNoun (Just detText) adjs noun finalNum rel)
      | otherwise -> Nothing
parseNP _ (List [Atom "UseN", n]) = do
  (num, adjs, noun, rel) <- parseN n
  if quantifierAdjNounAgreementOk num adjs noun
    then pure (CommonNoun Nothing adjs noun num rel)
    else Nothing
parseNP _ (List [Atom "UsePN", pn]) = ProperNoun <$> parsePN pn
parseNP _ (Atom "ThisNP") = pure (Demonstrative "this" Singular)
parseNP _ (Atom "ThatNP") = pure (Demonstrative "that" Singular)
parseNP _ (Atom "TheseNP") = pure (Demonstrative "these" Plural)
parseNP _ (Atom "ThoseNP") = pure (Demonstrative "those" Plural)
parseNP c (List [Atom "UsePron", pr]) = do
  (p, n) <- parsePron pr
  pure (Pronoun p n c)
parseNP _ _ = Nothing

parseVP ∷ SExp → Maybe (VerbPhrase, VerbForm)
parseVP (List [Atom "AdvVP", vp, adv]) = do
  (baseVP, vf) <- parseVP vp
  advp <- parseAdv adv
  pure (VPWithAdv baseVP advp, vf)
parseVP (List [Atom "PreAdvVP", adv, vp]) = do
  advp <- parseAdV adv
  (baseVP, vf) <- parseVP vp
  pure (VPWithAdv baseVP advp, vf)
parseVP (List [Atom "PreFullAdvVP", adv, vp]) = do
  advp <- parseAdv adv
  (baseVP, vf) <- parseVP vp
  pure (VPWithAdv baseVP advp, vf)
parseVP (List [Atom "ComplVV", vv, vp]) = do
  verb <- parseVV vv
  (compVP, _) <- parseVP vp
  pure (VVComplement verb compVP, BaseForm)
parseVP (List [Atom "ComplVVCoord", vv, conj, vp1, vp2]) = do
  verb <- parseVV vv
  c <- parseConj conj
  (comp1, _) <- parseVP vp1
  (comp2, _) <- parseVP vp2
  pure (VVComplement verb (CoordVP c comp1 comp2), BaseForm)
parseVP (List [Atom "ComplVVSharedCoord", vv, conj, vp1, vp2]) = do
  verb <- parseVV vv
  c <- parseConj conj
  (comp1, _) <- parseVP vp1
  (comp2, _) <- parseVP vp2
  pure (VVComplement verb (CoordVP c comp1 comp2), BaseForm)
parseVP (List [Atom "SeriesVVSharedCoord3", vv, vp1, vp2, vp3]) = do
  verb <- parseVV vv
  (comp1, _) <- parseVP vp1
  (comp2, _) <- parseVP vp2
  (comp3, _) <- parseVP vp3
  pure
    ( VVComplement verb (CoordVP And comp1 (CoordVP And comp2 comp3))
    , BaseForm
    )
parseVP (List [Atom "SeriesVVSharedCoord4", vv, vp1, vp2, vp3, vp4]) = do
  verb <- parseVV vv
  (comp1, _) <- parseVP vp1
  (comp2, _) <- parseVP vp2
  (comp3, _) <- parseVP vp3
  (comp4, _) <- parseVP vp4
  pure
    ( VVComplement verb
        (CoordVP And comp1 (CoordVP And comp2 (CoordVP And comp3 comp4)))
    , BaseForm
    )
parseVP (List [Atom "ComplV2V", v2v, np, vp]) = do
  verb <- parseV2V v2v
  obj <- parseNP Objective np
  (compVP, _) <- parseVP vp
  pure (V2VComplement verb obj compVP, BaseForm)
parseVP (List [Atom "ComplVS", vs, s]) = do
  verb <- parseVS vs
  clause <- parseSentence s
  pure (VSComplement verb clause, BaseForm)
parseVP (List [Atom "PassVSVP", vs, s]) = do
  verb <- parseVS vs
  clause <- parseSentence s
  pure (PassiveVSComplement verb clause, BaseForm)
parseVP (List [Atom "UseV", v]) = do
  (lemma, vf) <- parseV v
  pure (Intransitive lemma, vf)
parseVP (List [Atom "UseV2", v2, np]) = do
  (lemma, vf) <- parseV2 v2
  obj <- parseNP Objective np
  pure (Transitive lemma obj, vf)
parseVP (List [Atom "UseAP", a]) = do
  adj <- parseAP a
  pure (Copula adj, ThirdSingular)
parseVP (List [Atom "UseNP", np]) = do
  complement <- parseNP Subjective np
  pure (CopulaNP complement, ThirdSingular)
parseVP (List [Atom "UseAdv", adv]) = do
  advp <- parseAdv adv
  pure (CopulaAdv advp, ThirdSingular)
parseVP (List [Atom "SeemAP", ap]) = do
  adj <- parseAP ap
  pure (SeemAdj adj, ThirdSingular)
parseVP (List [Atom "SeemNP", np]) = do
  complement <- parseNP Subjective np
  pure (SeemNP complement, ThirdSingular)
parseVP (List [Atom "SeemAdv", adv]) = do
  advp <- parseAdv adv
  pure (SeemAdv advp, ThirdSingular)
parseVP (List [Atom "FeelAP", ap]) = do
  adj <- parseAP ap
  pure (FeelAdj adj, ThirdSingular)
parseVP (List [Atom "GrowAP", ap]) = do
  adj <- parseAP ap
  pure (GrowAdj adj, ThirdSingular)
parseVP (List [Atom "GoAP", ap]) = do
  adj <- parseAP ap
  pure (GoAdj adj, ThirdSingular)
parseVP (List [Atom "PassV2VP", v2]) = do
  (lemma, _) <- parseV2 v2
  pure (Passive lemma, BaseForm)
parseVP (List [Atom "MidAdvPassV2VP", adv, v2]) = do
  advp <- parseAdv adv
  (lemma, _) <- parseV2 v2
  pure (VPWithAdv (Passive lemma) advp, BaseForm)
parseVP (List [Atom "CoordPassV2VP", v21, v22]) = do
  (lemma1, _) <- parseV2 v21
  (lemma2, _) <- parseV2 v22
  pure (CoordVP And (Passive lemma1) (Passive lemma2), BaseForm)
parseVP (List [Atom "MidAdvCoordPassV2VP", adv, v21, v22]) = do
  advp <- parseAdv adv
  (lemma1, _) <- parseV2 v21
  (lemma2, _) <- parseV2 v22
  pure (VPWithAdv (CoordVP And (Passive lemma1) (Passive lemma2)) advp, BaseForm)
parseVP (List [Atom "SeriesPassV2VP3", v21, v22, v23]) = do
  (lemma1, _) <- parseV2 v21
  (lemma2, _) <- parseV2 v22
  (lemma3, _) <- parseV2 v23
  pure
    ( CoordVP And
        (Passive lemma1)
        (CoordVP And (Passive lemma2) (Passive lemma3))
    , BaseForm
    )
parseVP (List [Atom "MidAdvSeriesPassV2VP3", adv, v21, v22, v23]) = do
  advp <- parseAdv adv
  (lemma1, _) <- parseV2 v21
  (lemma2, _) <- parseV2 v22
  (lemma3, _) <- parseV2 v23
  pure
    ( VPWithAdv
        (CoordVP And
          (Passive lemma1)
          (CoordVP And (Passive lemma2) (Passive lemma3)))
        advp
    , BaseForm
    )
parseVP (List [Atom "SeriesPassV2VP4", v21, v22, v23, v24]) = do
  (lemma1, _) <- parseV2 v21
  (lemma2, _) <- parseV2 v22
  (lemma3, _) <- parseV2 v23
  (lemma4, _) <- parseV2 v24
  pure
    ( CoordVP And
        (Passive lemma1)
        (CoordVP And
          (Passive lemma2)
          (CoordVP And (Passive lemma3) (Passive lemma4)))
    , BaseForm
    )
parseVP (List [Atom "MidAdvSeriesPassV2VP4", adv, v21, v22, v23, v24]) = do
  advp <- parseAdv adv
  (lemma1, _) <- parseV2 v21
  (lemma2, _) <- parseV2 v22
  (lemma3, _) <- parseV2 v23
  (lemma4, _) <- parseV2 v24
  pure
    ( VPWithAdv
        (CoordVP And
          (Passive lemma1)
          (CoordVP And
            (Passive lemma2)
            (CoordVP And (Passive lemma3) (Passive lemma4))))
        advp
    , BaseForm
    )
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

quantifierAdjNounAgreementOk ∷ Number → [String] → String → Bool
quantifierAdjNounAgreementOk num adjs noun =
  singularQuantOk && lessAdjOk && fewerAdjOk
  where
    lowerAdjs = map (map toLower) adjs
    singularQuantOk =
      case num of
        Singular -> not (any (`elem` ["all", "many"]) lowerAdjs)
        Plural   -> True
    lessAdjOk =
      not ("less" `elem` lowerAdjs) || (num == Singular && lessCompatible noun)
    fewerAdjOk =
      not ("fewer" `elem` lowerAdjs) || (num == Plural && fewerCompatible noun)

determinerNounCountabilityOk ∷ String → String → Bool
determinerNounCountabilityOk detText noun =
  case map toLower detText of
    "less"  -> lessCompatible noun
    "fewer" -> fewerCompatible noun
    _       -> True

questionDeterminerNounCountabilityOk ∷ String → String → Bool
questionDeterminerNounCountabilityOk detText noun =
  case map toLower detText of
    "how much" -> lessCompatible noun
    "how many" -> fewerCompatible noun
    _          -> True

lessCompatible ∷ String → Bool
lessCompatible noun
  | isMassNounHint noun = True
  | isCountNounHint noun = False
  | otherwise = True

fewerCompatible ∷ String → Bool
fewerCompatible noun = not (isMassNounHint noun)

isMassNounHint ∷ String → Bool
isMassNounHint noun =
  map toLower noun `elem`
    [ "advice"
    , "air"
    , "equipment"
    , "food"
    , "furniture"
    , "information"
    , "money"
    , "music"
    , "rice"
    , "traffic"
    , "water"
    , "work"
    ]

isCountNounHint ∷ String → Bool
isCountNounHint noun =
  map toLower noun `elem`
    [ "cat"
    , "city"
    , "day"
    , "dog"
    , "man"
    , "park"
    , "spoon"
    , "table"
    , "woman"
    , "year"
    ]

renderNP ∷ NounPhrase → String
renderNP (CoordNP c a b) =
  unwords [renderNP a, renderConj c, renderNP b]
renderNP (ProperNoun s) = fantasyToken s
renderNP (Demonstrative s _) = fantasyToken s
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
renderVP (PassiveVSComplement v sentence) =
  unwords [fantasyToken "be", fantasyToken v, fantasyToken "that", renderEmbeddedSentence sentence]
renderVP (Copula adj) = unwords [fantasyToken "be", renderAdjPhrase adj]
renderVP (CopulaNP np) = unwords [fantasyToken "be", renderNP np]
renderVP (CopulaAdv adv) = unwords [fantasyToken "be", renderAdv adv]
renderVP (SeemAdj adj) = unwords [fantasyToken "seem", renderAdjPhrase adj]
renderVP (SeemNP np) = unwords [fantasyToken "seem", renderNP np]
renderVP (SeemAdv adv) = unwords [fantasyToken "seem", renderAdv adv]
renderVP (FeelAdj adj) = unwords [fantasyToken "feel", renderAdjPhrase adj]
renderVP (GrowAdj adj) = unwords [fantasyToken "grow", renderAdjPhrase adj]
renderVP (GoAdj adj) = unwords [fantasyToken "go", renderAdjPhrase adj]
renderVP (Passive v) = unwords [fantasyToken "be", fantasyToken v]
renderVP (Progressive vp) = unwords [fantasyToken "be", renderVP vp]
renderVP (Perfective vp) = unwords [fantasyToken "have", renderVP vp]
renderVP (VPWithAdv vp adv) = unwords [renderVP vp, renderAdv adv]

renderAdjPhrase ∷ AdjPhrase → String
renderAdjPhrase (BareAdj adj) = fantasyToken adj
renderAdjPhrase (ModifiedAdj modifier adj) =
  unwords [fantasyToken modifier, renderAdjPhrase adj]
renderAdjPhrase (CoordAdj c a b) =
  unwords [renderAdjPhrase a, renderConj c, renderAdjPhrase b]

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
renderQuestionWord HowMuch = unwords [fantasyToken "how", fantasyToken "much"]

renderConj ∷ Conj → String
renderConj And = fantasyToken "and"
renderConj Or  = fantasyToken "or"

isThirdSingular ∷ NounPhrase → Bool
isThirdSingular (CoordNP _ _ _) = False
isThirdSingular (ProperNoun _) = True
isThirdSingular (Demonstrative _ Singular) = True
isThirdSingular (PossessedNoun _ _ _ Singular _) = True
isThirdSingular (CommonNoun _ _ _ Singular _) = True
isThirdSingular (Pronoun Third Singular _) = True
isThirdSingular _ = False

renderTense ∷ Tense → String
renderTense Present = "ta"
renderTense Past    = "na"
renderTense Future  = "va"
renderTense Conditional = "ca"
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
renderRelClause (RelWhoseBe noun subj) =
  unwords [fantasyToken "whose", fantasyToken noun, renderNP subj, fantasyToken "am"]
renderRelClause (RelPrep prep obj) =
  unwords [fantasyToken prep, renderNP obj]
renderRelClause (RelPrepSentence prep sentence) =
  unwords [fantasyToken prep, renderEmbeddedSentence sentence]
renderRelClause (PostAdj adj) =
  renderAdjPhrase adj
renderRelClause (RelChain left right) =
  unwords [renderRelClause left, renderRelClause right]

renderAdv ∷ AdvPhrase → String
renderAdv (PrepPhrase prep np) =
  unwords [fantasyToken prep, renderNP np]
renderAdv (ClausePhrase subj sentence) =
  unwords [fantasyToken subj, renderEmbeddedSentence sentence]
renderAdv (LexicalAdv adv) = fantasyToken adv
renderAdv (ModifiedAdv modifier adv) =
  unwords [fantasyToken modifier, renderAdv adv]
renderAdv (CoordAdv c left right) =
  unwords [renderAdv left, renderConj c, renderAdv right]

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
  sexp <- decodeExpr expr
  sentence <- parseSentence sexp
  if sentenceStructureOk sentence
    then pure (sentence, collectSymbPN sexp)
    else Nothing

sentenceStructureOk ∷ Sentence → Bool
sentenceStructureOk (Sentence _ _ subj vp) =
  nounPhraseStructureOk subj && verbPhraseStructureOk vp
sentenceStructureOk (SentenceWithAdv sentence adv) =
  sentenceStructureOk sentence && advPhraseStructureOk adv
sentenceStructureOk (Vocative sentence np) =
  sentenceStructureOk sentence && nounPhraseStructureOk np
sentenceStructureOk (Question _ _ subj vp) =
  nounPhraseStructureOk subj && verbPhraseStructureOk vp
sentenceStructureOk (WhQuestion _ _ clause) =
  whClauseStructureOk clause
sentenceStructureOk (Existential _ _ np) =
  nounPhraseStructureOk np
sentenceStructureOk (Imperative _ vp) =
  verbPhraseStructureOk vp

whClauseStructureOk ∷ WhClause → Bool
whClauseStructureOk (SubjectWh _ vp) = verbPhraseStructureOk vp
whClauseStructureOk (ObjectWh _ subj _) = nounPhraseStructureOk subj
whClauseStructureOk (SubjectDetWh _ queried vp) =
  nounPhraseStructureOk queried && verbPhraseStructureOk vp
whClauseStructureOk (ObjectDetWh _ queried subj _) =
  objectNounPhraseOk queried && nounPhraseStructureOk subj
whClauseStructureOk (AdvWh _ subj vp) =
  nounPhraseStructureOk subj && verbPhraseStructureOk vp

verbPhraseStructureOk ∷ VerbPhrase → Bool
verbPhraseStructureOk (Intransitive verb) =
  lexicalVerbHeadAllowed verb
verbPhraseStructureOk (Transitive verb obj) =
  lexicalVerbHeadAllowed verb && objectNounPhraseOk obj
verbPhraseStructureOk (VVComplement _ vp) = verbPhraseStructureOk vp
verbPhraseStructureOk (V2VComplement _ obj vp) =
  objectNounPhraseOk obj && verbPhraseStructureOk vp
verbPhraseStructureOk (VSComplement _ sentence) = sentenceStructureOk sentence
verbPhraseStructureOk (PassiveVSComplement _ sentence) = sentenceStructureOk sentence
verbPhraseStructureOk (Copula adj) = adjPhraseStructureOk adj
verbPhraseStructureOk (CopulaNP np) = nounPhraseStructureOk np
verbPhraseStructureOk (CopulaAdv adv) = advPhraseStructureOk adv
verbPhraseStructureOk (SeemAdj adj) = adjPhraseStructureOk adj
verbPhraseStructureOk (SeemNP np) = nounPhraseStructureOk np
verbPhraseStructureOk (SeemAdv adv) = advPhraseStructureOk adv
verbPhraseStructureOk (FeelAdj adj) = adjPhraseStructureOk adj
verbPhraseStructureOk (GrowAdj adj) = adjPhraseStructureOk adj
verbPhraseStructureOk (GoAdj adj) = adjPhraseStructureOk adj
verbPhraseStructureOk (Passive _) = True
verbPhraseStructureOk (Progressive vp) = verbPhraseStructureOk vp
verbPhraseStructureOk (Perfective vp) = verbPhraseStructureOk vp
verbPhraseStructureOk (VPWithAdv vp adv) =
  verbPhraseStructureOk vp && advPhraseStructureOk adv
verbPhraseStructureOk (CoordVP _ a b) =
  verbPhraseStructureOk a && verbPhraseStructureOk b

advPhraseStructureOk ∷ AdvPhrase → Bool
advPhraseStructureOk (PrepPhrase _ np) = nounPhraseStructureOk np
advPhraseStructureOk (ClausePhrase _ sentence) = sentenceStructureOk sentence
advPhraseStructureOk (LexicalAdv _) = True
advPhraseStructureOk (ModifiedAdv _ adv) = advPhraseStructureOk adv
advPhraseStructureOk (CoordAdv _ left right) =
  advPhraseStructureOk left && advPhraseStructureOk right

adjPhraseStructureOk ∷ AdjPhrase → Bool
adjPhraseStructureOk (BareAdj adj) = not (null adj)
adjPhraseStructureOk (ModifiedAdj modifier adj) =
  not (null modifier) && adjPhraseStructureOk adj
adjPhraseStructureOk (CoordAdj _ a b) =
  adjPhraseStructureOk a && adjPhraseStructureOk b

nounPhraseStructureOk ∷ NounPhrase → Bool
nounPhraseStructureOk (ProperNoun _) = True
nounPhraseStructureOk (Demonstrative _ _) = True
nounPhraseStructureOk (Pronoun _ _ _) = True
nounPhraseStructureOk (CommonNoun _ adjs noun _ relClause) =
  commonNounHeadAllowed adjs noun && maybe True relClauseStructureOk relClause
nounPhraseStructureOk (PossessedNoun owner adjs noun _ relClause) =
  nounPhraseStructureOk owner
    && commonNounHeadAllowed adjs noun
    && maybe True relClauseStructureOk relClause
nounPhraseStructureOk (CoordNP _ a b) =
  nounPhraseStructureOk a && nounPhraseStructureOk b

relClauseStructureOk ∷ RelClause → Bool
relClauseStructureOk (RelVP vp) = verbPhraseStructureOk vp
relClauseStructureOk (NegRelVP vp) = verbPhraseStructureOk vp
relClauseStructureOk (RelV2 _ np) = objectNounPhraseOk np
relClauseStructureOk (NegRelV2 _ np) = objectNounPhraseOk np
relClauseStructureOk (RelWhoseBe _ np) = nounPhraseStructureOk np
relClauseStructureOk (RelPrep _ np) = nounPhraseStructureOk np
relClauseStructureOk (RelPrepSentence _ sentence) = sentenceStructureOk sentence
relClauseStructureOk (PostAdj adj) = adjPhraseStructureOk adj
relClauseStructureOk (RelChain left right) =
  relClauseStructureOk left && relClauseStructureOk right

objectNounPhraseOk ∷ NounPhrase → Bool
objectNounPhraseOk np =
  nounPhraseStructureOk np && not (isIllFormedBareSingularObject np)

isIllFormedBareSingularObject ∷ NounPhrase → Bool
isIllFormedBareSingularObject (CommonNoun Nothing _ noun Singular Nothing) =
  not (bareSingularObjectNounAllowed noun)
isIllFormedBareSingularObject (CoordNP _ a b) =
  isIllFormedBareSingularObject a || isIllFormedBareSingularObject b
isIllFormedBareSingularObject _ = False

bareSingularObjectNounAllowed ∷ String → Bool
bareSingularObjectNounAllowed noun
  | isMassNounHint noun = True
  | isIndefinitePronounLike noun = True
  | otherwise =
      map toLower noun `elem`
        [ "breakfast"
        , "dinner"
        , "fun"
        , "harm"
        , "help"
        , "home"
        , "lunch"
        , "news"
        , "progress"
        , "research"
        , "school"
        , "sight"
        , "trouble"
        , "work"
        ]

isIndefinitePronounLike ∷ String → Bool
isIndefinitePronounLike noun =
  hasIndefinitePrefix lower && hasIndefinitePronounSuffix lower
  where
    lower = map toLower noun

hasIndefinitePrefix ∷ String → Bool
hasIndefinitePrefix noun =
  any (`isPrefixOf` noun) ["any", "every", "no", "some"]

hasIndefinitePronounSuffix ∷ String → Bool
hasIndefinitePronounSuffix noun =
  any (`isSuffixOf` noun) ["body", "one", "thing"]

commonNounHeadAllowed ∷ [String] → String → Bool
commonNounHeadAllowed adjs noun =
  case map toLower noun of
    "how" ->
      map (map toLower) adjs == ["much"]
    lowerNoun ->
      lowerNoun `notElem`
        [ "all"
        , "many"
        , "much"
        ]

lexicalVerbHeadAllowed ∷ String → Bool
lexicalVerbHeadAllowed verb =
  map toLower verb `notElem`
    [ "all"
    , "fewer"
    , "how"
    , "less"
    , "many"
    , "more"
    , "much"
    ]
