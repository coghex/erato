{-# LANGUAGE Strict, UnicodeSyntax #-}

module Parser.GFParser
  ( GrammarBundle(..)
  , loadGrammars
  , parseControlled
  , parseControlledSentences
  , parsePreferredControlledSentence
  , parseFallbackAllEng
  , parseFallbackSentences
  , parsePreferredFallbackSentence
  ) where

import Data.List (minimumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import PGF
import Parser.AST (AdvPhrase(..), NounPhrase(..), Number(..), RelClause(..), Sentence(..), VerbPhrase(..), WhClause(..))
import Parser.Translate (exprToSentence, validateExpr)

data GrammarBundle = GrammarBundle
  { controlledPgf   ∷ PGF
  , fallbackPgf     ∷ PGF
  , controlledLang  ∷ CId
  , controlledMorpho ∷ Morpho
  }

loadGrammars ∷ FilePath → FilePath → IO GrammarBundle
loadGrammars controlledPath fallbackPath = do
  c <- readPGF controlledPath
  f <- readPGF fallbackPath
  let lang   = mkCId "EratoEng"
      morpho = buildMorpho c lang
  pure (GrammarBundle c f lang morpho)

parseControlled ∷ GrammarBundle → String → [Expr]
parseControlled bundle input =
  let pgf    = controlledPgf bundle
      lang   = controlledLang bundle
      morpho = controlledMorpho bundle
      typ    = startCat pgf
      parses = parse pgf lang typ input
  in filter (validParse morpho) parses

parseControlledSentences ∷ GrammarBundle → String → [Sentence]
parseControlledSentences bundle input =
  let pgf    = controlledPgf bundle
      lang   = controlledLang bundle
      morpho = controlledMorpho bundle
      typ    = startCat pgf
      parses = parse pgf lang typ input
  in mapMaybe (validatedSentence morpho) parses

parsePreferredControlledSentence ∷ GrammarBundle → String → Maybe Sentence
parsePreferredControlledSentence bundle input =
  preferSentence (parseControlledSentences bundle input)

parseFallbackAllEng ∷ GrammarBundle → String → [Expr]
parseFallbackAllEng bundle input =
  let pgf  = fallbackPgf bundle
      lang = mkCId "AllEng"
      typ  = startCat pgf
  in parse pgf lang typ input

parseFallbackSentences ∷ GrammarBundle → String → [Sentence]
parseFallbackSentences bundle input =
  let pgf  = fallbackPgf bundle
      lang = mkCId "AllEng"
      typ  = startCat pgf
  in mapMaybe exprToSentence (parse pgf lang typ input)

parsePreferredFallbackSentence ∷ GrammarBundle → String → Maybe Sentence
parsePreferredFallbackSentence bundle input =
  preferSentence (parseFallbackSentences bundle input)

validParse ∷ Morpho → Expr → Bool
validParse morpho expr =
  case validatedSentence morpho expr of
    Just _  → True
    Nothing → False

validatedSentence ∷ Morpho → Expr → Maybe Sentence
validatedSentence morpho expr =
  case validateExpr expr of
    Nothing         → Nothing
    Just (sentence, pns)
      -- SymbPN is a fallback for tokens that are not already recognized by the
      -- controlled grammar's morphology. Keeping known words out of this path
      -- avoids extra proper-noun parses for ordinary vocabulary.
      | all (\w → null (lookupMorpho morpho w)) pns → Just sentence
      | otherwise                                   → Nothing

preferSentence ∷ [Sentence] → Maybe Sentence
preferSentence [] = Nothing
preferSentence sentences =
  Just (minimumBy (comparing sentencePenalty) sentences)

sentencePenalty ∷ Sentence → (Int, Int, Int)
sentencePenalty sentence =
  ( sentenceFormPenalty sentence
  , sentenceLexicalPenalty sentence
  , sentenceBareNounPenalty sentence
  )

sentenceFormPenalty ∷ Sentence → Int
sentenceFormPenalty (Imperative _ _) = 5
sentenceFormPenalty _                = 0

sentenceLexicalPenalty ∷ Sentence → Int
sentenceLexicalPenalty (Sentence _ _ subj vp) = nounPhraseLexicalPenalty subj + verbPhraseLexicalPenalty vp
sentenceLexicalPenalty (Question _ _ subj vp) = nounPhraseLexicalPenalty subj + verbPhraseLexicalPenalty vp
sentenceLexicalPenalty (WhQuestion _ _ whClause) = whClauseLexicalPenalty whClause
sentenceLexicalPenalty (Existential _ _ np)   = nounPhraseLexicalPenalty np
sentenceLexicalPenalty (Imperative _ vp)      = verbPhraseLexicalPenalty vp

sentenceBareNounPenalty ∷ Sentence → Int
sentenceBareNounPenalty (Sentence _ _ subj vp) = nounPhraseBarePenalty subj + verbPhraseBarePenalty vp
sentenceBareNounPenalty (Question _ _ subj vp) = nounPhraseBarePenalty subj + verbPhraseBarePenalty vp
sentenceBareNounPenalty (WhQuestion _ _ whClause) = whClauseBarePenalty whClause
sentenceBareNounPenalty (Existential _ _ np)   = nounPhraseBarePenalty np
sentenceBareNounPenalty (Imperative _ vp)      = verbPhraseBarePenalty vp

whClauseLexicalPenalty ∷ WhClause → Int
whClauseLexicalPenalty (SubjectWh _ vp) = verbPhraseLexicalPenalty vp
whClauseLexicalPenalty (ObjectWh _ subj verb) =
  nounPhraseLexicalPenalty subj + functionWordPenalty verb
whClauseLexicalPenalty (AdvWh _ subj vp) =
  nounPhraseLexicalPenalty subj + verbPhraseLexicalPenalty vp

whClauseBarePenalty ∷ WhClause → Int
whClauseBarePenalty (SubjectWh _ vp) = verbPhraseBarePenalty vp
whClauseBarePenalty (ObjectWh _ subj _) = nounPhraseBarePenalty subj
whClauseBarePenalty (AdvWh _ subj vp) =
  nounPhraseBarePenalty subj + verbPhraseBarePenalty vp

nounPhraseLexicalPenalty ∷ NounPhrase → Int
nounPhraseLexicalPenalty (ProperNoun _) = 0
nounPhraseLexicalPenalty (Pronoun _ _ _) = 0
nounPhraseLexicalPenalty (CommonNoun _ _ noun _ rel) =
  functionWordPenalty noun + maybe 0 relClausePenalty rel
nounPhraseLexicalPenalty (CoordNP _ a b) =
  nounPhraseLexicalPenalty a + nounPhraseLexicalPenalty b

nounPhraseBarePenalty ∷ NounPhrase → Int
nounPhraseBarePenalty (ProperNoun _) = 0
nounPhraseBarePenalty (Pronoun _ _ _) = 0
nounPhraseBarePenalty (CommonNoun Nothing _ _ Singular rel) =
  4 + maybe 0 relClauseBarePenalty rel
nounPhraseBarePenalty (CommonNoun Nothing _ _ Plural rel) =
  1 + maybe 0 relClauseBarePenalty rel
nounPhraseBarePenalty (CommonNoun _ _ _ _ rel) =
  maybe 0 relClauseBarePenalty rel
nounPhraseBarePenalty (CoordNP _ a b) =
  nounPhraseBarePenalty a + nounPhraseBarePenalty b

verbPhraseLexicalPenalty ∷ VerbPhrase → Int
verbPhraseLexicalPenalty (Intransitive verb) = functionWordPenalty verb
verbPhraseLexicalPenalty (Transitive verb obj) =
  functionWordPenalty verb + nounPhraseLexicalPenalty obj
verbPhraseLexicalPenalty (Copula adj) = functionWordPenalty adj
verbPhraseLexicalPenalty (Passive verb) = functionWordPenalty verb
verbPhraseLexicalPenalty (Progressive vp) = verbPhraseLexicalPenalty vp
verbPhraseLexicalPenalty (VPWithAdv vp adv) =
  verbPhraseLexicalPenalty vp + advPhraseLexicalPenalty adv
verbPhraseLexicalPenalty (CoordVP _ a b) =
  verbPhraseLexicalPenalty a + verbPhraseLexicalPenalty b

verbPhraseBarePenalty ∷ VerbPhrase → Int
verbPhraseBarePenalty (Intransitive _) = 0
verbPhraseBarePenalty (Transitive _ obj) = nounPhraseBarePenalty obj
verbPhraseBarePenalty (Copula _) = 0
verbPhraseBarePenalty (Passive _) = 0
verbPhraseBarePenalty (Progressive vp) = verbPhraseBarePenalty vp
verbPhraseBarePenalty (VPWithAdv vp adv) =
  verbPhraseBarePenalty vp + advPhraseBarePenalty adv
verbPhraseBarePenalty (CoordVP _ a b) =
  verbPhraseBarePenalty a + verbPhraseBarePenalty b

advPhraseLexicalPenalty ∷ AdvPhrase → Int
advPhraseLexicalPenalty (PrepPhrase _ np) = nounPhraseLexicalPenalty np

advPhraseBarePenalty ∷ AdvPhrase → Int
advPhraseBarePenalty (PrepPhrase _ np) = nounPhraseBarePenalty np

relClausePenalty ∷ RelClause → Int
relClausePenalty (RelVP vp) = verbPhraseLexicalPenalty vp
relClausePenalty (NegRelVP vp) = verbPhraseLexicalPenalty vp
relClausePenalty (RelV2 verb np) = functionWordPenalty verb + nounPhraseLexicalPenalty np
relClausePenalty (NegRelV2 verb np) = functionWordPenalty verb + nounPhraseLexicalPenalty np

relClauseBarePenalty ∷ RelClause → Int
relClauseBarePenalty (RelVP vp) = verbPhraseBarePenalty vp
relClauseBarePenalty (NegRelVP vp) = verbPhraseBarePenalty vp
relClauseBarePenalty (RelV2 _ np) = nounPhraseBarePenalty np
relClauseBarePenalty (NegRelV2 _ np) = nounPhraseBarePenalty np

functionWordPenalty ∷ String → Int
functionWordPenalty word
  | word `elem` ["be", "can", "could", "do", "may", "must", "shall", "should", "will", "would"] = 3
  | otherwise = 0
