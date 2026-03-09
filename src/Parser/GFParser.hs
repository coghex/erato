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

import Data.Char (isAlpha, isAlphaNum, isUpper, toLower)
import Data.List (isSuffixOf, minimumBy)
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
  let pgf            = controlledPgf bundle
      lang           = controlledLang bundle
      morpho         = controlledMorpho bundle
      typ            = startCat pgf
      rewrittenInput = normalizeSentenceInitialPronoun (normalizeDegreeModifiers (normalizeContractions (tokenizeInput input)))
      normalized     = normalizePossessives rewrittenInput
      parseInput
        | possessiveMarkerCount normalized > 0 = normalizedInput normalized
        | otherwise = normalizeSentenceInitialTokenCase morpho (normalizedInput normalized)
      parses         = parse pgf lang typ parseInput
      validParses    = mapMaybe (\expr -> fmap (\sentence -> (sentence, expr)) (validatedSentence morpho expr)) parses
      filteredParses = filterPossessiveParses normalized validParses
  in map snd filteredParses

parseControlledSentences ∷ GrammarBundle → String → [Sentence]
parseControlledSentences bundle input =
  let pgf            = controlledPgf bundle
      lang           = controlledLang bundle
      morpho         = controlledMorpho bundle
      typ            = startCat pgf
      rewrittenInput = normalizeSentenceInitialPronoun (normalizeDegreeModifiers (normalizeContractions (tokenizeInput input)))
      normalized     = normalizePossessives rewrittenInput
      parseInput
        | possessiveMarkerCount normalized > 0 = normalizedInput normalized
        | otherwise = normalizeSentenceInitialTokenCase morpho (normalizedInput normalized)
      parses         = parse pgf lang typ parseInput
      validParses    = mapMaybe (\expr -> fmap (\sentence -> (sentence, expr)) (validatedSentence morpho expr)) parses
      filteredParses = filterPossessiveParses normalized validParses
  in map fst filteredParses

parsePreferredControlledSentence ∷ GrammarBundle → String → Maybe Sentence
parsePreferredControlledSentence bundle input =
  preferSentence (parseControlledSentences bundle input)

parseFallbackAllEng ∷ GrammarBundle → String → [Expr]
parseFallbackAllEng bundle input =
  let pgf            = fallbackPgf bundle
      lang           = mkCId "AllEng"
      morpho         = controlledMorpho bundle
      typ            = startCat pgf
      rewrittenInput = normalizeSentenceInitialPronoun (normalizeDegreeModifiers (normalizeContractions (tokenizeInput input)))
      normalized     = normalizePossessives rewrittenInput
      parseInput
        | possessiveMarkerCount normalized > 0 = normalizedInput normalized
        | otherwise = normalizeSentenceInitialTokenCase morpho (normalizedInput normalized)
      parses         = parse pgf lang typ parseInput
      parsedSentences = mapMaybe (\expr -> fmap (\sentence -> (sentence, expr)) (exprToSentence expr)) parses
      filteredParses = filterPossessiveParses normalized parsedSentences
  in map snd filteredParses

parseFallbackSentences ∷ GrammarBundle → String → [Sentence]
parseFallbackSentences bundle input =
  let pgf            = fallbackPgf bundle
      lang           = mkCId "AllEng"
      morpho         = controlledMorpho bundle
      typ            = startCat pgf
      rewrittenInput = normalizeSentenceInitialPronoun (normalizeDegreeModifiers (normalizeContractions (tokenizeInput input)))
      normalized     = normalizePossessives rewrittenInput
      parseInput
        | possessiveMarkerCount normalized > 0 = normalizedInput normalized
        | otherwise = normalizeSentenceInitialTokenCase morpho (normalizedInput normalized)
      parses         = parse pgf lang typ parseInput
      parsedSentences = mapMaybe (\expr -> fmap (\sentence -> (sentence, expr)) (exprToSentence expr)) parses
      filteredParses = filterPossessiveParses normalized parsedSentences
  in map fst filteredParses

parsePreferredFallbackSentence ∷ GrammarBundle → String → Maybe Sentence
parsePreferredFallbackSentence bundle input =
  preferSentence (parseFallbackSentences bundle input)

data NormalizedInput = NormalizedInput
  { normalizedInput        ∷ String
  , possessiveMarkerCount ∷ Int
  }

tokenizeInput ∷ String → String
tokenizeInput input =
  let roughTokens = words (map normalizeTokenChar input)
      cleanedTokens = map cleanToken roughTokens
  in unwords (filter (not . null) cleanedTokens)

normalizeTokenChar ∷ Char → Char
normalizeTokenChar c
  | c == '’' = '\''
  | isAlphaNum c || c == '\'' = c
  | otherwise = ' '

cleanToken ∷ String → String
cleanToken token =
  let noLeadingQuotes = dropWhile (== '\'') token
  in trimTrailingQuotes noLeadingQuotes

trimTrailingQuotes ∷ String → String
trimTrailingQuotes token
  | tokenHasPluralPossessiveQuote token = token
  | otherwise = reverse (dropWhile (== '\'') (reverse token))

tokenHasPluralPossessiveQuote ∷ String → Bool
tokenHasPluralPossessiveQuote token =
  "'" `isSuffixOf` token && hasPluralPossessiveStem token "'"

normalizeContractions ∷ String → String
normalizeContractions input =
  unwords (concatMap normalizeContractionToken (words input))

normalizeDegreeModifiers ∷ String → String
normalizeDegreeModifiers input =
  unwords (collapseDegreeModifierTokens (words input))

collapseDegreeModifierTokens ∷ [String] → [String]
collapseDegreeModifierTokens [] = []
collapseDegreeModifierTokens [token] = [token]
collapseDegreeModifierTokens (first : second : third : rest)
  | isHowMuchPrefix first second =
      first : second : collapseDegreeModifierTokens (third : rest)
  | isALotDegreeModifier first second && isDegreeHead third =
      third : collapseDegreeModifierTokens rest
  | isABitDegreeModifier first second && isDegreeHead third =
      third : collapseDegreeModifierTokens rest
collapseDegreeModifierTokens (first : second : rest)
  | isSimpleDegreeModifier first && isDegreeHead second =
      second : collapseDegreeModifierTokens rest
  | otherwise =
      first : collapseDegreeModifierTokens (second : rest)

isSimpleDegreeModifier ∷ String → Bool
isSimpleDegreeModifier token =
  map toLower token `elem` ["much", "far", "slightly"]

isALotDegreeModifier ∷ String → String → Bool
isALotDegreeModifier first second =
  map toLower first == "a" && map toLower second == "lot"

isABitDegreeModifier ∷ String → String → Bool
isABitDegreeModifier first second =
  map toLower first == "a" && map toLower second == "bit"

isHowMuchPrefix ∷ String → String → Bool
isHowMuchPrefix first second =
  map toLower first == "how" && map toLower second == "much"

isDegreeHead ∷ String → Bool
isDegreeHead token =
  let lower = map toLower token
  in lower `elem` ["more", "fewer", "less"] || isComparativeAdjectiveToken lower

isComparativeAdjectiveToken ∷ String → Bool
isComparativeAdjectiveToken token =
  length token > 3 && "er" `isSuffixOf` token && all isAlpha token

normalizeContractionToken ∷ String → [String]
normalizeContractionToken token =
  case contractionExpansion token of
    Just expanded -> expanded
    Nothing       -> [token]

contractionExpansion ∷ String → Maybe [String]
contractionExpansion token =
  let normalized = canonicalizeApostrophes token
      lower = map toLower normalized
  in case lower of
       "can't"   -> Just ["can", "not"]
       "cannot"  -> Just ["can", "not"]
       "won't"   -> Just ["will", "not"]
       "shan't"  -> Just ["shall", "not"]
       "i'm"     -> Just ["I", "am"]
       "you're"  -> Just ["you", "are"]
       "he's"    -> Just ["he", "is"]
       "she's"   -> Just ["she", "is"]
       "it's"    -> Just ["it", "is"]
       "we're"   -> Just ["we", "are"]
       "they're" -> Just ["they", "are"]
       "that's"  -> Just ["that", "is"]
       "there's" -> Just ["there", "is"]
       "what's"  -> Just ["what", "is"]
       "who's"   -> Just ["who", "is"]
       "where's" -> Just ["where", "is"]
       "i've"    -> Just ["I", "have"]
       "you've"  -> Just ["you", "have"]
       "we've"   -> Just ["we", "have"]
       "they've" -> Just ["they", "have"]
       "i'll"    -> Just ["I", "will"]
       "you'll"  -> Just ["you", "will"]
       "he'll"   -> Just ["he", "will"]
       "she'll"  -> Just ["she", "will"]
       "it'll"   -> Just ["it", "will"]
       "we'll"   -> Just ["we", "will"]
       "they'll" -> Just ["they", "will"]
       "i'd"     -> Just ["I", "would"]
       "you'd"   -> Just ["you", "would"]
       "he'd"    -> Just ["he", "would"]
       "she'd"   -> Just ["she", "would"]
       "it'd"    -> Just ["it", "would"]
       "we'd"    -> Just ["we", "would"]
       "they'd"  -> Just ["they", "would"]
       _ | Just stem <- stripSuffix "n't" lower ->
             Just [stem, "not"]
         | otherwise ->
             Nothing

canonicalizeApostrophes ∷ String → String
canonicalizeApostrophes = map (\c -> if c == '’' then '\'' else c)

normalizePossessives ∷ String → NormalizedInput
normalizePossessives input =
  let tokens = words input
      normalizedTokens = map normalizePossessiveToken tokens
  in NormalizedInput
       { normalizedInput = unwords normalizedTokens
       , possessiveMarkerCount = length (filter isPossessiveToken tokens)
       }

normalizeSentenceInitialPronoun ∷ String → String
normalizeSentenceInitialPronoun input =
  case words input of
    [] -> input
    firstToken : rest ->
      case rewriteInitialPronounToken firstToken of
        Nothing -> input
        Just rewritten -> unwords (rewritten ++ rest)

rewriteInitialPronounToken ∷ String → Maybe [String]
rewriteInitialPronounToken "i" = Just ["I"]
rewriteInitialPronounToken "i'm" = Just ["I", "am"]
rewriteInitialPronounToken "I'm" = Just ["I", "am"]
rewriteInitialPronounToken "i’m" = Just ["I", "am"]
rewriteInitialPronounToken "I’m" = Just ["I", "am"]
rewriteInitialPronounToken "i've" = Just ["I", "have"]
rewriteInitialPronounToken "I've" = Just ["I", "have"]
rewriteInitialPronounToken "i’ve" = Just ["I", "have"]
rewriteInitialPronounToken "I’ve" = Just ["I", "have"]
rewriteInitialPronounToken "i'll" = Just ["I", "will"]
rewriteInitialPronounToken "I'll" = Just ["I", "will"]
rewriteInitialPronounToken "i’ll" = Just ["I", "will"]
rewriteInitialPronounToken "I’ll" = Just ["I", "will"]
rewriteInitialPronounToken _ = Nothing

normalizeSentenceInitialTokenCase ∷ Morpho → String → String
normalizeSentenceInitialTokenCase morpho input =
  case words input of
    [] -> input
    firstToken : rest ->
      let normalizedFirst = case normalizeSentenceInitialWord morpho firstToken of
            Just word -> word
            Nothing   -> firstToken
      in unwords (normalizedFirst : rest)

normalizeSentenceInitialWord ∷ Morpho → String → Maybe String
normalizeSentenceInitialWord morpho token
  | token `elem` sentenceInitialCaseExceptions = Nothing
  | otherwise = do
      lowered <- lowercaseInitial token
      if null (lookupMorpho morpho lowered)
        then Nothing
        else Just lowered

lowercaseInitial ∷ String → Maybe String
lowercaseInitial [] = Nothing
lowercaseInitial (c:cs)
  | isUpper c = Just (toLower c : cs)
  | otherwise = Nothing

sentenceInitialCaseExceptions ∷ [String]
sentenceInitialCaseExceptions =
  [ "I", "I'm", "I’m", "I'll", "I’ll", "I'd", "I’d", "I've", "I’ve" ]

normalizePossessiveToken ∷ String → String
normalizePossessiveToken token
  | Just stem <- stripSuffix "'s" token = stem
  | Just stem <- stripSuffix "’s" token = stem
  | Just stem <- stripSuffix "'" token
  , not (null stem)
  , last stem == 's' = stem
  | Just stem <- stripSuffix "’" token
  , not (null stem)
  , last stem == 's' = stem
  | otherwise = token

isPossessiveToken ∷ String → Bool
isPossessiveToken token =
  any (`isSuffixOf` token) ["'s", "’s"]
  || any (\suffix -> suffix `isSuffixOf` token && hasPluralPossessiveStem token suffix) ["'", "’"]

hasPluralPossessiveStem ∷ String → String → Bool
hasPluralPossessiveStem token suffix =
  case stripSuffix suffix token of
    Just stem -> not (null stem) && last stem == 's'
    Nothing   -> False

stripSuffix ∷ String → String → Maybe String
stripSuffix suffix s
  | suffix `isSuffixOf` s = Just (take (length s - length suffix) s)
  | otherwise             = Nothing

filterPossessiveParses ∷ NormalizedInput → [(Sentence, a)] → [(Sentence, a)]
filterPossessiveParses normalized parses
  | possessiveMarkerCount normalized == 0 = parses
  | otherwise =
      let matching = filter ((== possessiveMarkerCount normalized) . sentencePossessiveCount . fst) parses
      in if null matching then parses else matching

sentencePossessiveCount ∷ Sentence → Int
sentencePossessiveCount (Sentence _ _ subj vp) = nounPhrasePossessiveCount subj + verbPhrasePossessiveCount vp
sentencePossessiveCount (Question _ _ subj vp) = nounPhrasePossessiveCount subj + verbPhrasePossessiveCount vp
sentencePossessiveCount (WhQuestion _ _ whClause) = whClausePossessiveCount whClause
sentencePossessiveCount (Existential _ _ np) = nounPhrasePossessiveCount np
sentencePossessiveCount (Imperative _ vp) = verbPhrasePossessiveCount vp

nounPhrasePossessiveCount ∷ NounPhrase → Int
nounPhrasePossessiveCount (ProperNoun _) = 0
nounPhrasePossessiveCount (Pronoun _ _ _) = 0
nounPhrasePossessiveCount (CommonNoun _ _ _ _ rel) = maybe 0 relClausePossessiveCount rel
nounPhrasePossessiveCount (PossessedNoun owner _ _ _ rel) =
  1 + nounPhrasePossessiveCount owner + maybe 0 relClausePossessiveCount rel
nounPhrasePossessiveCount (CoordNP _ a b) =
  nounPhrasePossessiveCount a + nounPhrasePossessiveCount b

verbPhrasePossessiveCount ∷ VerbPhrase → Int
verbPhrasePossessiveCount (Intransitive _) = 0
verbPhrasePossessiveCount (Transitive _ np) = nounPhrasePossessiveCount np
verbPhrasePossessiveCount (VVComplement _ vp) = verbPhrasePossessiveCount vp
verbPhrasePossessiveCount (V2VComplement _ np vp) =
  nounPhrasePossessiveCount np + verbPhrasePossessiveCount vp
verbPhrasePossessiveCount (VSComplement _ sentence) = sentencePossessiveCount sentence
verbPhrasePossessiveCount (Copula _) = 0
verbPhrasePossessiveCount (Passive _) = 0
verbPhrasePossessiveCount (Progressive vp) = verbPhrasePossessiveCount vp
verbPhrasePossessiveCount (Perfective vp) = verbPhrasePossessiveCount vp
verbPhrasePossessiveCount (VPWithAdv vp adv) = verbPhrasePossessiveCount vp + advPhrasePossessiveCount adv
verbPhrasePossessiveCount (CoordVP _ a b) = verbPhrasePossessiveCount a + verbPhrasePossessiveCount b

whClausePossessiveCount ∷ WhClause → Int
whClausePossessiveCount (SubjectWh _ vp) = verbPhrasePossessiveCount vp
whClausePossessiveCount (ObjectWh _ subj _) = nounPhrasePossessiveCount subj
whClausePossessiveCount (SubjectDetWh _ queried vp) =
  nounPhrasePossessiveCount queried + verbPhrasePossessiveCount vp
whClausePossessiveCount (ObjectDetWh _ queried subj _) =
  nounPhrasePossessiveCount queried + nounPhrasePossessiveCount subj
whClausePossessiveCount (AdvWh _ subj vp) = nounPhrasePossessiveCount subj + verbPhrasePossessiveCount vp

relClausePossessiveCount ∷ RelClause → Int
relClausePossessiveCount (RelVP vp) = verbPhrasePossessiveCount vp
relClausePossessiveCount (NegRelVP vp) = verbPhrasePossessiveCount vp
relClausePossessiveCount (RelV2 _ np) = nounPhrasePossessiveCount np
relClausePossessiveCount (NegRelV2 _ np) = nounPhrasePossessiveCount np
relClausePossessiveCount (RelPrep _ np) = nounPhrasePossessiveCount np

advPhrasePossessiveCount ∷ AdvPhrase → Int
advPhrasePossessiveCount (PrepPhrase _ np) = nounPhrasePossessiveCount np
advPhrasePossessiveCount (ClausePhrase _ sentence) = sentencePossessiveCount sentence
advPhrasePossessiveCount (LexicalAdv _) = 0
advPhrasePossessiveCount (ModifiedAdv _ adv) = advPhrasePossessiveCount adv

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
whClauseLexicalPenalty (SubjectDetWh _ queried vp) =
  nounPhraseLexicalPenalty queried + verbPhraseLexicalPenalty vp
whClauseLexicalPenalty (ObjectDetWh _ queried subj verb) =
  nounPhraseLexicalPenalty queried + nounPhraseLexicalPenalty subj + functionWordPenalty verb
whClauseLexicalPenalty (AdvWh _ subj vp) =
  nounPhraseLexicalPenalty subj + verbPhraseLexicalPenalty vp

whClauseBarePenalty ∷ WhClause → Int
whClauseBarePenalty (SubjectWh _ vp) = verbPhraseBarePenalty vp
whClauseBarePenalty (ObjectWh _ subj _) = nounPhraseBarePenalty subj
whClauseBarePenalty (SubjectDetWh _ queried vp) =
  nounPhraseBarePenalty queried + verbPhraseBarePenalty vp
whClauseBarePenalty (ObjectDetWh _ queried subj _) =
  nounPhraseBarePenalty queried + nounPhraseBarePenalty subj
whClauseBarePenalty (AdvWh _ subj vp) =
  nounPhraseBarePenalty subj + verbPhraseBarePenalty vp

nounPhraseLexicalPenalty ∷ NounPhrase → Int
nounPhraseLexicalPenalty (ProperNoun _) = 0
nounPhraseLexicalPenalty (Pronoun _ _ _) = 0
nounPhraseLexicalPenalty (PossessedNoun owner _ noun _ rel) =
  nounPhraseLexicalPenalty owner + functionWordPenalty noun + maybe 0 relClausePenalty rel
nounPhraseLexicalPenalty (CommonNoun _ _ noun _ rel) =
  functionWordPenalty noun + maybe 0 relClausePenalty rel
nounPhraseLexicalPenalty (CoordNP _ a b) =
  nounPhraseLexicalPenalty a + nounPhraseLexicalPenalty b

nounPhraseBarePenalty ∷ NounPhrase → Int
nounPhraseBarePenalty (ProperNoun _) = 0
nounPhraseBarePenalty (Pronoun _ _ _) = 0
nounPhraseBarePenalty (PossessedNoun _ _ _ _ rel) =
  maybe 0 relClauseBarePenalty rel
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
verbPhraseLexicalPenalty (VVComplement verb vp) =
  functionWordPenalty verb + verbPhraseLexicalPenalty vp
verbPhraseLexicalPenalty (V2VComplement verb obj vp) =
  functionWordPenalty verb + nounPhraseLexicalPenalty obj + verbPhraseLexicalPenalty vp
verbPhraseLexicalPenalty (VSComplement verb sentence) =
  functionWordPenalty verb + sentenceLexicalPenalty sentence
verbPhraseLexicalPenalty (Copula adj) = functionWordPenalty adj
verbPhraseLexicalPenalty (Passive verb) = functionWordPenalty verb
verbPhraseLexicalPenalty (Progressive vp) = verbPhraseLexicalPenalty vp
verbPhraseLexicalPenalty (Perfective vp) = verbPhraseLexicalPenalty vp
verbPhraseLexicalPenalty (VPWithAdv vp adv) =
  verbPhraseLexicalPenalty vp + advPhraseLexicalPenalty adv
verbPhraseLexicalPenalty (CoordVP _ a b) =
  verbPhraseLexicalPenalty a + verbPhraseLexicalPenalty b

verbPhraseBarePenalty ∷ VerbPhrase → Int
verbPhraseBarePenalty (Intransitive _) = 0
verbPhraseBarePenalty (Transitive _ obj) = nounPhraseBarePenalty obj
verbPhraseBarePenalty (VVComplement _ vp) = verbPhraseBarePenalty vp
verbPhraseBarePenalty (V2VComplement _ obj vp) =
  nounPhraseBarePenalty obj + verbPhraseBarePenalty vp
verbPhraseBarePenalty (VSComplement _ sentence) = sentenceBareNounPenalty sentence
verbPhraseBarePenalty (Copula _) = 0
verbPhraseBarePenalty (Passive _) = 0
verbPhraseBarePenalty (Progressive vp) = verbPhraseBarePenalty vp
verbPhraseBarePenalty (Perfective vp) = verbPhraseBarePenalty vp
verbPhraseBarePenalty (VPWithAdv vp adv) =
  verbPhraseBarePenalty vp + advPhraseBarePenalty adv
verbPhraseBarePenalty (CoordVP _ a b) =
  verbPhraseBarePenalty a + verbPhraseBarePenalty b

advPhraseLexicalPenalty ∷ AdvPhrase → Int
advPhraseLexicalPenalty (PrepPhrase _ np) = nounPhraseLexicalPenalty np
advPhraseLexicalPenalty (ClausePhrase _ sentence) = sentenceLexicalPenalty sentence
advPhraseLexicalPenalty (LexicalAdv adv) = functionWordPenalty adv
advPhraseLexicalPenalty (ModifiedAdv modifier adv) =
  functionWordPenalty modifier + advPhraseLexicalPenalty adv

advPhraseBarePenalty ∷ AdvPhrase → Int
advPhraseBarePenalty (PrepPhrase _ np) = nounPhraseBarePenalty np
advPhraseBarePenalty (ClausePhrase _ sentence) = sentenceBareNounPenalty sentence
advPhraseBarePenalty (LexicalAdv _) = 0
advPhraseBarePenalty (ModifiedAdv _ adv) = advPhraseBarePenalty adv

relClausePenalty ∷ RelClause → Int
relClausePenalty (RelVP vp) = verbPhraseLexicalPenalty vp
relClausePenalty (NegRelVP vp) = verbPhraseLexicalPenalty vp
relClausePenalty (RelV2 verb np) = functionWordPenalty verb + nounPhraseLexicalPenalty np
relClausePenalty (NegRelV2 verb np) = functionWordPenalty verb + nounPhraseLexicalPenalty np
relClausePenalty (RelPrep _ np) = nounPhraseLexicalPenalty np

relClauseBarePenalty ∷ RelClause → Int
relClauseBarePenalty (RelVP vp) = verbPhraseBarePenalty vp
relClauseBarePenalty (NegRelVP vp) = verbPhraseBarePenalty vp
relClauseBarePenalty (RelV2 _ np) = nounPhraseBarePenalty np
relClauseBarePenalty (NegRelV2 _ np) = nounPhraseBarePenalty np
relClauseBarePenalty (RelPrep _ np) = nounPhraseBarePenalty np

functionWordPenalty ∷ String → Int
functionWordPenalty word
  | word `elem` ["be", "can", "could", "do", "may", "must", "shall", "should", "will", "would"] = 3
  | otherwise = 0
