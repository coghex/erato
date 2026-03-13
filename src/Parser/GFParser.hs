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
import Parser.AST (AdvPhrase(..), NounPhrase(..), Number(..), QuestionWord(..), RelClause(..), Sentence(..), VerbPhrase(..), WhClause(..))
import Parser.Translate (exprToSentence, validateExpr)

data GrammarBundle = GrammarBundle
  { controlledPgf   ∷ PGF
  , fallbackPgf     ∷ PGF
  , controlledLang  ∷ CId
  , fallbackLang    ∷ Maybe CId
  , controlledMorpho ∷ Morpho
  }

data ParsedParse a = ParsedParse
  { parsedSentence ∷ Sentence
  , parsedValue    ∷ a
  , parsedMetrics  ∷ SentenceMetrics
  }

data SentenceMetrics = SentenceMetrics
  { metricPossessiveCount     ∷ Int
  , metricFormPenalty         ∷ Int
  , metricLexicalPenalty      ∷ Int
  , metricDisambiguationPenalty ∷ Int
  , metricBareNounPenalty     ∷ Int
  }

loadGrammars ∷ FilePath → FilePath → IO GrammarBundle
loadGrammars controlledPath fallbackPath = do
  c <- readPGF controlledPath
  f <- readPGF fallbackPath
  let lang   = mkCId "EratoEng"
      morpho = buildMorpho c lang
      fallback = case languages f of
        l : _ -> Just l
        [] -> Nothing
  pure (GrammarBundle c f lang fallback morpho)

parseControlled ∷ GrammarBundle → String → [Expr]
parseControlled bundle input =
  map parsedValue (parseControlledResults bundle input)

parseControlledSentences ∷ GrammarBundle → String → [Sentence]
parseControlledSentences bundle input =
  map parsedSentence (parseControlledResults bundle input)

parsePreferredControlledSentence ∷ GrammarBundle → String → Maybe Sentence
parsePreferredControlledSentence bundle input =
  fmap parsedSentence (preferParsedParse (parseControlledResults bundle input))

parseFallbackAllEng ∷ GrammarBundle → String → [Expr]
parseFallbackAllEng bundle input =
  map parsedValue (parseFallbackResults bundle input)

parseFallbackSentences ∷ GrammarBundle → String → [Sentence]
parseFallbackSentences bundle input =
  map parsedSentence (parseFallbackResults bundle input)

parsePreferredFallbackSentence ∷ GrammarBundle → String → Maybe Sentence
parsePreferredFallbackSentence bundle input =
  fmap parsedSentence (preferParsedParse (parseFallbackResults bundle input))

data NormalizedInput = NormalizedInput
  { normalizedInput        ∷ String
  , possessiveMarkerCount ∷ Int
  }

parseControlledResults ∷ GrammarBundle → String → [ParsedParse Expr]
parseControlledResults bundle input =
  let pgf                = controlledPgf bundle
      lang               = controlledLang bundle
      morpho             = controlledMorpho bundle
      typ                = startCat pgf
      (normalized, text) = prepareParseInput morpho input
      parses             = parse pgf lang typ text
      validParses        = mapMaybe (validatedParse morpho) parses
  in filterPossessiveParses normalized validParses

parseFallbackResults ∷ GrammarBundle → String → [ParsedParse Expr]
parseFallbackResults bundle input =
  let pgf                = fallbackPgf bundle
      mLang              = fallbackLang bundle
      morpho             = controlledMorpho bundle
      typ                = startCat pgf
      (normalized, text) = prepareParseInput morpho input
      parses             = case mLang of
        Just lang -> parse pgf lang typ text
        Nothing -> concat (parseAll pgf typ text)
      parsedSentences    = mapMaybe fallbackParsedParse parses
  in filterPossessiveParses normalized parsedSentences

prepareParseInput ∷ Morpho → String → (NormalizedInput, String)
prepareParseInput morpho input =
  let rewrittenInput = normalizeSentenceInitialPronoun (normalizeDegreeModifiers (normalizeObjectComparativeWh (normalizeQuestionNegationOrder (normalizeContractions (tokenizeInput input)))))
      normalized = normalizePossessives rewrittenInput
      parseInput
        | possessiveMarkerCount normalized > 0 = normalizedInput normalized
        | otherwise = normalizeSentenceInitialTokenCase morpho (normalizedInput normalized)
  in (normalized, parseInput)

validatedParse ∷ Morpho → Expr → Maybe (ParsedParse Expr)
validatedParse morpho expr = do
  sentence <- validatedSentence morpho expr
  pure (makeParsedParse sentence expr)

fallbackParsedParse ∷ Expr → Maybe (ParsedParse Expr)
fallbackParsedParse expr = do
  sentence <- exprToSentence expr
  pure (makeParsedParse sentence expr)

makeParsedParse ∷ Sentence → a → ParsedParse a
makeParsedParse sentence value =
  ParsedParse
    { parsedSentence = sentence
    , parsedValue = value
    , parsedMetrics = sentenceMetrics sentence
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

normalizeQuestionNegationOrder ∷ String → String
normalizeQuestionNegationOrder input =
  unwords (rewriteQuestionNegationTokens (words input))

normalizeObjectComparativeWh ∷ String → String
normalizeObjectComparativeWh input =
  unwords (rewriteObjectComparativeWhTokens (words input))

rewriteQuestionNegationTokens ∷ [String] → [String]
rewriteQuestionNegationTokens (wh : aux : "not" : det : noun : rest)
  | isWhToken wh && isInversionAux aux && isDetToken det =
      [wh, aux, det, noun, "not"] ++ rest
rewriteQuestionNegationTokens (wh : aux : "not" : subj : rest)
  | isWhToken wh && isInversionAux aux =
      [wh, aux, subj, "not"] ++ rest
rewriteQuestionNegationTokens tokens = tokens

rewriteObjectComparativeWhTokens ∷ [String] → [String]
rewriteObjectComparativeWhTokens (wh : noun : aux : rest)
  | map toLower wh == "which"
  , isObjectWhAux aux =
      case break isObjectComparativeMatrixVerb rest of
        (subj, matrixVerb : compVerb : compTail)
          | not (null subj)
          , isLikelyBareVerbToken compVerb
          , containsComparativeAdverbToken compTail ->
              [wh, noun, "that", inflectRelativeVerb aux noun compVerb]
                ++ compTail ++ [aux] ++ subj ++ [matrixVerb]
        _ -> wh : noun : aux : rest
rewriteObjectComparativeWhTokens tokens = tokens

isObjectWhAux ∷ String → Bool
isObjectWhAux token =
  map toLower token `elem` ["do", "does", "did"]

isObjectComparativeMatrixVerb ∷ String → Bool
isObjectComparativeMatrixVerb token =
  map toLower token `elem` ["see", "watch", "hear", "make"]

isLikelyBareVerbToken ∷ String → Bool
isLikelyBareVerbToken token =
  let lower = map toLower token
  in all isAlpha lower && not (null lower) && lower `notElem` nonVerbTokens
  where
    nonVerbTokens =
      [ "the", "a", "an", "this", "that", "these", "those"
      , "my", "your", "his", "her", "its", "our", "their"
      , "i", "you", "he", "she", "it", "we", "they", "me", "him", "her", "us", "them"
      , "not", "and", "or", "to", "than"
      , "do", "does", "did", "is", "are", "was", "were", "has", "have", "had"
      ]

containsComparativeAdverbToken ∷ [String] → Bool
containsComparativeAdverbToken =
  any (isComparativeAdverbLike . map toLower)

inflectRelativeVerb ∷ String → String → String → String
inflectRelativeVerb aux noun verb
  | map toLower aux == "did" = pastForm baseVerb
  | looksPlural noun = baseVerb
  | otherwise = thirdPersonSingularForm baseVerb
  where
    baseVerb = map toLower verb

pastForm ∷ String → String
pastForm verb =
  case verb of
    "run" -> "ran"
    "eat" -> "ate"
    "go" -> "went"
    "be" -> "was"
    "have" -> "had"
    "do" -> "did"
    "see" -> "saw"
    "hear" -> "heard"
    "make" -> "made"
    _ -> regularPastForm verb

thirdPersonSingularForm ∷ String → String
thirdPersonSingularForm verb
  | endsWithAny ["s", "x", "z", "ch", "sh", "o"] verb = verb ++ "es"
  | hasConsonantYEnding verb = init verb ++ "ies"
  | otherwise = verb ++ "s"

regularPastForm ∷ String → String
regularPastForm verb
  | "e" `isSuffixOf` verb = verb ++ "d"
  | hasConsonantYEnding verb = init verb ++ "ied"
  | otherwise = verb ++ "ed"

hasConsonantYEnding ∷ String → Bool
hasConsonantYEnding word =
  case reverse word of
    'y' : prev : _ -> not (isVowel prev)
    _ -> False

isVowel ∷ Char → Bool
isVowel c =
  c `elem` ("aeiou" ∷ String)

endsWithAny ∷ [String] → String → Bool
endsWithAny suffixes word =
  any (`isSuffixOf` word) suffixes

looksPlural ∷ String → Bool
looksPlural noun =
  let lower = map toLower noun
  in length lower > 1 && "s" `isSuffixOf` lower && not ("ss" `isSuffixOf` lower)

isWhToken ∷ String → Bool
isWhToken token =
  map toLower token `elem` ["why", "where", "when", "how", "what", "who", "which"]

isInversionAux ∷ String → Bool
isInversionAux token =
  map toLower token `elem`
    [ "do", "does", "did", "is", "are", "was", "were", "has", "have", "had"
    , "will", "would", "can", "could", "shall", "should", "may", "might", "must"
    ]

isDetToken ∷ String → Bool
isDetToken token =
  map toLower token `elem`
    [ "the", "a", "an", "this", "that", "these", "those"
    , "my", "your", "his", "her", "its", "our", "their"
    ]

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
       "doesn't" -> Just ["does", "not"]
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

filterPossessiveParses ∷ NormalizedInput → [ParsedParse a] → [ParsedParse a]
filterPossessiveParses normalized parses
  | possessiveMarkerCount normalized == 0 = parses
  | otherwise =
      let matching =
            filter
              ((== possessiveMarkerCount normalized) . metricPossessiveCount . parsedMetrics)
              parses
      in if null matching then parses else matching

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


preferParsedParse ∷ [ParsedParse a] → Maybe (ParsedParse a)
preferParsedParse [] = Nothing
preferParsedParse parses =
  Just (minimumBy (comparing (metricsPenaltyTuple . parsedMetrics)) parses)

emptySentenceMetrics ∷ SentenceMetrics
emptySentenceMetrics = SentenceMetrics 0 0 0 0 0

sentenceMetricsDelta ∷ Int → Int → Int → Int → Int → SentenceMetrics
sentenceMetricsDelta poss form lexical disamb bare =
  SentenceMetrics poss form lexical disamb bare

plusSentenceMetrics ∷ SentenceMetrics → SentenceMetrics → SentenceMetrics
plusSentenceMetrics left right =
  SentenceMetrics
    { metricPossessiveCount =
        metricPossessiveCount left + metricPossessiveCount right
    , metricFormPenalty =
        metricFormPenalty left + metricFormPenalty right
    , metricLexicalPenalty =
        metricLexicalPenalty left + metricLexicalPenalty right
    , metricDisambiguationPenalty =
        metricDisambiguationPenalty left + metricDisambiguationPenalty right
    , metricBareNounPenalty =
        metricBareNounPenalty left + metricBareNounPenalty right
    }

applyFormPenalty ∷ Int → SentenceMetrics → SentenceMetrics
applyFormPenalty penalty metrics =
  metrics
    { metricFormPenalty = metricFormPenalty metrics + penalty
    }

embeddedSentenceMetrics ∷ Sentence → SentenceMetrics
embeddedSentenceMetrics sentence =
  let metrics = sentenceMetrics sentence
  in metrics { metricFormPenalty = 0 }

metricsPenaltyTuple ∷ SentenceMetrics → (Int, Int, Int, Int)
metricsPenaltyTuple metrics =
  ( metricFormPenalty metrics
  , metricLexicalPenalty metrics
  , metricDisambiguationPenalty metrics
  , metricBareNounPenalty metrics
  )

sentenceMetrics ∷ Sentence → SentenceMetrics
sentenceMetrics (Sentence _ _ subj vp) =
  nounPhraseMetrics subj `plusSentenceMetrics` verbPhraseMetrics vp
sentenceMetrics (Question _ _ subj vp) =
  nounPhraseMetrics subj `plusSentenceMetrics` verbPhraseMetrics vp
sentenceMetrics (WhQuestion _ _ whClause) =
  whClauseMetrics whClause
sentenceMetrics (Existential _ _ np) =
  nounPhraseMetrics np
sentenceMetrics (Imperative _ vp) =
  applyFormPenalty 5 (verbPhraseMetrics vp)

whClauseMetrics ∷ WhClause → SentenceMetrics
whClauseMetrics (SubjectWh _ vp) =
  verbPhraseMetrics vp
whClauseMetrics (ObjectWh _ subj verb) =
  nounPhraseMetrics subj
    `plusSentenceMetrics` sentenceMetricsDelta 0 0 (functionWordPenalty verb) 0 0
whClauseMetrics (SubjectDetWh _ queried vp) =
  nounPhraseMetrics queried `plusSentenceMetrics` verbPhraseMetrics vp
whClauseMetrics (ObjectDetWh qword queried subj verb) =
  nounPhraseMetrics queried
    `plusSentenceMetrics` nounPhraseMetrics subj
    `plusSentenceMetrics` sentenceMetricsDelta 0 0 (functionWordPenalty verb) (objectDetComparativeAdverbPenalty qword queried) 0
whClauseMetrics (AdvWh _ subj vp) =
  nounPhraseMetrics subj `plusSentenceMetrics` verbPhraseMetrics vp

nounPhraseMetrics ∷ NounPhrase → SentenceMetrics
nounPhraseMetrics (ProperNoun _) = emptySentenceMetrics
nounPhraseMetrics (Pronoun _ _ _) = emptySentenceMetrics
nounPhraseMetrics (CommonNoun detTxt _ noun number relClause) =
  maybe emptySentenceMetrics relClauseMetrics relClause
    `plusSentenceMetrics` sentenceMetricsDelta 0 0 (functionWordPenalty noun) 0 barePenalty
  where
    barePenalty =
      case detTxt of
        Nothing ->
          case number of
            Singular -> 4
            Plural -> 1
        Just _ -> 0
nounPhraseMetrics (PossessedNoun owner _ noun _ relClause) =
  nounPhraseMetrics owner
    `plusSentenceMetrics` maybe emptySentenceMetrics relClauseMetrics relClause
    `plusSentenceMetrics` sentenceMetricsDelta 1 0 (functionWordPenalty noun) 0 0
nounPhraseMetrics (CoordNP _ a b) =
  nounPhraseMetrics a `plusSentenceMetrics` nounPhraseMetrics b

verbPhraseMetrics ∷ VerbPhrase → SentenceMetrics
verbPhraseMetrics (Intransitive verb) =
  sentenceMetricsDelta 0 0 (functionWordPenalty verb) (ambiguousVerbHeadPenalty verb) 0
verbPhraseMetrics (Transitive verb obj) =
  nounPhraseMetrics obj
    `plusSentenceMetrics` sentenceMetricsDelta 0 0 (functionWordPenalty verb) disambPenalty 0
  where
    disambPenalty =
      ambiguousVerbHeadPenalty verb + transitiveObjectAmbiguityPenalty obj
verbPhraseMetrics (VVComplement verb vp) =
  verbPhraseMetrics vp
    `plusSentenceMetrics` sentenceMetricsDelta 0 0 (functionWordPenalty verb) 0 0
verbPhraseMetrics (V2VComplement verb obj vp) =
  nounPhraseMetrics obj
    `plusSentenceMetrics` verbPhraseMetrics vp
    `plusSentenceMetrics` sentenceMetricsDelta 0 0 (functionWordPenalty verb) 0 0
verbPhraseMetrics (VSComplement verb sentence) =
  embeddedSentenceMetrics sentence
    `plusSentenceMetrics` sentenceMetricsDelta 0 0 (functionWordPenalty verb) 0 0
verbPhraseMetrics (Copula adj) =
  sentenceMetricsDelta 0 0 (functionWordPenalty adj) 0 0
verbPhraseMetrics (Passive verb) =
  sentenceMetricsDelta 0 0 (functionWordPenalty verb) 0 0
verbPhraseMetrics (Progressive vp) =
  verbPhraseMetrics vp
verbPhraseMetrics (Perfective vp) =
  verbPhraseMetrics vp
verbPhraseMetrics (VPWithAdv vp adv) =
  verbPhraseMetrics vp `plusSentenceMetrics` advPhraseMetrics adv
verbPhraseMetrics (CoordVP _ a b) =
  verbPhraseMetrics a `plusSentenceMetrics` verbPhraseMetrics b

advPhraseMetrics ∷ AdvPhrase → SentenceMetrics
advPhraseMetrics (PrepPhrase _ np) =
  nounPhraseMetrics np
advPhraseMetrics (ClausePhrase _ sentence) =
  embeddedSentenceMetrics sentence
advPhraseMetrics (LexicalAdv adv) =
  sentenceMetricsDelta 0 0 (functionWordPenalty adv) 0 0
advPhraseMetrics (ModifiedAdv modifier adv) =
  advPhraseMetrics adv
    `plusSentenceMetrics` sentenceMetricsDelta 0 0 (functionWordPenalty modifier) 0 0

relClauseMetrics ∷ RelClause → SentenceMetrics
relClauseMetrics (RelVP vp) =
  verbPhraseMetrics vp
relClauseMetrics (NegRelVP vp) =
  verbPhraseMetrics vp
relClauseMetrics (RelV2 verb np) =
  nounPhraseMetrics np
    `plusSentenceMetrics` sentenceMetricsDelta 0 0 (functionWordPenalty verb) 0 0
relClauseMetrics (NegRelV2 verb np) =
  nounPhraseMetrics np
    `plusSentenceMetrics` sentenceMetricsDelta 0 0 (functionWordPenalty verb) 0 0
relClauseMetrics (RelPrep _ np) =
  nounPhraseMetrics np

transitiveObjectAmbiguityPenalty ∷ NounPhrase → Int
transitiveObjectAmbiguityPenalty (CommonNoun Nothing adjs noun Singular Nothing)
  | looksLikeAdverbialObject adjs noun = 3
transitiveObjectAmbiguityPenalty _ = 0

looksLikeAdverbialObject ∷ [String] → String → Bool
looksLikeAdverbialObject adjs noun =
  isComparativeAdverbLike nounLower || hasDegreeModifierAdj adjsLower
  where
    nounLower = map toLower noun
    adjsLower = map (map toLower) adjs
    hasDegreeModifierAdj xs =
      any (`elem` ["much", "far", "slightly", "less", "more"]) xs
        && (isComparativeAdverbLike nounLower
            || nounLower `elem` ["quick", "slow", "fast", "hard", "easy", "late", "early"])

isComparativeAdverbLike ∷ String → Bool
isComparativeAdverbLike token =
  token `elem` ["better", "worse", "faster", "slower", "harder", "sooner", "later"]
    || (length token > 3 && "er" `isSuffixOf` token && all isAlpha token)

objectDetComparativeAdverbPenalty ∷ QuestionWord → NounPhrase → Int
objectDetComparativeAdverbPenalty HowMuch (CommonNoun (Just detTxt) [] noun Singular Nothing)
  | map toLower detTxt == "how much"
    && isComparativeAdverbLike (map toLower noun) = 3
objectDetComparativeAdverbPenalty _ _ = 0

ambiguousVerbHeadPenalty ∷ String → Int
ambiguousVerbHeadPenalty verb
  | lower `elem` ["better", "worse", "faster", "slower", "harder", "sooner", "later", "far"] = 4
  | otherwise = 0
  where
    lower = map toLower verb

functionWordPenalty ∷ String → Int
functionWordPenalty word
  | word `elem` ["be", "can", "could", "do", "may", "must", "shall", "should", "will", "would"] = 3
  | otherwise = 0
