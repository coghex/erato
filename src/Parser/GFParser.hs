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

import Data.Char (isAlpha, isAlphaNum, isSpace, isUpper, toLower, toUpper)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (mapMaybe, maybeToList)
import PGF
import Parser.AST (AdjPhrase(..), AdvPhrase(..), Conj(..), NounPhrase(..), Number(..), Person(..), Polarity(..), PronounCase(..), QuestionWord(..), RelClause(..), Sentence(..), Tense(..), VerbPhrase(..), WhClause(..))
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

data PreferredSelection a = PreferredSelection
  { preferredAnyParse        ∷ Maybe (ParsedParse a)
  , preferredPossessiveParse ∷ Maybe (ParsedParse a)
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
  case directLeadNPSentence bundle input of
    Just sentence -> Just sentence
    Nothing -> fmap parsedSentence (parsePreferredControlledResult bundle input)

parseFallbackAllEng ∷ GrammarBundle → String → [Expr]
parseFallbackAllEng bundle input =
  map parsedValue (parseFallbackExprResults bundle input)

parseFallbackSentences ∷ GrammarBundle → String → [Sentence]
parseFallbackSentences bundle input =
  map parsedSentence (parseFallbackResults bundle input)

parsePreferredFallbackSentence ∷ GrammarBundle → String → Maybe Sentence
parsePreferredFallbackSentence bundle input =
  case parsePreferredFallbackExprResult bundle input of
    Just parsed -> Just (parsedSentence parsed)
    Nothing -> fmap parsedSentence (singleWordFallbackParse bundle input)

data NormalizedInput = NormalizedInput
  { normalizedInput        ∷ String
  , possessiveMarkerCount ∷ Int
  }

type TextRewrite = String → String

parsePreferredControlledResult ∷ GrammarBundle → String → Maybe (ParsedParse Expr)
parsePreferredControlledResult bundle input =
  let pgf                = controlledPgf bundle
      lang               = controlledLang bundle
      morpho             = controlledMorpho bundle
      typ                = startCat pgf
      preparedInputs     = prepareParseInputs morpho input
      normalized         = primaryNormalizedInput input preparedInputs
      desiredPossessives = preferredPossessiveCount normalized
  in selectPreferredPreparedInputParse
       desiredPossessives
       preparedInputs
       (\text -> [parse pgf lang typ text])
       (validatedParse morpho)

parsePreferredFallbackExprResult ∷ GrammarBundle → String → Maybe (ParsedParse Expr)
parsePreferredFallbackExprResult bundle input =
  let pgf                = fallbackPgf bundle
      mLang              = fallbackLang bundle
      morpho             = controlledMorpho bundle
      typ                = startCat pgf
      preparedInputs     = prepareParseInputs morpho input
      normalized         = primaryNormalizedInput input preparedInputs
      desiredPossessives = preferredPossessiveCount normalized
      parseBatches text =
        case mLang of
          Just lang -> [parse pgf lang typ text]
          Nothing -> parseAll pgf typ text
  in selectPreferredPreparedInputParse
       desiredPossessives
       preparedInputs
       parseBatches
       fallbackParsedParse

parseControlledResults ∷ GrammarBundle → String → [ParsedParse Expr]
parseControlledResults bundle input =
  case directLeadNPUttResult bundle input of
    Just parsed -> [parsed]
    Nothing ->
      let pgf                = controlledPgf bundle
          lang               = controlledLang bundle
          morpho             = controlledMorpho bundle
          typ                = startCat pgf
          preparedInputs     = prepareParseInputs morpho input
          normalized         = primaryNormalizedInput input preparedInputs
          parses             = concatMap (\(_, text) → parse pgf lang typ text) preparedInputs
          validParses        = mapMaybe (validatedParse morpho) parses
      in filterPossessiveParses normalized validParses

parseFallbackExprResults ∷ GrammarBundle → String → [ParsedParse Expr]
parseFallbackExprResults bundle input =
  let pgf                = fallbackPgf bundle
      mLang              = fallbackLang bundle
      morpho             = controlledMorpho bundle
      typ                = startCat pgf
      preparedInputs     = prepareParseInputs morpho input
      normalized         = primaryNormalizedInput input preparedInputs
      parses             = concatMap
        (\(_, text) →
          case mLang of
            Just lang -> parse pgf lang typ text
            Nothing -> concat (parseAll pgf typ text))
        preparedInputs
      parsedSentences    = mapMaybe fallbackParsedParse parses
  in filterPossessiveParses normalized parsedSentences

parseFallbackResults ∷ GrammarBundle → String → [ParsedParse Sentence]
parseFallbackResults bundle input =
  let exprParses = parseFallbackExprResults bundle input
  in if null exprParses
       then maybeToList (singleWordFallbackParse bundle input)
       else map parsedSentenceResult exprParses

parsedSentenceResult ∷ ParsedParse a → ParsedParse Sentence
parsedSentenceResult parsed =
  ParsedParse
    { parsedSentence = parsedSentence parsed
    , parsedValue = parsedSentence parsed
    , parsedMetrics = parsedMetrics parsed
    }

singleWordFallbackParse ∷ GrammarBundle → String → Maybe (ParsedParse Sentence)
singleWordFallbackParse bundle input =
  let morpho = controlledMorpho bundle
      preparedInputs = prepareParseInputs morpho input
      candidates =
        [ token
        | (normalized, _) <- preparedInputs
        , [token] <- [words (normalizedInput normalized)]
        ]
  in case candidates of
       word : _ -> Just (makeParsedParse (SingleWord word) (SingleWord word))
       []       -> Nothing

prepareParseInputs ∷ Morpho → String → [(NormalizedInput, String)]
prepareParseInputs morpho input =
  let tokenizedInputs =
        tokenizeInputVariants input
          ++ extraNormalizedTokenizeInputs input
      rewrittenInputs =
        concatMap (prepareTokenizedParseInputs morpho) (uniqueStrings tokenizedInputs)
  in map (finalizeParseInput morpho) (uniqueStrings rewrittenInputs)

extraNormalizedTokenizeInputs ∷ String → [String]
extraNormalizedTokenizeInputs input =
  let normalized = normalizeSerialCommaLists input
  in if normalized == input
       then []
       else tokenizeInputVariants normalized

prepareTokenizedParseInputs ∷ Morpho → String → [String]
prepareTokenizedParseInputs morpho tokenizedInput =
  concatMap
    (prioritizedRewriteVariants . rewriteParseInput morpho)
    [ tokenizedInput
    , normalizeKnownTokenCase morpho tokenizedInput
    ]

rewriteParseInput ∷ Morpho → String → String
rewriteParseInput morpho =
  applyTextRewrites
    [ normalizeContractions
    , normalizeQuestionNegationOrder
    , normalizeObjectComparativeWh
    , normalizeComparativeCorrelatives
    , normalizeDegreeModifiers
    , normalizeLiteraryNames
    , normalizeArchaicEnglish morpho
    , normalizeSentenceInitialBut
    , normalizeSentenceInitialPronoun
    ]

applyTextRewrites ∷ [TextRewrite] → TextRewrite
applyTextRewrites rewrites input =
  foldl' (\rewritten rewrite → rewrite rewritten) input rewrites

strippedAsAffordingTail ∷ String → String
strippedAsAffordingTail input =
  case splitBeforeAsAffording (words input) of
    Just prefixTokens
      | not (null prefixTokens) ->
          unwords prefixTokens
    _ ->
      input

prioritizedRewriteVariants ∷ String → [String]
prioritizedRewriteVariants input =
  case simplifiedTouchingAsWellAsAffordingFrame input of
    simplified
      | simplified /= input -> [simplified]
    _ -> [input]

simplifiedTouchingAsWellAsAffordingFrame ∷ String → String
simplifiedTouchingAsWellAsAffordingFrame input =
  let stripped = strippedAsAffordingTail input
  in if mentionsTouchingWellAsAndAffording input
       then unwords (dropHereAppearingPair (words stripped))
       else input

mentionsTouchingWellAsAndAffording ∷ String → Bool
mentionsTouchingWellAsAndAffording input =
  let lower = map toLower input
  in "as touching " `isPrefixOf` lower
      && " as well as " `isInfixOf` lower
      && " as affording " `isInfixOf` lower

dropHereAppearingPair ∷ [String] → [String]
dropHereAppearingPair ("here" : "appearing" : rest) =
  dropHereAppearingPair rest
dropHereAppearingPair (token : rest) =
  token : dropHereAppearingPair rest
dropHereAppearingPair [] = []

splitBeforeAsAffording ∷ [String] → Maybe [String]
splitBeforeAsAffording =
  go []
  where
    go _ [] = Nothing
    go _ [_] = Nothing
    go acc (first : second : tailTokens)
      | map toLower first == "as"
      , map toLower second == "affording" =
          Just (reverse acc)
      | otherwise =
          go (first : acc) (second : tailTokens)

normalizeArchaicEnglish ∷ Morpho → String → String
normalizeArchaicEnglish morpho =
  unwords . rewriteArchaicTokens morpho . words

rewriteArchaicTokens ∷ Morpho → [String] → [String]
rewriteArchaicTokens _ [] = []
rewriteArchaicTokens morpho (token : next : rest)
  | isThouToken token =
      rewriteArchaicPronounToken token
        : rewriteThouVerbToken morpho next
        : rewriteArchaicTokens morpho rest
rewriteArchaicTokens morpho (token : rest) =
  rewriteStandaloneArchaicToken morpho token
    : rewriteArchaicTokens morpho rest

isThouToken ∷ String → Bool
isThouToken token =
  map toLower token == "thou"

rewriteArchaicPronounToken ∷ String → String
rewriteArchaicPronounToken token =
  matchTokenCase token "you"

rewriteStandaloneArchaicToken ∷ Morpho → String → String
rewriteStandaloneArchaicToken morpho token
  | lowerToken == "thy" =
      matchTokenCase token "your"
  | lowerToken == "thine" =
      matchTokenCase token "yours"
  | lowerToken == "thyself" =
      matchTokenCase token "yourself"
  | Just modern <- archaicThirdSingularModernForm morpho lowerToken =
      matchTokenCase token modern
  | otherwise =
      token
  where
    lowerToken = map toLower token

rewriteThouVerbToken ∷ Morpho → String → String
rewriteThouVerbToken morpho token =
  case archaicSecondSingularModernForm morpho (map toLower token) of
    Just modern ->
      matchTokenCase token modern
    Nothing ->
      rewriteStandaloneArchaicToken morpho token

archaicSecondSingularModernForm ∷ Morpho → String → Maybe String
archaicSecondSingularModernForm morpho token =
  case token of
    "art" -> Just "are"
    "hast" -> Just "have"
    "dost" -> Just "do"
    "wilt" -> Just "will"
    "shalt" -> Just "shall"
    "wert" -> Just "were"
    "hadst" -> Just "had"
    "didst" -> Just "did"
    _
      | "est" `isSuffixOf` token ->
          let stem = dropEnd 3 token
              candidates = stem : [stem ++ "e"]
          in findKnownMorphoForm morpho candidates
      | otherwise ->
          Nothing

archaicThirdSingularModernForm ∷ Morpho → String → Maybe String
archaicThirdSingularModernForm morpho token =
  case token of
    "hath" -> Just "has"
    "doth" -> Just "does"
    "saith" -> Just "says"
    _
      | "eth" `isSuffixOf` token ->
          let stem = dropEnd 3 token
              candidates = archaicStemCandidates stem
          in findKnownInflectedForm morpho candidates
      | otherwise ->
          Nothing

findKnownInflectedForm ∷ Morpho → [String] → Maybe String
findKnownInflectedForm morpho =
  findKnownMorphoForm morpho . map thirdPersonSingularForm

findKnownMorphoForm ∷ Morpho → [String] → Maybe String
findKnownMorphoForm morpho =
  firstMatching (\candidate → not (null (lookupMorpho morpho candidate)))

firstMatching ∷ (a → Bool) → [a] → Maybe a
firstMatching _ [] = Nothing
firstMatching predicate (candidate : rest)
  | predicate candidate = Just candidate
  | otherwise = firstMatching predicate rest

dropEnd ∷ Int → String → String
dropEnd n token =
  take (length token - n) token

archaicStemCandidates ∷ String → [String]
archaicStemCandidates stem
  | prefersSilentEStem stem =
      [stem ++ "e", stem]
  | otherwise =
      [stem, stem ++ "e"]

prefersSilentEStem ∷ String → Bool
prefersSilentEStem stem =
  length stem <= 3 && not (null stem)

matchTokenCase ∷ String → String → String
matchTokenCase source target =
  case source of
    first : _
      | isUpper first ->
          capitalize target
    _ ->
      target

capitalize ∷ String → String
capitalize [] = []
capitalize (first : rest) =
  toUpper first : rest

normalizeLiteraryNames ∷ String → String
normalizeLiteraryNames input =
  unwords (rewriteLiteraryNameTokens (words input))

rewriteLiteraryNameTokens ∷ [String] → [String]
rewriteLiteraryNameTokens [] = []
rewriteLiteraryNameTokens (first : second : rest)
  | map toLower first == "hampton"
  , map toLower second == "court" =
      (first ++ "-" ++ second) : rewriteLiteraryNameTokens rest
rewriteLiteraryNameTokens (article : name : rest)
  | map toLower article == "the"
  , map toLower name == "tuileries" =
      name : rewriteLiteraryNameTokens rest
rewriteLiteraryNameTokens (token : rest)
  | map toLower token == "royal-mast" =
      "royal" : "mast" : rewriteLiteraryNameTokens rest
rewriteLiteraryNameTokens (token : rest) =
  token : rewriteLiteraryNameTokens rest

finalizeParseInput ∷ Morpho → String → (NormalizedInput, String)
finalizeParseInput morpho rewrittenInput =
  let normalized = normalizePossessives rewrittenInput
      parseInput
        | possessiveMarkerCount normalized > 0 = normalizedInput normalized
        | otherwise = normalizeSentenceInitialTokenCase morpho (normalizedInput normalized)
  in (normalized, parseInput)

primaryNormalizedInput ∷ String → [(NormalizedInput, String)] → NormalizedInput
primaryNormalizedInput input preparedInputs =
  case preparedInputs of
    (normalized, _) : _ -> normalized
    []                -> NormalizedInput input 0

uniqueStrings ∷ [String] → [String]
uniqueStrings = foldr addUnique []
  where
    addUnique value acc
      | value `elem` acc = acc
      | otherwise        = value : acc

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

tokenizeInputVariants ∷ String → [String]
tokenizeInputVariants input =
  uniqueStrings
    [ tokenizeInput True input
    , tokenizeInput False input
    ]

tokenizeInput ∷ Bool → String → String
tokenizeInput preserveCommas input =
  let roughTokens = words (concatMap (normalizeTokenChars preserveCommas) input)
      cleanedTokens = map cleanToken roughTokens
  in unwords (filter (not . null) cleanedTokens)

normalizeTokenChars ∷ Bool → Char → String
normalizeTokenChars preserveCommas c
  | c == '’' = "'"
  | preserveCommas && c == ',' = " , "
  | preserveCommas && c == ';' = " ; "
  | isAlphaNum c || c == '\'' || c == '-' = [c]
  | otherwise = " "

directLeadNPSentence ∷ GrammarBundle → String → Maybe Sentence
directLeadNPSentence bundle input = do
  (leadText, uttText) <- splitSingleLeadSemicolon input
  lead <- parseLeadNPSurface (controlledMorpho bundle) leadText
  sentence <- parsePreferredControlledSentence bundle uttText
  pure (SentenceWithLeadNP lead sentence)

directLeadNPUttResult ∷ GrammarBundle → String → Maybe (ParsedParse Expr)
directLeadNPUttResult bundle input = do
  sentence <- directLeadNPSentence bundle input
  expr <- exprFromSupportedSentence sentence
  pure (makeParsedParse sentence expr)

splitSingleLeadSemicolon ∷ String → Maybe (String, String)
splitSingleLeadSemicolon input =
  case break (== ';') input of
    (left, ';' : right)
      | ';' `notElem` right
      , not (null (trimWhitespace left))
      , not (null (trimWhitespace right)) ->
          Just (trimWhitespace left, trimWhitespace right)
    _ ->
      Nothing

trimWhitespace ∷ String → String
trimWhitespace =
  reverse . dropWhile isSpace . reverse . dropWhile isSpace

parseLeadNPSurface ∷ Morpho → String → Maybe NounPhrase
parseLeadNPSurface morpho input = do
  let normalized = normalizeKnownTokenCase morpho (normalizeLeadSurface input)
      tokens = words normalized
      (coreTokens, rel) =
        case break (== "in") tokens of
          (before, "in" : after) ->
            (before, Just (RelPrep "in" <$> parseLeadList after))
          _ ->
            (tokens, Nothing)
  (detToken, contentTokens) <- splitLeadDeterminer coreTokens
  let (nounTokens, postAdj) =
        case rel of
          Just _ | length contentTokens >= 2 ->
            (init contentTokens, Just (last contentTokens))
          _ ->
            (contentTokens, Nothing)
  headNoun <- lastMaybe nounTokens
  let adjectives = map stripTrailingComma (init nounTokens)
      relClause =
        case (postAdj, rel) of
          (Just adj, Just (Just prepRel)) ->
            Just (RelChain (PostAdj (BareAdj (stripTrailingComma adj))) prepRel)
          (Just adj, _) ->
            Just (PostAdj (BareAdj (stripTrailingComma adj)))
          (_, Just prepRel) ->
            prepRel
          _ ->
            Nothing
  pure
    (CommonNoun
      (Just detToken)
      adjectives
      (stripTrailingComma headNoun)
      Singular
      relClause)

splitLeadDeterminer ∷ [String] → Maybe (String, [String])
splitLeadDeterminer (detToken : rest)
  | detToken `elem` ["the", "a", "an", "this", "that", "these", "those"]
  , not (null rest) =
      Just (detToken, rest)
splitLeadDeterminer _ =
  Nothing

parseLeadList ∷ [String] → Maybe NounPhrase
parseLeadList tokens =
  coordList (map mkNoun cleaned)
  where
    cleaned =
      [ stripTrailingComma token
      | token <- tokens
      , token /= "and"
      , not (null (stripTrailingComma token))
      ]
    mkNoun nounToken =
      CommonNoun Nothing [] nounToken Singular Nothing

coordList ∷ [NounPhrase] → Maybe NounPhrase
coordList [] = Nothing
coordList [np] = Just np
coordList (np : rest) =
  CoordNP And np <$> coordList rest

normalizeLeadSurface ∷ String → String
normalizeLeadSurface =
  trimWhitespace . unwords . words . map normalizeLeadChar

normalizeLeadChar ∷ Char → Char
normalizeLeadChar c
  | c == '’' = '\''
  | c == '—' || c == '–' = ' '
  | otherwise = c

stripTrailingComma ∷ String → String
stripTrailingComma = reverse . dropWhile (== ',') . reverse

lastMaybe ∷ [a] → Maybe a
lastMaybe [] = Nothing
lastMaybe [item] = Just item
lastMaybe (_ : rest) = lastMaybe rest

exprFromSupportedSentence ∷ Sentence → Maybe Expr
exprFromSupportedSentence (SentenceWithLeadNP np sentence) =
  mkExpr "LeadNPUtt"
    [ exprFromSupportedNP np
    , mkExpr "UttS" [exprFromSupportedSentence sentence]
    ]
exprFromSupportedSentence (Sentence tense polarity subject verb) =
  mkExpr "MkS"
    [ exprFromTense tense
    , exprFromPolarity polarity
    , mkExpr "Pred"
        [ exprFromSupportedNP subject
        , exprFromSupportedVP verb
        ]
    ]
exprFromSupportedSentence _ =
  Nothing

exprFromSupportedNP ∷ NounPhrase → Maybe Expr
exprFromSupportedNP (CommonNoun detToken adjs nounStem Singular relClause) = do
  nounExpr <- exprFromCommonNoun adjs nounStem relClause
  case detToken of
    Just detExprName ->
      mkExpr "DetCN" [exprFromDet detExprName, pure nounExpr]
    Nothing ->
      mkExpr "UseN" [pure nounExpr]
exprFromSupportedNP (Pronoun person number pronounCase) =
  mkExpr "UsePron" [exprFromPronoun person number pronounCase]
exprFromSupportedNP (CoordNP And left right) =
  mkExpr "ConjNP"
    [ pure (mkLeaf "and_Conj")
    , exprFromSupportedNP left
    , exprFromSupportedNP right
    ]
exprFromSupportedNP _ =
  Nothing

exprFromCommonNoun ∷ [String] → String → Maybe RelClause → Maybe Expr
exprFromCommonNoun adjs nounStem relClause = do
  base <- pure (mkLeaf (encodeLexemeStem nounStem <> "_N"))
  prenominal <- foldr
    (\adjStem acc -> mkExpr "AdjCN" [pure (mkLeaf (encodeLexemeStem adjStem <> "_A")), acc])
    (Just base)
    adjs
  case relClause of
    Nothing ->
      pure prenominal
    Just rel' ->
      applyRelClause prenominal rel'

applyRelClause ∷ Expr → RelClause → Maybe Expr
applyRelClause nounExpr (PostAdj (BareAdj adjStem)) =
  mkExpr "AdjPCN"
    [ mkExpr "PostPositA" [pure (mkLeaf (encodeLexemeStem adjStem <> "_A"))]
    , pure nounExpr
    ]
applyRelClause nounExpr (RelPrep prepStem np) =
  mkExpr "PrepCN"
    [ pure nounExpr
    , pure (mkLeaf (encodeLexemeStem prepStem <> "_Prep"))
    , exprFromSupportedNP np
    ]
applyRelClause nounExpr (RelChain left right) = do
  leftExpr <- applyRelClause nounExpr left
  applyRelClause leftExpr right
applyRelClause _ _ =
  Nothing

exprFromSupportedVP ∷ VerbPhrase → Maybe Expr
exprFromSupportedVP (Transitive lemma obj) =
  mkExpr "UseV2"
    [ pure (mkLeaf (encodeLexemeStem lemma <> "_V2"))
    , exprFromSupportedNP obj
    ]
exprFromSupportedVP (Intransitive lemma) =
  mkExpr "UseV" [pure (mkLeaf (encodeLexemeStem lemma <> "_V"))]
exprFromSupportedVP (VPWithAdv verb adv) =
  mkExpr "AdvVP"
    [ exprFromSupportedVP verb
    , exprFromSupportedAdv adv
    ]
exprFromSupportedVP _ =
  Nothing

exprFromSupportedAdv ∷ AdvPhrase → Maybe Expr
exprFromSupportedAdv (LexicalAdv stem) =
  pure (mkLeaf (encodeLexemeStem stem <> "_Adv"))
exprFromSupportedAdv _ =
  Nothing

exprFromTense ∷ Tense → Maybe Expr
exprFromTense Present = pure (mkLeaf "TPres")
exprFromTense Past = pure (mkLeaf "TPast")
exprFromTense Future = pure (mkLeaf "TFut")
exprFromTense Conditional = pure (mkLeaf "TCond")
exprFromTense Perfect = pure (mkLeaf "TPerf")

exprFromPolarity ∷ Polarity → Maybe Expr
exprFromPolarity Positive = pure (mkLeaf "PPos")
exprFromPolarity Negative = pure (mkLeaf "PNeg")

exprFromDet ∷ String → Maybe Expr
exprFromDet "the" = pure (mkLeaf "the_Det")
exprFromDet "a" = pure (mkLeaf "a_Det")
exprFromDet "this" = pure (mkLeaf "this_Det")
exprFromDet "that" = pure (mkLeaf "that_Det")
exprFromDet "these" = pure (mkLeaf "these_Det")
exprFromDet "those" = pure (mkLeaf "those_Det")
exprFromDet _ = Nothing

exprFromPronoun ∷ Person → Number → PronounCase → Maybe Expr
exprFromPronoun First Singular _ = pure (mkLeaf "i_Pron")
exprFromPronoun First Plural _ = pure (mkLeaf "we_Pron")
exprFromPronoun Second Singular Objective = pure (mkLeaf "thee_Pron")
exprFromPronoun Second Singular _ = pure (mkLeaf "you_Pron")
exprFromPronoun Second Plural _ = pure (mkLeaf "youPl_Pron")
exprFromPronoun Third Singular Subjective = pure (mkLeaf "he_Pron")
exprFromPronoun Third Singular Objective = pure (mkLeaf "he_Pron")
exprFromPronoun Third Plural Subjective = pure (mkLeaf "they_Pron")
exprFromPronoun Third Plural Objective = pure (mkLeaf "they_Pron")

mkExpr ∷ String → [Maybe Expr] → Maybe Expr
mkExpr funName args =
  mkApp (mkCId funName) <$> sequence args

mkLeaf ∷ String → Expr
mkLeaf funName = mkApp (mkCId funName) []

encodeLexemeStem ∷ String → String
encodeLexemeStem =
  concatMap encodeChar
  where
    encodeChar '-' = "_H_"
    encodeChar '\'' = "_A_"
    encodeChar c = [toLower c]

normalizeSerialCommaLists ∷ String → String
normalizeSerialCommaLists =
  unwords . rewriteSerialCommaTokens . words . concatMap normalizeSerialCommaChar

normalizeSerialCommaChar ∷ Char → String
normalizeSerialCommaChar c
  | c == ',' = " , "
  | c == ';' = " ; "
  | otherwise = [c]

rewriteSerialCommaTokens ∷ [String] → [String]
rewriteSerialCommaTokens (a : "," : b : "," : c : "," : "and" : d : rest)
  | all isSerialListItem [a, b, c, d] =
      a : "and" : b : "and" : c : "and" : d : rewriteSerialCommaTokens rest
rewriteSerialCommaTokens (a : "," : b : "," : "and" : c : rest)
  | all isSerialListItem [a, b, c] =
      a : "and" : b : "and" : c : rewriteSerialCommaTokens rest
rewriteSerialCommaTokens (token : rest) =
  token : rewriteSerialCommaTokens rest
rewriteSerialCommaTokens [] = []

isSerialListItem ∷ String → Bool
isSerialListItem token =
  not (null token) && all isSerialTokenChar token

isSerialTokenChar ∷ Char → Bool
isSerialTokenChar c =
  isAlphaNum c || c == '\'' || c == '-'

normalizeKnownTokenCase ∷ Morpho → String → String
normalizeKnownTokenCase morpho =
  unwords . map normalizeToken . words
  where
    normalizeToken token
      | not (any isUpper token) = token
      | otherwise =
          let lowered = map toLower token
          in if null (lookupMorpho morpho lowered) then token else lowered

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

normalizeComparativeCorrelatives ∷ String → String
normalizeComparativeCorrelatives input =
  unwords (rewriteComparativeCorrelativeTokens (words input))

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

rewriteComparativeCorrelativeTokens ∷ [String] → [String]
rewriteComparativeCorrelativeTokens [] = []
rewriteComparativeCorrelativeTokens ("by" : "how" : "much" : "the" : "more" : "pains" : "ye" : takeTok : rest)
  | map toLower takeTok == "take" =
      ["by", "how", "much", "ye", takeTok, "the", "more", "pains"]
        ++ rewriteComparativeCorrelativeTokens rest
rewriteComparativeCorrelativeTokens ("For" : "by" : "how" : "much" : rest) =
      rewriteComparativeCorrelativeTokens ("by" : "how" : "much" : rest)
rewriteComparativeCorrelativeTokens ("for" : "by" : "how" : "much" : rest) =
      rewriteComparativeCorrelativeTokens ("by" : "how" : "much" : rest)
rewriteComparativeCorrelativeTokens ("by" : "so" : "much" : "the" : "more" : shallTok : "ye" : rest)
  | map toLower shallTok == "shall" =
      ["by", "so", "much", "the", "more", "ye", shallTok]
        ++ rewriteComparativeCorrelativeTokens rest
rewriteComparativeCorrelativeTokens (token : rest) =
  token : rewriteComparativeCorrelativeTokens rest

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

normalizeSentenceInitialBut ∷ String → String
normalizeSentenceInitialBut input =
  case words input of
    token : rest
      | map toLower token == "but" ->
          unwords rest
    _ -> input

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

preferredPossessiveCount ∷ NormalizedInput → Maybe Int
preferredPossessiveCount normalized
  | possessiveMarkerCount normalized > 0 =
      Just (possessiveMarkerCount normalized)
  | otherwise =
      Nothing

emptyPreferredSelection ∷ PreferredSelection a
emptyPreferredSelection =
  PreferredSelection Nothing Nothing

selectPreferredPreparedInputParse
  ∷ Maybe Int
  → [(NormalizedInput, String)]
  → (String → [[Expr]])
  → (Expr → Maybe (ParsedParse a))
  → Maybe (ParsedParse a)
selectPreferredPreparedInputParse desiredPossessives preparedInputs parseBatches toParsed =
  finalizePreferredSelection desiredPossessives (consumePreparedInputs emptyPreferredSelection preparedInputs)
  where
    consumePreparedInputs selection [] = selection
    consumePreparedInputs selection ((_, text) : rest) =
      consumePreparedInputs (consumeParseBatchGroups selection (parseBatches text)) rest

    consumeParseBatchGroups selection [] = selection
    consumeParseBatchGroups selection (exprs : rest) =
      consumeParseBatchGroups (consumeParseBatch selection exprs) rest

    consumeParseBatch selection [] = selection
    consumeParseBatch selection (expr : rest) =
      let nextSelection =
            case toParsed expr of
              Just parsed -> recordPreferredParse desiredPossessives parsed selection
              Nothing -> selection
      in consumeParseBatch nextSelection rest

recordPreferredParse ∷ Maybe Int → ParsedParse a → PreferredSelection a → PreferredSelection a
recordPreferredParse desiredPossessives parsed selection =
  selection
    { preferredAnyParse =
        chooseBetterParse parsed (preferredAnyParse selection)
    , preferredPossessiveParse =
        case desiredPossessives of
          Just wanted
            | metricPossessiveCount (parsedMetrics parsed) == wanted ->
                chooseBetterParse parsed (preferredPossessiveParse selection)
          _ ->
            preferredPossessiveParse selection
    }

chooseBetterParse ∷ ParsedParse a → Maybe (ParsedParse a) → Maybe (ParsedParse a)
chooseBetterParse candidate Nothing =
  Just candidate
chooseBetterParse candidate current@(Just best)
  | metricsPenaltyTuple (parsedMetrics candidate) < metricsPenaltyTuple (parsedMetrics best) =
      Just candidate
  | otherwise =
      current

finalizePreferredSelection ∷ Maybe Int → PreferredSelection a → Maybe (ParsedParse a)
finalizePreferredSelection desiredPossessives selection =
  case desiredPossessives of
    Just _ ->
      case preferredPossessiveParse selection of
        Just parsed -> Just parsed
        Nothing -> preferredAnyParse selection
    Nothing ->
      preferredAnyParse selection

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
sentenceMetrics (SingleWord _) =
  emptySentenceMetrics
sentenceMetrics (SentenceWithLeadNP np sentence) =
  nounPhraseMetrics np `plusSentenceMetrics` sentenceMetrics sentence
sentenceMetrics (SentenceWithAdv sentence adv) =
  sentenceMetrics sentence `plusSentenceMetrics` advPhraseMetrics adv
sentenceMetrics (Vocative sentence np) =
  sentenceMetrics sentence `plusSentenceMetrics` nounPhraseMetrics np
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
nounPhraseMetrics (Demonstrative _ _) = emptySentenceMetrics
nounPhraseMetrics (AppositiveNP headNP appositiveNP) =
  nounPhraseMetrics headNP `plusSentenceMetrics` nounPhraseMetrics appositiveNP
nounPhraseMetrics (Pronoun _ _ _) = emptySentenceMetrics
nounPhraseMetrics (CommonNoun detTxt adjs noun number relClause) =
  maybe emptySentenceMetrics relClauseMetrics relClause
    `plusSentenceMetrics`
      sentenceMetricsDelta
        0
        0
        (functionWordPenalty noun)
        (comparativeRelAttachmentPenalty adjs relClause)
        barePenalty
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
verbPhraseMetrics (PassiveVSComplement verb sentence) =
  embeddedSentenceMetrics sentence
    `plusSentenceMetrics` sentenceMetricsDelta 0 0 (functionWordPenalty verb) 0 0
verbPhraseMetrics (Copula adj) =
  adjPhraseMetrics adj
verbPhraseMetrics (CopulaNP np) =
  nounPhraseMetrics np
verbPhraseMetrics (CopulaAdv adv) =
  advPhraseMetrics adv
verbPhraseMetrics (SeemAdj adj) =
  adjPhraseMetrics adj
    `plusSentenceMetrics` sentenceMetricsDelta 0 0 (functionWordPenalty "seem") 0 0
verbPhraseMetrics (SeemNP np) =
  nounPhraseMetrics np
    `plusSentenceMetrics` sentenceMetricsDelta 0 0 (functionWordPenalty "seem") 0 0
verbPhraseMetrics (SeemAdv adv) =
  advPhraseMetrics adv
    `plusSentenceMetrics` sentenceMetricsDelta 0 0 (functionWordPenalty "seem") 0 0
verbPhraseMetrics (FeelAdj adj) =
  adjPhraseMetrics adj
    `plusSentenceMetrics` sentenceMetricsDelta 0 0 (functionWordPenalty "feel") 0 0
verbPhraseMetrics (GrowAdj adj) =
  adjPhraseMetrics adj
    `plusSentenceMetrics` sentenceMetricsDelta 0 0 (functionWordPenalty "grow") 0 0
verbPhraseMetrics (GoAdj adj) =
  adjPhraseMetrics adj
    `plusSentenceMetrics` sentenceMetricsDelta 0 0 (functionWordPenalty "go") 0 0
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

adjPhraseMetrics ∷ AdjPhrase → SentenceMetrics
adjPhraseMetrics (BareAdj adj) =
  sentenceMetricsDelta 0 0 (functionWordPenalty adj) 0 0
adjPhraseMetrics (ModifiedAdj modifier adj) =
  adjPhraseMetrics adj
    `plusSentenceMetrics` sentenceMetricsDelta 0 0 (functionWordPenalty modifier) 0 0
adjPhraseMetrics (AdjWithAdv adj adv) =
  adjPhraseMetrics adj `plusSentenceMetrics` advPhraseMetrics adv
adjPhraseMetrics (CoordAdj _ a b) =
  adjPhraseMetrics a `plusSentenceMetrics` adjPhraseMetrics b

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
advPhraseMetrics (CoordAdv _ left right) =
  advPhraseMetrics left `plusSentenceMetrics` advPhraseMetrics right

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
relClauseMetrics (RelWhoseBe noun np) =
  nounPhraseMetrics np
    `plusSentenceMetrics` sentenceMetricsDelta 0 0 (functionWordPenalty "whose" + functionWordPenalty noun + functionWordPenalty "am") 0 0
relClauseMetrics (RelPrep _ np) =
  nounPhraseMetrics np
relClauseMetrics (RelPrepSentence _ sentence) =
  embeddedSentenceMetrics sentence
relClauseMetrics (PostAdj adj) =
  adjPhraseMetrics adj
relClauseMetrics (RelChain left right) =
  relClauseMetrics left `plusSentenceMetrics` relClauseMetrics right

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

comparativeRelAttachmentPenalty ∷ [String] → Maybe RelClause → Int
comparativeRelAttachmentPenalty adjs (Just (RelVP (Intransitive _)))
  | any (isAmbiguousComparativeModifier . map toLower) adjs = 3
comparativeRelAttachmentPenalty _ _ = 0

isAmbiguousComparativeModifier ∷ String → Bool
isAmbiguousComparativeModifier token =
  token `elem` ["better", "worse", "faster", "slower", "harder", "sooner", "later"]

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
