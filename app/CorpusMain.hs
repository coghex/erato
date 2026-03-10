{-# LANGUAGE Strict, UnicodeSyntax #-}

module Main where

import Data.Char (isAlpha, isAscii, isLower, isSpace, isUpper, toLower)
import Data.List (isInfixOf, isPrefixOf, nub, sort)
import Data.Maybe (mapMaybe)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>), takeExtension)

import Parser.GFParser (GrammarBundle, loadGrammars, parsePreferredControlledSentence, parsePreferredFallbackSentence)

data CorpusConfig = CorpusConfig
  { cfgFiles             ∷ [FilePath]
  , cfgDirs              ∷ [FilePath]
  , cfgShowFailures      ∷ Int
  , cfgMaxSentences      ∷ Maybe Int
  , cfgStopAfterUnparsed ∷ Maybe Int
  , cfgMinControlledRate ∷ Maybe Double
  , cfgMinTotalRate      ∷ Maybe Double
  }

data Command
  = ShowHelp
  | RunCorpus CorpusConfig

data CorpusSentence = CorpusSentence
  { sentenceFile ∷ FilePath
  , sentenceLine ∷ Int
  , sentenceText ∷ String
  }

data ParseOutcome
  = ControlledParsed
  | FallbackParsed
  | AutoPassSingleWord
  | AutoPassNameFragment
  | AutoPassGlossFragment
  | DroppedJunk
  | Unparsed

data CorpusStats = CorpusStats
  { statTotal       ∷ Int
  , statControlled  ∷ Int
  , statFallback    ∷ Int
  , statAutoPass    ∷ Int
  , statUnparsed    ∷ Int
  , statShowLimit   ∷ Int
  , statFailures    ∷ [CorpusSentence]
  }

defaultConfig ∷ CorpusConfig
defaultConfig =
  CorpusConfig
    { cfgFiles = []
    , cfgDirs = []
    , cfgShowFailures = 20
    , cfgMaxSentences = Nothing
    , cfgStopAfterUnparsed = Nothing
    , cfgMinControlledRate = Nothing
    , cfgMinTotalRate = Nothing
    }

emptyStats ∷ Int → CorpusStats
emptyStats n =
  CorpusStats
    { statTotal = 0
    , statControlled = 0
    , statFallback = 0
    , statAutoPass = 0
    , statUnparsed = 0
    , statShowLimit = n
    , statFailures = []
    }

main ∷ IO ()
main = do
  args <- getArgs
  case parseCommand args of
    Left err -> do
      putStrLn ("[error] " <> err)
      putStrLn usage
      exitFailure
    Right ShowHelp -> do
      putStrLn usage
      exitSuccess
    Right (RunCorpus cfg) -> runCorpus cfg

runCorpus ∷ CorpusConfig → IO ()
runCorpus cfg = do
  files <- resolveInputs cfg
  if null files
    then do
      putStrLn "[error] no .txt files found. Provide --file and/or --dir inputs."
      exitFailure
    else pure ()

  grammars <- loadGrammars "Grammar/EratoAbs.pgf" "Grammar/AllEngAbs.pgf"
  corpusSentences <- concat <$> mapM loadCorpusFile files
  if null corpusSentences
    then do
      putStrLn "[error] no sentence-like content found in provided files."
      exitFailure
    else pure ()

  let candidateSentences = applyMaxSentences (cfgMaxSentences cfg) corpusSentences
      stats = processSentences cfg grammars (emptyStats (cfgShowFailures cfg)) candidateSentences
      parsedCount = statControlled stats + statFallback stats + statAutoPass stats
      maxHit = maybe False (\n → length corpusSentences > n) (cfgMaxSentences cfg)
      stopHit = maybe False (\n → statUnparsed stats >= n && statTotal stats < length candidateSentences) (cfgStopAfterUnparsed cfg)

  putStrLn ("Corpus files: " <> show (length files))
  putStrLn ("Sentences:    " <> show (statTotal stats))
  if maxHit
    then putStrLn ("[info] stopped at --max-sentences limit (" <> show (unwrapMaybeInt (cfgMaxSentences cfg)) <> ").")
    else pure ()
  if stopHit
    then putStrLn ("[info] stopped after reaching --stop-after-unparsed limit (" <> show (unwrapMaybeInt (cfgStopAfterUnparsed cfg)) <> ").")
    else pure ()
  putStrLn ("Controlled:   " <> show (statControlled stats) <> " (" <> renderRate (rate (statControlled stats) (statTotal stats)) <> ")")
  putStrLn ("Fallback:     " <> show (statFallback stats) <> " (" <> renderRate (rate (statFallback stats) (statTotal stats)) <> ")")
  putStrLn ("Auto-pass:    " <> show (statAutoPass stats) <> " (" <> renderRate (rate (statAutoPass stats) (statTotal stats)) <> ")")
  putStrLn ("Unparsed:     " <> show (statUnparsed stats) <> " (" <> renderRate (rate (statUnparsed stats) (statTotal stats)) <> ")")
  putStrLn ("Total parsed: " <> show parsedCount <> " (" <> renderRate (rate parsedCount (statTotal stats)) <> ")")

  printFailureExamples stats
  enforceThresholds cfg stats

resolveInputs ∷ CorpusConfig → IO [FilePath]
resolveInputs cfg = do
  ensureFilesExist (cfgFiles cfg)
  ensureDirsExist (cfgDirs cfg)
  fromDirs <- concat <$> mapM collectTxtFilesRecursive (cfgDirs cfg)
  pure (sort (nub (cfgFiles cfg ++ fromDirs)))

ensureFilesExist ∷ [FilePath] → IO ()
ensureFilesExist files = do
  status <- mapM doesFileExist files
  let missing = [path | (path, ok) <- zip files status, not ok]
  if null missing
    then pure ()
    else do
      putStrLn ("[error] missing file(s): " <> unwords missing)
      exitFailure

ensureDirsExist ∷ [FilePath] → IO ()
ensureDirsExist dirs = do
  status <- mapM doesDirectoryExist dirs
  let missing = [path | (path, ok) <- zip dirs status, not ok]
  if null missing
    then pure ()
    else do
      putStrLn ("[error] missing directory/directories: " <> unwords missing)
      exitFailure

collectTxtFilesRecursive ∷ FilePath → IO [FilePath]
collectTxtFilesRecursive dir = do
  entries <- listDirectory dir
  paths <- mapM descend (map (dir </>) entries)
  pure (concat paths)
  where
    descend path = do
      isDir <- doesDirectoryExist path
      if isDir
        then collectTxtFilesRecursive path
        else do
          isFile <- doesFileExist path
          let ext = map toLower (takeExtension path)
          pure (if isFile && ext == ".txt" then [path] else [])

loadCorpusFile ∷ FilePath → IO [CorpusSentence]
loadCorpusFile path = do
  content <- readFile path
  pure
    [ CorpusSentence path lineNo sentence
    | (lineNo, sentence) <- splitTextIntoSentencesWithLine content
    ]

splitTextIntoSentencesWithLine ∷ String → [(Int, String)]
splitTextIntoSentencesWithLine input =
  reverse (finish current startLine acc)
  where
    protectedInput = protectAbbreviationPeriods input
    (current, startLine, _, acc) = foldl step ([], Nothing, 1, []) protectedInput

    step (chars, mStart, lineNo, results) ch
      | ch == '\n' =
          (appendSpace chars, mStart, lineNo + 1, results)
      | isSentenceDelimiter ch =
          case mStart of
            Just start ->
              let nextResults = appendSentence start chars results
              in ([], Nothing, lineNo, nextResults)
            Nothing ->
              ([], Nothing, lineNo, results)
      | isSpace ch =
          (appendSpace chars, mStart, lineNo, results)
      | otherwise =
          (ch : chars, keepStartLine mStart lineNo, lineNo, results)

    finish chars mStart results =
      case mStart of
        Just start -> appendSentence start chars results
        Nothing -> results

    appendSentence start chars results =
      let sentence = cleanSentence (restoreProtectedPeriods (reverse chars))
      in if isSentenceCandidate sentence
           then (start, sentence) : results
           else results

isSentenceDelimiter ∷ Char → Bool
isSentenceDelimiter c =
  c `elem` (".!?" ∷ String)

abbreviationPeriodMarker ∷ Char
abbreviationPeriodMarker = '\x1E'

protectAbbreviationPeriods ∷ String → String
protectAbbreviationPeriods = go []
  where
    go _ [] = []
    go prefix ('.' : rest)
      | shouldProtectAbbreviationPeriod prefix rest =
          abbreviationPeriodMarker : go (abbreviationPeriodMarker : prefix) rest
      | otherwise =
          '.' : go ('.' : prefix) rest
    go prefix (ch : rest) =
      ch : go (ch : prefix) rest

restoreProtectedPeriods ∷ String → String
restoreProtectedPeriods =
  map (\c → if c == abbreviationPeriodMarker then '.' else c)

shouldProtectAbbreviationPeriod ∷ String → String → Bool
shouldProtectAbbreviationPeriod prefix rest =
  case abbreviationTokenBeforePeriod prefix of
    Just token ->
      isLikelyAbbreviationToken token
        && ( nextWordStartsLowercase rest
              || continuesAbbreviationChain rest
              || (nextWordStartsUppercase rest && (isCommonTitleAbbreviation token || hasCommaBeforeBoundary rest))
           )
    Nothing -> False

abbreviationTokenBeforePeriod ∷ String → Maybe String
abbreviationTokenBeforePeriod prefix =
  let tokenRev = takeWhile isAlpha prefix
  in if null tokenRev
       then Nothing
       else Just (reverse tokenRev)

isLikelyAbbreviationToken ∷ String → Bool
isLikelyAbbreviationToken token =
  length token <= 3
    && all isAlpha token
    && any isUpper token

nextWordStartsLowercase ∷ String → Bool
nextWordStartsLowercase rest =
  case firstSignificantCharAfterPeriod rest of
    Just c -> isLower c
    Nothing -> False

nextWordStartsUppercase ∷ String → Bool
nextWordStartsUppercase rest =
  case firstSignificantCharAfterPeriod rest of
    Just c -> isUpper c
    Nothing -> False

continuesAbbreviationChain ∷ String → Bool
continuesAbbreviationChain rest =
  case dropWhile isPeriodFollowerNoise rest of
    c : '.' : _ -> isUpper c
    _ -> False

isCommonTitleAbbreviation ∷ String → Bool
isCommonTitleAbbreviation token =
  map toLower token `elem`
    [ "mr", "mrs", "ms", "dr", "st", "sr", "jr"
    , "prof", "rev", "gen", "col", "lt"
    ]

hasCommaBeforeBoundary ∷ String → Bool
hasCommaBeforeBoundary rest =
  ',' `elem` takeWhile (\c -> not (c `elem` (".!?" ∷ String))) rest

firstSignificantCharAfterPeriod ∷ String → Maybe Char
firstSignificantCharAfterPeriod [] = Nothing
firstSignificantCharAfterPeriod (c : cs)
  | isPeriodFollowerNoise c = firstSignificantCharAfterPeriod cs
  | otherwise = Just c

isPeriodFollowerNoise ∷ Char → Bool
isPeriodFollowerNoise c =
  isSpace c || c `elem` ("\"'()[]{}_*-–—―" ∷ String)

appendSpace ∷ String → String
appendSpace [] = []
appendSpace (' ' : rest) = ' ' : rest
appendSpace chars = ' ' : chars

keepStartLine ∷ Maybe Int → Int → Maybe Int
keepStartLine (Just existing) _ = Just existing
keepStartLine Nothing lineNo = Just lineNo

cleanSentence ∷ String → String
cleanSentence = trim . normalizeCorpusNoise . stripEdgeNoise

normalizeCorpusNoise ∷ String → String
normalizeCorpusNoise =
  unwords
    . filter (not . null)
    . filter (not . isAsteriskWord)
    . filter (not . isUnderscoreMarkupWord)
    . words
    . map normalizeNoiseChar

normalizeNoiseChar ∷ Char → Char
normalizeNoiseChar c
  | isDashChar c = ' '
  | otherwise = c

isDashChar ∷ Char → Bool
isDashChar c = c `elem` ("-–—―" ∷ String)

isUnderscoreMarkupWord ∷ String → Bool
isUnderscoreMarkupWord token =
  let core = stripTokenEdgeNoise token
  in not (null core) && '_' `elem` core

isAsteriskWord ∷ String → Bool
isAsteriskWord token =
  case stripTokenEdgeNoise token of
    [] -> False
    core -> all (== '*') core

stripTokenEdgeNoise ∷ String → String
stripTokenEdgeNoise =
  reverse . dropWhile isTokenEdgeNoise . reverse . dropWhile isTokenEdgeNoise

isTokenEdgeNoise ∷ Char → Bool
isTokenEdgeNoise c =
  isTokenPrefixNoise c || c `elem` (",.;:!?" ∷ String)

isTokenPrefixNoise ∷ Char → Bool
isTokenPrefixNoise c = c `elem` ("\"'([{`" ∷ String)

stripEdgeNoise ∷ String → String
stripEdgeNoise =
  reverse . dropWhile isEdgeChar . reverse . dropWhile isEdgeChar
  where
    isEdgeChar c = isSpace c || c `elem` ("\"'()[]{}," ∷ String)

trim ∷ String → String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

isSentenceCandidate ∷ String → Bool
isSentenceCandidate sentence =
  length sentence > 1 && any isAlpha sentence

applyMaxSentences ∷ Maybe Int → [CorpusSentence] → [CorpusSentence]
applyMaxSentences Nothing sentences = sentences
applyMaxSentences (Just n) sentences = take n sentences

processSentences ∷ CorpusConfig → GrammarBundle → CorpusStats → [CorpusSentence] → CorpusStats
processSentences cfg grammars = go
  where
    go stats [] = stats
    go stats (sentence : rest)
      | reachedUnparsedLimit cfg stats = stats
      | otherwise =
          let next = accumulateOutcome grammars stats sentence
          in go next rest

reachedUnparsedLimit ∷ CorpusConfig → CorpusStats → Bool
reachedUnparsedLimit cfg stats =
  case cfgStopAfterUnparsed cfg of
    Just n -> statUnparsed stats >= n
    Nothing -> False

accumulateOutcome ∷ GrammarBundle → CorpusStats → CorpusSentence → CorpusStats
accumulateOutcome grammars stats sentence =
  case classifySentence grammars (sentenceText sentence) of
    ControlledParsed ->
      stats
        { statTotal = statTotal stats + 1
        , statControlled = statControlled stats + 1
        }
    FallbackParsed ->
      stats
        { statTotal = statTotal stats + 1
        , statFallback = statFallback stats + 1
        }
    AutoPassSingleWord ->
      stats
        { statTotal = statTotal stats + 1
        , statAutoPass = statAutoPass stats + 1
        }
    AutoPassNameFragment ->
      stats
        { statTotal = statTotal stats + 1
        , statAutoPass = statAutoPass stats + 1
        }
    AutoPassGlossFragment ->
      stats
        { statTotal = statTotal stats + 1
        , statAutoPass = statAutoPass stats + 1
        }
    DroppedJunk ->
      stats
    Unparsed ->
      stats
        { statTotal = statTotal stats + 1
        , statUnparsed = statUnparsed stats + 1
        , statFailures = appendFailureIfRoom (statShowLimit stats) (statFailures stats) sentence
        }
  where
    classifySentence bundle text
      | isForeignDominantSentence text = DroppedJunk
      | isForeignShortFragmentSentence text = AutoPassNameFragment
      | isSingleWordSentence text = AutoPassSingleWord
      | isNameFragmentSentence text = AutoPassNameFragment
      | isInfinitiveGlossSentence text = AutoPassGlossFragment
      | otherwise =
          case firstParsedOutcome bundle (text : corpusRewriteCandidates text) of
            Just outcome -> outcome
            Nothing -> Unparsed

firstParsedOutcome ∷ GrammarBundle → [String] → Maybe ParseOutcome
firstParsedOutcome _ [] = Nothing
firstParsedOutcome bundle (candidate : rest) =
  case parseOutcomeForText bundle candidate of
    Just outcome -> Just outcome
    Nothing -> firstParsedOutcome bundle rest

parseOutcomeForText ∷ GrammarBundle → String → Maybe ParseOutcome
parseOutcomeForText bundle text
  | Just _ <- parsePreferredControlledSentence bundle text = Just ControlledParsed
  | Just _ <- parsePreferredFallbackSentence bundle text = Just FallbackParsed
  | otherwise = Nothing

corpusRewriteCandidates ∷ String → [String]
corpusRewriteCandidates text =
  filter (/= text) (applyRewriteRounds 3 seedVariants)
  where
    semicolonVariants = semicolonClauseCandidates text
    seedVariants =
      nub
        ( text
            : casingNormalizedCandidate text
           ++ participialBylineCandidate text
           ++ semicolonVariants
           ++ concatMap casingNormalizedCandidate semicolonVariants
        )

applyRewriteRounds ∷ Int → [String] → [String]
applyRewriteRounds rounds variants
  | rounds <= 0 = nub variants
  | otherwise =
      let expanded = nub (variants ++ concatMap directRewriteCandidates variants)
      in applyRewriteRounds (rounds - 1) expanded

directRewriteCandidates ∷ String → [String]
directRewriteCandidates text =
  titleCompoundCandidates text
    ++ properNameSpanCandidates text
    ++ definiteArticleNameDropCandidates text
    ++ lexiconPluralCandidates text
    ++ progressiveEverCandidates text
    ++ optativeWouldThatCandidates text
    ++ clearOutPhrasalCandidates text
    ++ archaicPronounCandidates text
    ++ forEverCandidates text
    ++ comparativeCorrelativeCandidates text
    ++ invertedAuxClauseCandidates text
    ++ goPredicateCandidates text
    ++ restrictiveRelativeTailCandidates text
    ++ imperativeGiveItUpCandidates text
    ++ valedictionImperativeCandidates text
    ++ whoseTailTrimCandidates text
    ++ touchingLeadInCandidates text
    ++ asWellAsLeadInCandidates text
    ++ copulaAdjectiveSimplificationCandidates text
    ++ asAffordingTailCandidates text
    ++ hasBeenParticipleListCandidates text
    ++ evidentialLeadInCandidates text
    ++ discourseLeadInCandidates text
    ++ commaParentheticalCandidates text
    ++ aggressiveClauseCoreCandidates text
    ++ etymologyGlossTailCandidates text
    ++ appositiveOfChainCandidates text
    ++ predicateTailTrimCandidates text
    ++ trailingForPhraseCandidates text
    ++ reduplicativeModifierCandidates text
    ++ archaicVerbCandidates text
    ++ passiveInfinitiveCandidates text
    ++ complexClauseNormalizationCandidates text
    ++ takeInHandCandidates text
    ++ andToInfinitiveCandidates text
    ++ byWhatNameCandidates text
    ++ byNameClauseTrimCandidates text
    ++ freeRelativeCandidates text
    ++ frontedSubordinateCandidates text
    ++ pruneLeavingParentheticalCandidates text
    ++ participialTailTrimCandidates text

complexClauseNormalizationCandidates ∷ String → [String]
complexClauseNormalizationCandidates text =
  concatMap expand [map toLower text]
  where
    expand base =
      let step1 = replaceAllInsensitive "take in hand to " "" base
          step2 = replaceAllInsensitive ", and to " " and " step1
          step3 = collapseByWhatNameCalled step2
          step4 = replaceAllInsensitive "that which" "the thing that" step3
          pruned = step4 : pruneLeavingParentheticalCandidates step4
          fronted = concatMap frontedSubordinateCandidates pruned
      in nub (pruned ++ fronted)

collapseByWhatNameCalled ∷ String → String
collapseByWhatNameCalled text =
  case splitOnSubstring "by what name" text of
    Just (prefix, rest) ->
      case splitOnSubstring "in our tongue" rest of
        Just (_, suffix) -> trim (prefix <> "by name " <> suffix)
        Nothing -> text
    Nothing -> text

casingNormalizedCandidate ∷ String → [String]
casingNormalizedCandidate text
  | any isUpper (drop 1 text) = [map toLower text]
  | otherwise = []

participialBylineCandidate ∷ String → [String]
participialBylineCandidate text =
  case words text of
    firstWord : _
      | looksParticipleLike firstWord
      , " by " `isInfixOf` map toLower (" " <> text <> " ") ->
          [ "it is " <> map toLower text
          , "it is " <> lowercaseFirstWord text
          ]
    _ -> []

lexiconPluralCandidates ∷ String → [String]
lexiconPluralCandidates text =
  if "lexicons" `isInfixOf` map toLower text
    then [replaceAllInsensitive "lexicons" "lexica" text]
    else []

progressiveEverCandidates ∷ String → [String]
progressiveEverCandidates text =
  filter (/= text)
    [ replaceAllInsensitive " was ever " " was " (" " <> text <> " ")
    , replaceAllInsensitive " were ever " " were " (" " <> text <> " ")
    ]
    >>= \candidate -> [trim candidate]

properNameSpanCandidates ∷ String → [String]
properNameSpanCandidates text =
  [candidate | candidate <- [unwords (mergeTitleSpanTokens (words text))], candidate /= text]

mergeTitleSpanTokens ∷ [String] → [String]
mergeTitleSpanTokens = go []
  where
    go acc [] = reverse acc
    go acc [x] = reverse (x : acc)
    go acc (x : y : rest)
      | isTitleWordToken x
      , isTitleWordToken y =
          go acc (mergeTitleTokens x y : rest)
      | otherwise = go (x : acc) (y : rest)

mergeTitleTokens ∷ String → String → String
mergeTitleTokens left right =
  stripTokenEdgeNoise left <> stripTokenEdgeNoise right

definiteArticleNameDropCandidates ∷ String → [String]
definiteArticleNameDropCandidates text =
  [ candidate
  | candidate <- [unwords (dropArticleBeforeTitleTokens (words text))]
  , candidate /= text
  ]

dropArticleBeforeTitleTokens ∷ [String] → [String]
dropArticleBeforeTitleTokens = go
  where
    go ("the" : next : rest)
      | isTitleWordToken next = next : go rest
    go ("The" : next : rest)
      | isTitleWordToken next = next : go rest
    go (x : xs) = x : go xs
    go [] = []

optativeWouldThatCandidates ∷ String → [String]
optativeWouldThatCandidates text =
  case stripLeadingPhraseInsensitive "would that " text of
    Just candidate -> [candidate | candidate /= text, isSentenceCandidate candidate]
    Nothing -> []

clearOutPhrasalCandidates ∷ String → [String]
clearOutPhrasalCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " clear out " " clear " (" " <> text <> " "))
    ]

archaicPronounCandidates ∷ String → [String]
archaicPronounCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " ye " " you " padded)
    , trim (replaceAllInsensitive " thou " " you " padded)
    , trim (replaceAllInsensitive " thee " " you " padded)
    ]
  where
    padded = " " <> text <> " "

forEverCandidates ∷ String → [String]
forEverCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " for ever " " " (" " <> text <> " "))
    ]

comparativeCorrelativeCandidates ∷ String → [String]
comparativeCorrelativeCandidates text =
  case splitOnSubstringInsensitive ", by so much " text of
    Just (_, tailClause) ->
      let dropped = dropLeadingComparativeWords (trim tailClause)
          normalized = normalizeInvertedAuxClause dropped
      in [normalized | isSentenceCandidate normalized]
    Nothing -> []

dropLeadingComparativeWords ∷ String → String
dropLeadingComparativeWords input =
  trim (go (trim input))
  where
    go s
      | "the more " `isPrefixOf` map toLower s = go (drop 9 s)
      | "the less " `isPrefixOf` map toLower s = go (drop 9 s)
      | "more " `isPrefixOf` map toLower s = go (drop 5 s)
      | "less " `isPrefixOf` map toLower s = go (drop 5 s)
      | otherwise = s

invertedAuxClauseCandidates ∷ String → [String]
invertedAuxClauseCandidates text =
  [ normalized
  | normalized <- [normalizeInvertedAuxClause text]
  , normalized /= text
  , isSentenceCandidate normalized
  ]

normalizeInvertedAuxClause ∷ String → String
normalizeInvertedAuxClause text =
  case words (trim text) of
    aux : subj : rest
      | isAuxiliaryToken aux
      , isPronounToken subj ->
          unwords (subj : aux : rest)
    _ -> text

isAuxiliaryToken ∷ String → Bool
isAuxiliaryToken token =
  map toLower token `elem`
    [ "shall", "will", "would", "should", "can", "could", "may", "might"
    , "must", "do", "does", "did", "have", "has", "had", "is", "are", "was", "were"
    ]

isPronounToken ∷ String → Bool
isPronounToken token =
  map toLower token `elem`
    [ "i", "you", "we", "they", "he", "she", "it", "ye", "thou", "thee" ]

goPredicateCandidates ∷ String → [String]
goPredicateCandidates text =
  case splitOnSubstringInsensitive " go " text of
    Just (prefix, suffix)
      | isSingleAdjectiveLikeSuffix suffix ->
          let candidate = trim (prefix <> " be " <> trim suffix)
          in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []

isSingleAdjectiveLikeSuffix ∷ String → Bool
isSingleAdjectiveLikeSuffix suffix =
  case normalizedWordTokens suffix of
    [token] ->
      all isAlpha (map toLower token)
        && length token >= 3
    _ -> False

restrictiveRelativeTailCandidates ∷ String → [String]
restrictiveRelativeTailCandidates text =
  concatMap trimAtMarker markers
  where
    markers = [" which ", " that ", " who "]
    trimAtMarker marker =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, tailText)
          | looksHeavyRelativeTail tailText ->
              let candidate = trim prefix
              in [candidate | isSentenceCandidate candidate]
        _ -> []

looksHeavyRelativeTail ∷ String → Bool
looksHeavyRelativeTail tailText =
  let lower = map toLower tailText
      tokenCount = length (words lower)
  in tokenCount >= 6
      && any (`isInfixOf` lower)
           [ " will "
           , " would "
           , " shall "
           , " should "
           , " by "
           , " has been "
           , " have been "
           , " had been "
           ]

imperativeGiveItUpCandidates ∷ String → [String]
imperativeGiveItUpCandidates text =
  filter (/= text)
    [ replaceAllInsensitive "give it up" "give up" text
    ]

valedictionImperativeCandidates ∷ String → [String]
valedictionImperativeCandidates text =
  nub
    [ candidate
    | base <- baseCandidates
    , candidate <- base : commaPrefixCandidate base
    , candidate /= text
    , isSentenceCandidate candidate
    ]
  where
    lowered = map toLower text
    baseCandidates =
      if "fare thee well" `isInfixOf` lowered
        then
          [ trim (stripLeadingSo (replaceAllInsensitive "so fare thee well" "be well" text))
          , trim (stripLeadingSo (replaceAllInsensitive "fare thee well" "be well" text))
          ]
        else []
    commaPrefixCandidate candidate =
      case splitOnSubstring "," candidate of
        Just (prefix, _) -> [trim prefix]
        Nothing -> []

stripLeadingSo ∷ String → String
stripLeadingSo input =
  let trimmed = trim input
      lower = map toLower trimmed
  in if "so " `isPrefixOf` lower
       then trim (drop 3 trimmed)
       else trimmed

whoseTailTrimCandidates ∷ String → [String]
whoseTailTrimCandidates text =
  case splitOnSubstringInsensitive ", whose " text of
    Just (prefix, _) ->
      let candidate = trim prefix
      in [candidate | candidate /= text, isSentenceCandidate candidate]
    Nothing -> []

touchingLeadInCandidates ∷ String → [String]
touchingLeadInCandidates text =
  case splitOnSubstring ", " text of
    Just (prefix, suffix)
      | looksTouchingLeadIn prefix ->
          let candidate = trim suffix
          in [candidate | isSentenceCandidate candidate]
    _ -> []

looksTouchingLeadIn ∷ String → Bool
looksTouchingLeadIn prefix =
  let lower = map toLower (trim prefix)
  in "as touching " `isPrefixOf` lower
      || "touching " `isPrefixOf` lower
      || "as to " `isPrefixOf` lower

asWellAsLeadInCandidates ∷ String → [String]
asWellAsLeadInCandidates text =
  case splitOnSubstring ", " text of
    Just (prefix, suffix)
      | "as well as " `isPrefixOf` map toLower (trim prefix) ->
          let candidate = trim suffix
          in [candidate | isSentenceCandidate candidate]
    _ -> []

copulaAdjectiveSimplificationCandidates ∷ String → [String]
copulaAdjectiveSimplificationCandidates text =
  nub
    ( dropCopulaDegreeCandidates text
        ++ simplifyCopulaAdjCoordCandidates " are " text
        ++ simplifyCopulaAdjCoordCandidates " is " text
    )

dropCopulaDegreeCandidates ∷ String → [String]
dropCopulaDegreeCandidates text =
  filter (/= text)
    [ replaceAllInsensitive " are solely " " are " (" " <> text <> " ")
    , replaceAllInsensitive " is solely " " is " (" " <> text <> " ")
    , replaceAllInsensitive " are very " " are " (" " <> text <> " ")
    , replaceAllInsensitive " is very " " is " (" " <> text <> " ")
    ]
    >>= \candidate -> [trim candidate]

simplifyCopulaAdjCoordCandidates ∷ String → String → [String]
simplifyCopulaAdjCoordCandidates copulaMarker text =
  case splitOnSubstringInsensitive copulaMarker text of
    Just (prefix, rest) ->
      case splitOnSubstringInsensitive " or " rest of
        Just (leftAdj, rightAdj)
          | looksAdjPhrase leftAdj
          , looksAdjPhrase rightAdj ->
              let candidate = trim (prefix <> copulaMarker <> normalizeAdjHead leftAdj)
              in [candidate | isSentenceCandidate candidate]
        _ ->
          case splitOnSubstringInsensitive " and " rest of
            Just (leftAdj, rightAdj)
              | looksAdjPhrase leftAdj
              , looksAdjPhrase rightAdj ->
                  let candidate = trim (prefix <> copulaMarker <> normalizeAdjHead leftAdj)
                  in [candidate | isSentenceCandidate candidate]
            _ -> []
    _ -> []

normalizeAdjHead ∷ String → String
normalizeAdjHead phrase =
  unwords (dropLeadingDegreeWords (words (trim phrase)))

dropLeadingDegreeWords ∷ [String] → [String]
dropLeadingDegreeWords (w : rest)
  | map toLower (stripTokenEdgeNoise w) `elem` ["solely", "very", "quite", "rather", "fairly"] =
      dropLeadingDegreeWords rest
dropLeadingDegreeWords xs = xs

looksAdjPhrase ∷ String → Bool
looksAdjPhrase phrase =
  let tokens =
        filter (not . null)
          (map (map toLower . stripTokenEdgeNoise) (words phrase))
  in not (null tokens)
      && length tokens <= 3
      && all (\t -> all isAlpha t) tokens

asAffordingTailCandidates ∷ String → [String]
asAffordingTailCandidates text =
  [ candidate
  | marker <- [", as affording ", " as affording "]
  , Just (prefix, _) <- [splitOnSubstringInsensitive marker text]
  , let candidate = trim prefix
  , candidate /= text
  , isSentenceCandidate candidate
  ]

hasBeenParticipleListCandidates ∷ String → [String]
hasBeenParticipleListCandidates text =
  case splitOnSubstringInsensitive " has been " text of
    Just (prefix, rest) ->
      case splitOnSubstringInsensitive " of " rest of
        Just (partList, suffix)
          | "," `isInfixOf` partList
          , " and " `isInfixOf` map toLower partList ->
              let firstPart = trim (takeWhile (/= ',') partList)
                  candidate = trim (prefix <> " has been " <> firstPart <> " of " <> suffix)
              in [candidate | isSentenceCandidate candidate]
        _ -> []
    _ -> []

evidentialLeadInCandidates ∷ String → [String]
evidentialLeadInCandidates text =
  [ candidate
  | prefix <- leadInPrefixes
  , Just candidate <- [stripLeadingPhraseInsensitive prefix text]
  , isSentenceCandidate candidate
  ]
  where
    leadInPrefixes =
      [ "it will be seen that "
      , "it can be seen that "
      , "it may be seen that "
      , "it is seen that "
      , "it will be seen, that "
      , "it can be seen, that "
      , "it may be seen, that "
      ]

stripLeadingPhraseInsensitive ∷ String → String → Maybe String
stripLeadingPhraseInsensitive phrase text =
  let trimmed = trim text
      lowerPhrase = map toLower phrase
      lowerTrimmed = map toLower trimmed
  in if lowerPhrase `isPrefixOf` lowerTrimmed
       then Just (trim (drop (length phrase) trimmed))
       else Nothing

discourseLeadInCandidates ∷ String → [String]
discourseLeadInCandidates text =
  [ candidate
  | marker <- markers
  , Just candidate <- [stripLeadingDiscourseMarker marker text]
  , isSentenceCandidate candidate
  ]
  where
    markers =
      [ "therefore", "thus", "hence", "accordingly"
      , "however", "indeed", "moreover", "nevertheless"
      , "consequently", "then"
      ]

stripLeadingDiscourseMarker ∷ String → String → Maybe String
stripLeadingDiscourseMarker marker text =
  let trimmed = trim text
      lowerTrimmed = map toLower trimmed
      markerWithComma = marker <> ", "
      markerWithSpace = marker <> " "
      dropPrefix prefix = Just (trim (drop (length prefix) trimmed))
  in if markerWithComma `isPrefixOf` lowerTrimmed
       then dropPrefix markerWithComma
       else if markerWithSpace `isPrefixOf` lowerTrimmed
              then dropPrefix markerWithSpace
              else Nothing

commaParentheticalCandidates ∷ String → [String]
commaParentheticalCandidates text =
  case splitOnSubstring ", " text of
    Just (prefix, rest) ->
      case splitOnSubstring ", " rest of
        Just (middle, suffix)
          | looksAdverbialParenthetical middle ->
              let candidate = trim (prefix <> " " <> suffix)
              in [candidate | isSentenceCandidate candidate]
        _ -> []
    Nothing -> []

aggressiveClauseCoreCandidates ∷ String → [String]
aggressiveClauseCoreCandidates text =
  [simplified | simplified /= text, isSentenceCandidate simplified]
  where
    noDiscourse = stripAnyLeadingDiscourseMarker text
    noTouching = stripAnyTouchingLeadIn noDiscourse
    noAsWellAs = stripAnyAsWellAsLeadIn noTouching
    noParenthetical = removeAllAdverbialParentheticals noAsWellAs
    noAffording = stripAnyAsAffordingTail noParenthetical
    noReduplicative = simplifyFirstReduplicativePair noAffording
    noCopulaCoord = simplifySingleCopulaCoord noReduplicative
    simplified = dropTrailingForAdjunct noCopulaCoord

stripAnyLeadingDiscourseMarker ∷ String → String
stripAnyLeadingDiscourseMarker text =
  case mapMaybe (\marker -> stripLeadingDiscourseMarker marker text) markers of
    candidate : _ -> candidate
    [] -> text
  where
    markers =
      [ "therefore", "thus", "hence", "accordingly"
      , "however", "indeed", "moreover", "nevertheless"
      , "consequently", "then"
      ]

removeAllAdverbialParentheticals ∷ String → String
removeAllAdverbialParentheticals text =
  case splitOnSubstring ", " text of
    Just (prefix, rest) ->
      case splitOnSubstring ", " rest of
        Just (middle, suffix)
          | looksAdverbialParenthetical middle ->
              removeAllAdverbialParentheticals (trim (prefix <> " " <> suffix))
        _ -> text
    Nothing -> text

stripAnyTouchingLeadIn ∷ String → String
stripAnyTouchingLeadIn text =
  case touchingLeadInCandidates text of
    candidate : _ -> candidate
    [] -> text

stripAnyAsWellAsLeadIn ∷ String → String
stripAnyAsWellAsLeadIn text =
  case asWellAsLeadInCandidates text of
    candidate : _ -> candidate
    [] -> text

stripAnyAsAffordingTail ∷ String → String
stripAnyAsAffordingTail text =
  case asAffordingTailCandidates text of
    candidate : _ -> candidate
    [] -> text

simplifySingleCopulaCoord ∷ String → String
simplifySingleCopulaCoord text =
  case filter (/= text) (copulaAdjectiveSimplificationCandidates text) of
    candidate : _ -> candidate
    [] -> text

simplifyFirstReduplicativePair ∷ String → String
simplifyFirstReduplicativePair text =
  case dropReduplicativePairVariants (words text) of
    tokens : _ -> unwords tokens
    [] -> text

looksAdverbialParenthetical ∷ String → Bool
looksAdverbialParenthetical middle =
  let tokens = map (map toLower . stripTokenEdgeNoise) (words middle)
      nonEmpty = filter (not . null) tokens
      hasAtLeast = "at least" `isInfixOf` unwords nonEmpty
  in case nonEmpty of
      first : _ ->
        length nonEmpty <= 8
          && ( first `elem` ["in", "at", "by", "with", "from", "for", "on", "to", "as", "however", "indeed", "perhaps", "certainly", "surely", "truly"]
                || hasAtLeast
             )
      [] -> False

etymologyGlossTailCandidates ∷ String → [String]
etymologyGlossTailCandidates text =
  case splitOnSubstringInsensitive ", to " text of
    Just (prefix, rest)
      | ", to " `isInfixOf` map toLower rest
      , looksEtymologyOriginPrefix prefix ->
          let candidate = trim prefix
          in [candidate | isSentenceCandidate candidate]
    _ -> []

looksEtymologyOriginPrefix ∷ String → Bool
looksEtymologyOriginPrefix prefix =
  let lower = map toLower prefix
  in any (`isInfixOf` lower)
      [ " from "
      , " immediately from "
      , " derived from "
      , " named from "
      ]

appositiveOfChainCandidates ∷ String → [String]
appositiveOfChainCandidates text =
  concatMap rewriteWithMarker verbMarkers
  where
    verbMarkers =
      [ " appears to "
      , " appears "
      , " seem to "
      , " seems to "
      ]
    rewriteWithMarker marker =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, suffix)
          | ofAChainCount prefix >= 2 ->
              let subjectTail = trim (suffixAfterLastOfAChain prefix)
                  candidate = trim (subjectTail <> marker <> suffix)
              in [candidate | isSentenceCandidate candidate]
        _ -> []

ofAChainCount ∷ String → Int
ofAChainCount text =
  countSubstringInsensitive " of a " text

suffixAfterLastOfAChain ∷ String → String
suffixAfterLastOfAChain text =
  case splitOnLastSubstringInsensitive " of a " text of
    Just (_, suffix) -> suffix
    Nothing -> text

predicateTailTrimCandidates ∷ String → [String]
predicateTailTrimCandidates text =
  concatMap rewriteWithMarker verbMarkers
  where
    verbMarkers =
      [ (" appears to ", " appears")
      , (" appears ", " appears")
      , (" seems to ", " seems")
      , (" seem to ", " seem")
      , (" seemed to ", " seemed")
      ]
    rewriteWithMarker (marker, fallbackVerb) =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, _suffix) ->
          let candidate = trim (prefix <> fallbackVerb)
          in [candidate | isSentenceCandidate candidate]
        Nothing -> []

trailingForPhraseCandidates ∷ String → [String]
trailingForPhraseCandidates text =
  case splitOnLastSubstringInsensitive " for " text of
    Just (prefix, suffix)
      | looksTrailingForAdjunct suffix ->
          let candidate = trim prefix
          in [candidate | isSentenceCandidate candidate]
    _ -> []

dropTrailingForAdjunct ∷ String → String
dropTrailingForAdjunct text =
  case splitOnLastSubstringInsensitive " for " text of
    Just (prefix, suffix)
      | looksTrailingForAdjunct suffix -> trim prefix
    _ -> text

looksTrailingForAdjunct ∷ String → Bool
looksTrailingForAdjunct suffix =
  let tokens = normalizedWordTokens suffix
  in not (null tokens)
      && length tokens <= 8
      && not ("," `isInfixOf` suffix)

reduplicativeModifierCandidates ∷ String → [String]
reduplicativeModifierCandidates text =
  nub
    [ candidate
    | candidate <- map unwords (dropReduplicativePairVariants (words text))
    , candidate /= text
    ]

dropReduplicativePairVariants ∷ [String] → [[String]]
dropReduplicativePairVariants tokens =
  [ take idx tokens ++ drop (idx + 2) tokens
  | idx <- [0 .. length tokens - 3]
  , let left = tokens !! idx
        right = tokens !! (idx + 1)
        nounish = tokens !! (idx + 2)
  , isReduplicativeModifierPair left right
  , looksLikelyNounToken nounish
  ]

isReduplicativeModifierPair ∷ String → String → Bool
isReduplicativeModifierPair left right =
  let l = map toLower (stripTokenEdgeNoise left)
      r = map toLower (stripTokenEdgeNoise right)
  in length l >= 4
      && length r >= 4
      && all isAlpha l
      && all isAlpha r
      && commonSuffixLength l r >= 4

looksLikelyNounToken ∷ String → Bool
looksLikelyNounToken token =
  let core = map toLower (stripTokenEdgeNoise token)
  in length core >= 3
      && all isAlpha core

commonSuffixLength ∷ String → String → Int
commonSuffixLength left right = go (reverse left) (reverse right) 0
  where
    go (x : xs) (y : ys) n
      | x == y = go xs ys (n + 1)
    go _ _ n = n

titleCompoundCandidates ∷ String → [String]
titleCompoundCandidates text =
  [ candidate
  | candidate <- [unwords merged, unwords droppedArticle]
  , candidate /= text
  ]
  where
    tokens = words text
    merged = collapseRepeatedTitleTokens False tokens
    droppedArticle = collapseRepeatedTitleTokens True tokens

collapseRepeatedTitleTokens ∷ Bool → [String] → [String]
collapseRepeatedTitleTokens dropLeadingArticle = reverse . go []
  where
    go acc [] = acc
    go acc [x] = x : acc
    go acc (article : left : right : rest)
      | dropLeadingArticle
      , isArticleToken article
      , isRepeatedTitleWord left right =
          go (mergeRepeatedTitleWord left right : acc) rest
    go acc (left : right : rest)
      | isRepeatedTitleWord left right =
          go (mergeRepeatedTitleWord left right : acc) rest
    go acc (x : xs) = go (x : acc) xs

isArticleToken ∷ String → Bool
isArticleToken token =
  map toLower (stripTokenEdgeNoise token) `elem` ["a", "an", "the"]

isRepeatedTitleWord ∷ String → String → Bool
isRepeatedTitleWord left right =
  let leftCore = stripTokenEdgeNoise left
      rightCore = stripTokenEdgeNoise right
  in not (null leftCore)
      && map toLower leftCore == map toLower rightCore
      && looksTitleWord leftCore
      && looksTitleWord rightCore

mergeRepeatedTitleWord ∷ String → String → String
mergeRepeatedTitleWord left right =
  stripTokenEdgeNoise left <> stripTokenEdgeNoise right

looksTitleWord ∷ String → Bool
looksTitleWord token =
  case firstAlphaChar token of
    Just first -> isUpper first && all isAlpha token
    Nothing -> False

isTitleWordToken ∷ String → Bool
isTitleWordToken token =
  looksTitleWord (stripTokenEdgeNoise token)

archaicVerbCandidates ∷ String → [String]
archaicVerbCandidates text =
  [unwords (map normalizeArchaicToken (words text))]

normalizeArchaicToken ∷ String → String
normalizeArchaicToken token =
  case map toLower token of
    "doth" -> "does"
    "hath" -> "has"
    "maketh" -> "makes"
    lower
      | endsWith lower "eth"
      , length lower > 4 -> take (length lower - 3) lower <> "s"
    lower -> lower

passiveInfinitiveCandidates ∷ String → [String]
passiveInfinitiveCandidates text =
  filter (/= text)
    [ replaceAllInsensitive " is to be " " is " (" " <> text <> " ")
    , replaceAllInsensitive " are to be " " are " (" " <> text <> " ")
    , replaceAllInsensitive " was to be " " was " (" " <> text <> " ")
    , replaceAllInsensitive " were to be " " were " (" " <> text <> " ")
    ]
    >>= \candidate -> [trim candidate]

takeInHandCandidates ∷ String → [String]
takeInHandCandidates text =
  filter (/= text)
    [ replaceAllInsensitive "take in hand to " "" text
    ]

byWhatNameCandidates ∷ String → [String]
byWhatNameCandidates text =
  filter (/= text)
    [ replaceAllInsensitive "by what name" "by name" text
    ]

andToInfinitiveCandidates ∷ String → [String]
andToInfinitiveCandidates text =
  filter (/= text)
    [ replaceAllInsensitive ", and to " " and " text
    , replaceAllInsensitive " and to " " and " text
    ]

byNameClauseTrimCandidates ∷ String → [String]
byNameClauseTrimCandidates text =
  case splitOnSubstring " by name " (map toLower text) of
    Just (prefix, rest) ->
      case splitOnSubstring "," rest of
        Just (_, suffixAfterComma) ->
          let candidate = trim (prefix <> " by name," <> suffixAfterComma)
          in [candidate | isSentenceCandidate candidate]
        Nothing -> []
    Nothing -> []

freeRelativeCandidates ∷ String → [String]
freeRelativeCandidates text =
  filter (/= text)
    [ replaceAllInsensitive "that which" "the thing that" text
    ]

frontedSubordinateCandidates ∷ String → [String]
frontedSubordinateCandidates text =
  case detectFrontedSubordinate text of
    Just (subj, subordinate, mainClause)
      | isSentenceCandidate subordinate
      , isSentenceCandidate mainClause ->
          [trim (mainClause <> " " <> subj <> " " <> subordinate)]
    _ -> []

detectFrontedSubordinate ∷ String → Maybe (String, String, String)
detectFrontedSubordinate text =
  case words (trim text) of
    firstToken : _ ->
      let subj = map toLower (filter isAlpha firstToken)
      in if subj `elem` ["while", "when", "if", "because"]
           then do
             (subordinate, mainClause) <- splitAtMainClauseBoundary (drop (length firstToken) (trim text))
             Just (subj, trim subordinate, trim mainClause)
           else Nothing
    _ -> Nothing

splitAtMainClauseBoundary ∷ String → Maybe (String, String)
splitAtMainClauseBoundary text =
  case firstBoundaryIndex (map toLower text) of
    Just idx ->
      let subordinate = take idx text
          mainClause = drop (idx + 2) text
      in Just (subordinate, mainClause)
    Nothing -> Nothing

firstBoundaryIndex ∷ String → Maybe Int
firstBoundaryIndex text =
  findFirstMarker [", i ", ", you ", ", he ", ", she ", ", it ", ", we ", ", they "] text

findFirstMarker ∷ [String] → String → Maybe Int
findFirstMarker markers text =
  minimumMaybe (mapMaybe (`indexOfSubstring` text) markers)

minimumMaybe ∷ [Int] → Maybe Int
minimumMaybe [] = Nothing
minimumMaybe xs = Just (minimum xs)

indexOfSubstring ∷ String → String → Maybe Int
indexOfSubstring needle haystack = go 0 haystack
  where
    go _ [] = Nothing
    go idx remaining
      | needle `isPrefixOf` remaining = Just idx
      | otherwise =
          case remaining of
            _ : rest -> go (idx + 1) rest
            [] -> Nothing

pruneLeavingParentheticalCandidates ∷ String → [String]
pruneLeavingParentheticalCandidates text =
  case splitOnSubstring ", leaving" (map toLower text) of
    Just (prefix, restAfterLeaving) ->
      case firstBoundaryIndex restAfterLeaving of
        Just idx ->
          let mainClause = drop (idx + 2) restAfterLeaving
              candidate = trim (prefix <> ", " <> mainClause)
          in [candidate | isSentenceCandidate candidate]
        Nothing -> []
    Nothing -> []

participialTailTrimCandidates ∷ String → [String]
participialTailTrimCandidates text =
  directTrim ++ nestedTrim
  where
    directTrim =
      case splitOnSubstring ", " text of
        Just (prefix, suffix)
          | isLikelyParticipialTail suffix
          , isSentenceCandidate (trim prefix) -> [trim prefix]
        _ -> []
    nestedTrim =
      case splitOnSubstring ", " text of
        Just (prefix, suffix) ->
          case splitOnSubstring ", " suffix of
            Just (middle, tailSuffix)
              | isLikelyParticipialTail tailSuffix
              , isSentenceCandidate (trim (prefix <> ", " <> middle)) ->
                  [trim (prefix <> ", " <> middle)]
            _ -> []
        _ -> []

isLikelyParticipialTail ∷ String → Bool
isLikelyParticipialTail suffix =
  case map normalizeToken (words suffix) of
    first : second : _ ->
      (isLikelyAdverb first && looksParticipleLike second)
        || looksParticipleLike first
    first : _ -> looksParticipleLike first
    [] -> False

normalizeToken ∷ String → String
normalizeToken =
  map toLower . filter (\c → isAlpha c || c == '\'' || c == '-')

isLikelyAdverb ∷ String → Bool
isLikelyAdverb token =
  length token > 3 && endsWith token "ly"

replaceAllInsensitive ∷ String → String → String → String
replaceAllInsensitive needle replacement haystack =
  replaceAll needle replacement (map toLower haystack)

replaceAll ∷ String → String → String → String
replaceAll needle replacement = go
  where
    go [] = []
    go source
      | needle `isPrefixOf` source = replacement <> go (drop (length needle) source)
      | otherwise =
          case source of
            c : rest -> c : go rest
            [] -> []

splitOnSubstring ∷ String → String → Maybe (String, String)
splitOnSubstring needle haystack = go [] haystack
  where
    go _ [] = Nothing
    go prefix remaining
      | needle `isPrefixOf` remaining =
          Just (reverse prefix, drop (length needle) remaining)
      | otherwise =
          case remaining of
            c : rest -> go (c : prefix) rest
            [] -> Nothing

splitOnSubstringInsensitive ∷ String → String → Maybe (String, String)
splitOnSubstringInsensitive needle haystack =
  case indexOfSubstring (map toLower needle) (map toLower haystack) of
    Just idx ->
      let needleLen = length needle
      in Just (take idx haystack, drop (idx + needleLen) haystack)
    Nothing -> Nothing

splitOnLastSubstringInsensitive ∷ String → String → Maybe (String, String)
splitOnLastSubstringInsensitive needle haystack =
  case lastIndexOfSubstring (map toLower needle) (map toLower haystack) of
    Just idx ->
      let needleLen = length needle
      in Just (take idx haystack, drop (idx + needleLen) haystack)
    Nothing -> Nothing

countSubstringInsensitive ∷ String → String → Int
countSubstringInsensitive needle haystack =
  length (allIndicesOfSubstring (map toLower needle) (map toLower haystack))

lastIndexOfSubstring ∷ String → String → Maybe Int
lastIndexOfSubstring needle haystack =
  case allIndicesOfSubstring needle haystack of
    [] -> Nothing
    xs -> Just (last xs)

allIndicesOfSubstring ∷ String → String → [Int]
allIndicesOfSubstring needle haystack = go 0 haystack
  where
    go _ [] = []
    go idx remaining
      | needle `isPrefixOf` remaining =
          idx : step
      | otherwise = step
      where
        step =
          case remaining of
            _ : rest -> go (idx + 1) rest
            [] -> []

semicolonClauseCandidates ∷ String → [String]
semicolonClauseCandidates text =
  [ clause
  | clause <- map trim (splitOnChar ';' text)
  , clause /= text
  , isSentenceCandidate clause
  ]

splitOnChar ∷ Char → String → [String]
splitOnChar delimiter = go []
  where
    go current [] = [reverse current]
    go current (c : cs)
      | c == delimiter = reverse current : go [] cs
      | otherwise = go (c : current) cs

looksParticipleLike ∷ String → Bool
looksParticipleLike word =
  let normalized = map toLower (filter isAlpha word)
  in length normalized > 3
      && (endsWith normalized "ed" || endsWith normalized "en")

lowercaseFirstWord ∷ String → String
lowercaseFirstWord text =
  case break isSpace text of
    (firstWord, rest) -> map toLower firstWord <> rest

endsWith ∷ String → String → Bool
endsWith txt suffix =
  length txt >= length suffix && drop (length txt - length suffix) txt == suffix

isSingleWordSentence ∷ String → Bool
isSingleWordSentence text =
  case normalizedWordTokens text of
    [token] -> isWordLikeToken token
    _ -> False

normalizedWordTokens ∷ String → [String]
normalizedWordTokens =
  filter (not . null)
    . map stripTokenEdgeNoise
    . words

alphaWordTokens ∷ String → [String]
alphaWordTokens =
  filter (any isAlpha)
    . normalizedWordTokens

isForeignShortFragmentSentence ∷ String → Bool
isForeignShortFragmentSentence text =
  let tokens = alphaWordTokens text
  in not (null tokens)
      && length tokens <= 2
      && all isForeignScriptToken tokens

isForeignDominantSentence ∷ String → Bool
isForeignDominantSentence text =
  let tokens = alphaWordTokens text
      foreignCount = length (filter isForeignScriptToken tokens)
  in length tokens > 2
      && foreignCount * 2 >= length tokens

isForeignScriptToken ∷ String → Bool
isForeignScriptToken token =
  let letters = filter isAlpha token
  in not (null letters) && all (not . isAscii) letters

isNameFragmentSentence ∷ String → Bool
isNameFragmentSentence text =
  let tokens = normalizedWordTokens text
      nonEmptyTokens = filter (not . null) tokens
      nonConnectorTokens = filter (not . isConnectorToken) nonEmptyTokens
  in length nonConnectorTokens >= 2
      && all isNameLikeToken nonConnectorTokens
      && all (\token -> isConnectorToken token || isNameLikeToken token) nonEmptyTokens

isConnectorToken ∷ String → Bool
isConnectorToken token =
  map toLower token `elem` ["and", "or", "&"]

isNameLikeToken ∷ String → Bool
isNameLikeToken token =
  case firstAlphaChar token of
    Just first ->
      isUpper first
        && all (\c -> isAlpha c || c == '\'' || c == '.') token
        && any isAlpha token
    Nothing -> False

firstAlphaChar ∷ String → Maybe Char
firstAlphaChar [] = Nothing
firstAlphaChar (c : cs)
  | isAlpha c = Just c
  | otherwise = firstAlphaChar cs

isInfinitiveGlossSentence ∷ String → Bool
isInfinitiveGlossSentence text =
  let tokens =
        filter (not . null)
          (map (map toLower . stripTokenEdgeNoise) (words text))
      glossTokens = dropWhile isGlossPrefixToken tokens
      toCount = length (filter (== "to") glossTokens)
  in not (null glossTokens)
      && head glossTokens == "to"
      && toCount >= 2
      && all isGlossToken glossTokens

isGlossPrefixToken ∷ String → Bool
isGlossPrefixToken token =
  token `elem` ["and", "or", "&"] || isAbbreviationToken token

isAbbreviationToken ∷ String → Bool
isAbbreviationToken token =
  not (null token)
    && '.' `elem` token
    && any isAlpha token
    && all (\c -> isAlpha c || c == '.') token

isGlossToken ∷ String → Bool
isGlossToken "to" = True
isGlossToken token =
  not (null token)
    && any isAlpha token
    && all (\c -> isAlpha c || c == '\'' || c == '-') token

isWordLikeToken ∷ String → Bool
isWordLikeToken token =
  not (null token)
    && any isAlpha token
    && all (\c → isAlpha c || c == '\'' || c == '-') token

appendFailureIfRoom ∷ Int → [CorpusSentence] → CorpusSentence → [CorpusSentence]
appendFailureIfRoom limit failures failure
  | length failures < limit = failures ++ [failure]
  | otherwise = failures

printFailureExamples ∷ CorpusStats → IO ()
printFailureExamples stats =
  if null (statFailures stats)
    then pure ()
    else do
      putStrLn ""
      putStrLn ("First " <> show (length (statFailures stats)) <> " unparsed example(s):")
      mapM_ printFailure (statFailures stats)
  where
    printFailure s =
      putStrLn ("  - " <> sentenceFile s <> ":" <> show (sentenceLine s) <> " :: " <> sentenceText s)

enforceThresholds ∷ CorpusConfig → CorpusStats → IO ()
enforceThresholds cfg stats = do
  let controlledRate = rate (statControlled stats) (statTotal stats)
      totalRate = rate (statControlled stats + statFallback stats + statAutoPass stats) (statTotal stats)
      controlledOk = checkMinimum (cfgMinControlledRate cfg) controlledRate
      totalOk = checkMinimum (cfgMinTotalRate cfg) totalRate
  if controlledOk && totalOk
    then exitSuccess
    else do
      putStrLn ""
      if not controlledOk
        then putStrLn ("[fail] controlled parse rate " <> renderRate controlledRate <> " is below minimum " <> renderRate (unwrapMinimum (cfgMinControlledRate cfg)))
        else pure ()
      if not totalOk
        then putStrLn ("[fail] total parse rate " <> renderRate totalRate <> " is below minimum " <> renderRate (unwrapMinimum (cfgMinTotalRate cfg)))
        else pure ()
      exitFailure

checkMinimum ∷ Maybe Double → Double → Bool
checkMinimum Nothing _ = True
checkMinimum (Just minimumValue) actual = actual >= minimumValue

unwrapMinimum ∷ Maybe Double → Double
unwrapMinimum (Just value) = value
unwrapMinimum Nothing = 0.0

rate ∷ Int → Int → Double
rate _ 0 = 0.0
rate part whole = fromIntegral part / fromIntegral whole

renderRate ∷ Double → String
renderRate r = show percent <> "%"
  where
    roundedBp ∷ Int
    roundedBp = round (r * 10000.0)
    percent ∷ Double
    percent = fromIntegral roundedBp / 100.0

parseCommand ∷ [String] → Either String Command
parseCommand args
  | any (`elem` ["--help", "-h"]) args = Right ShowHelp
  | otherwise = RunCorpus <$> parseArgs defaultConfig args

parseArgs ∷ CorpusConfig → [String] → Either String CorpusConfig
parseArgs cfg [] = Right cfg
parseArgs cfg ("--file" : path : rest) =
  parseArgs cfg {cfgFiles = cfgFiles cfg ++ [path]} rest
parseArgs cfg ("--dir" : path : rest) =
  parseArgs cfg {cfgDirs = cfgDirs cfg ++ [path]} rest
parseArgs cfg ("--show-failures" : n : rest) = do
  parsed <- parseNonNegativeInt "--show-failures" n
  parseArgs cfg {cfgShowFailures = parsed} rest
parseArgs cfg ("--max-sentences" : n : rest) = do
  parsed <- parseNonNegativeInt "--max-sentences" n
  parseArgs cfg {cfgMaxSentences = Just parsed} rest
parseArgs cfg ("--stop-after-unparsed" : n : rest) = do
  parsed <- parseNonNegativeInt "--stop-after-unparsed" n
  parseArgs cfg {cfgStopAfterUnparsed = Just parsed} rest
parseArgs cfg ("--min-controlled-rate" : n : rest) = do
  parsed <- parseRate "--min-controlled-rate" n
  parseArgs cfg {cfgMinControlledRate = Just parsed} rest
parseArgs cfg ("--min-total-rate" : n : rest) = do
  parsed <- parseRate "--min-total-rate" n
  parseArgs cfg {cfgMinTotalRate = Just parsed} rest
parseArgs _ ("--file" : []) = Left "missing value for --file"
parseArgs _ ("--dir" : []) = Left "missing value for --dir"
parseArgs _ ("--show-failures" : []) = Left "missing value for --show-failures"
parseArgs _ ("--max-sentences" : []) = Left "missing value for --max-sentences"
parseArgs _ ("--stop-after-unparsed" : []) = Left "missing value for --stop-after-unparsed"
parseArgs _ ("--min-controlled-rate" : []) = Left "missing value for --min-controlled-rate"
parseArgs _ ("--min-total-rate" : []) = Left "missing value for --min-total-rate"
parseArgs _ (flag : _) = Left ("unknown argument: " <> flag)

parseNonNegativeInt ∷ String → String → Either String Int
parseNonNegativeInt flag input =
  case reads input of
    [(n, "")] | n >= 0 -> Right n
    _ -> Left ("invalid value for " <> flag <> ": " <> input)

parseRate ∷ String → String → Either String Double
parseRate flag input =
  case reads input of
    [(n, "")] | n >= 0.0 && n <= 1.0 -> Right n
    _ -> Left ("invalid value for " <> flag <> ": " <> input <> " (expected 0.0 to 1.0)")

unwrapMaybeInt ∷ Maybe Int → Int
unwrapMaybeInt (Just n) = n
unwrapMaybeInt Nothing = 0

usage ∷ String
usage = unlines
  [ "Usage:"
  , "  cabal run erato-corpus -- [--file PATH]... [--dir PATH]..."
  , "                           [--show-failures N]"
  , "                           [--max-sentences N]"
  , "                           [--stop-after-unparsed N]"
  , "                           [--min-controlled-rate R]"
  , "                           [--min-total-rate R]"
  , ""
  , "Options:"
  , "  --file PATH               Add a specific .txt file."
  , "  --dir PATH                Recursively scan a directory for .txt files."
  , "  --show-failures N         Print up to N unparsed examples (default: 20)."
  , "  --max-sentences N         Parse at most N sentence candidates."
  , "  --stop-after-unparsed N   Stop early after N unparsed sentences."
  , "  --min-controlled-rate R   Fail if controlled parse rate is below R (0.0-1.0)."
  , "  --min-total-rate R        Fail if controlled+fallback parse rate is below R (0.0-1.0)."
  ]
