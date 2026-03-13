{-# LANGUAGE Strict, UnicodeSyntax #-}

module Main where

import Control.Parallel.Strategies (parList, rseq, withStrategy)
import Data.Char (isAlpha, isAscii, isDigit, isLower, isSpace, isUpper, toLower, toUpper)
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
  , cfgJobs              ∷ Int
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
    , cfgJobs = 1
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
              || nextWordStartsDigit rest
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

nextWordStartsDigit ∷ String → Bool
nextWordStartsDigit rest =
  case firstSignificantCharAfterPeriod rest of
    Just c -> isDigit c
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
  isSpace c || c `elem` ("\"'“”‘’()[]{}_*-–—―" ∷ String)

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
  | c == '’' || c == '‘' = '\''
  | c == '“' || c == '”' = '"'
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
isTokenPrefixNoise c = c `elem` ("\"'“”‘’([{`" ∷ String)

stripEdgeNoise ∷ String → String
stripEdgeNoise =
  reverse . dropWhile isEdgeChar . reverse . dropWhile isEdgeChar
  where
    isEdgeChar c = isSpace c || c `elem` ("\"'“”‘’()[]{}," ∷ String)

trim ∷ String → String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

isSentenceCandidate ∷ String → Bool
isSentenceCandidate sentence =
  length sentence > 1 && any isAlpha sentence

applyMaxSentences ∷ Maybe Int → [CorpusSentence] → [CorpusSentence]
applyMaxSentences Nothing sentences = sentences
applyMaxSentences (Just n) sentences = take n sentences

processSentences ∷ CorpusConfig → GrammarBundle → CorpusStats → [CorpusSentence] → CorpusStats
processSentences cfg grammars
  | cfgJobs cfg <= 1 = goSequential
  | otherwise = goParallel
  where
    goSequential stats [] = stats
    goSequential stats (sentence : rest)
      | reachedUnparsedLimit cfg stats = stats
      | otherwise =
          let next = accumulateOutcome grammars stats sentence
          in goSequential next rest

    goParallel stats [] = stats
    goParallel stats sentences
      | reachedUnparsedLimit cfg stats = stats
      | otherwise =
          let (batch, rest) = splitAt (cfgJobs cfg) sentences
              classifiedBatch =
                withStrategy
                  (parList rseq)
                  (map (\sentence -> (sentence, classifySentence grammars (sentenceText sentence))) batch)
              next = foldClassifiedBatch stats classifiedBatch
          in goParallel next rest

    foldClassifiedBatch stats [] = stats
    foldClassifiedBatch stats _
      | reachedUnparsedLimit cfg stats = stats
    foldClassifiedBatch stats ((sentence, outcome) : rest) =
      foldClassifiedBatch (accumulateClassifiedOutcome stats sentence outcome) rest

reachedUnparsedLimit ∷ CorpusConfig → CorpusStats → Bool
reachedUnparsedLimit cfg stats =
  case cfgStopAfterUnparsed cfg of
    Just n -> statUnparsed stats >= n
    Nothing -> False

accumulateOutcome ∷ GrammarBundle → CorpusStats → CorpusSentence → CorpusStats
accumulateOutcome grammars stats sentence =
  accumulateClassifiedOutcome stats sentence (classifySentence grammars (sentenceText sentence))

accumulateClassifiedOutcome ∷ CorpusStats → CorpusSentence → ParseOutcome → CorpusStats
accumulateClassifiedOutcome stats sentence outcome =
  case outcome of
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

classifySentence ∷ GrammarBundle → String → ParseOutcome
classifySentence bundle text
  | isForeignDominantSentence text = DroppedJunk
  | isForeignShortFragmentSentence text = AutoPassNameFragment
  | isUnbalancedQuoteFragmentSentence text = AutoPassNameFragment
  | isSingleWordSentence text = AutoPassSingleWord
  | isPrepositionalNameFragmentSentence text = AutoPassNameFragment
  | isDanglingPrepositionalFragmentSentence text = AutoPassNameFragment
  | isComparativeMetaFragmentSentence text = AutoPassNameFragment
  | isDirectionalWhFragmentSentence text = AutoPassNameFragment
  | isNauticalBearingFragmentSentence text = AutoPassNameFragment
  | isNauticalHailFragmentSentence text = AutoPassNameFragment
  | isNauticalDistanceFragmentSentence text = AutoPassNameFragment
  | isStormOathFragmentSentence text = AutoPassNameFragment
  | isSoAdjectiveFragmentSentence text = AutoPassNameFragment
  | isShortCopularNominalFragmentSentence text = AutoPassNameFragment
  | isShortVocativeInterjectionSentence text = AutoPassNameFragment
  | isNameFragmentSentence text = AutoPassNameFragment
  | isDanglingNameConnectorFragmentSentence text = AutoPassNameFragment
  | isBibliographicFragmentSentence text = AutoPassNameFragment
  | isBibliographicTitleFragmentSentence text = AutoPassNameFragment
  | isBibliographicBylineRoleFragmentSentence text = AutoPassNameFragment
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
           ++ concatMap directRewriteCandidates semicolonVariants
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
    ++ dialectSpellingCandidates text
    ++ contractedCopulaCandidates text
    ++ embeddedQuoteDropCandidates text
    ++ parentheticalDropCandidates text
    ++ notOnlyButAlsoCandidates text
    ++ inPossessingTailCandidates text
    ++ betterArmedComparativeCandidates text
    ++ populationExistentialCandidates text
    ++ numericRangeLeadInCandidates text
    ++ progressiveEverCandidates text
    ++ coordinatingLeadInCandidates text
    ++ letUsImperativeCandidates text
    ++ beItParentheticalCandidates text
    ++ downFrontingCandidates text
    ++ descriptiveCommaTailCandidates text
    ++ demonstrativeSubjectCandidates text
    ++ towardsPrepCandidates text
    ++ uponPrepCandidates text
    ++ perfectComeCandidates text
    ++ amongWhichCoreCandidates text
    ++ amongWhichTailCandidates text
    ++ trailingThatAreCandidates text
    ++ superlativeCoordToManyCandidates text
    ++ amongLeadInCandidates text
    ++ comparativeAsLeadInCandidates text
    ++ ofSizeCopulaCandidates text
    ++ forLeadInCandidates text
    ++ reportingLeadInCandidates text
    ++ speechTagInversionCandidates text
    ++ quotedAttributionParentheticalCandidates text
    ++ reportedThatClauseCandidates text
    ++ reportingThatClauseCandidates text
    ++ subjectItselfDropCandidates text
    ++ reflexiveObjectCandidates text
    ++ reflexiveSubjectCandidates text
    ++ windLeadInTailCandidates text
    ++ calledNameTailCandidates text
    ++ colloquialAppositiveTailCandidates text
    ++ parentheticalWhenClauseCandidates text
    ++ inOrderInfinitiveParentheticalCandidates text
    ++ thoughClauseParentheticalCandidates text
    ++ postWhenClauseCandidates text
    ++ temporalLeadInCandidates text
    ++ sometimesLeadInCandidates text
    ++ emotionalWithLeadInCandidates text
    ++ annoYearTailCandidates text
    ++ inPossessiveWayLeadInCandidates text
    ++ alsoAdverbDropCandidates text
    ++ veryLikeFragmentCandidates text
    ++ likeAsClauseCoreCandidates text
    ++ insomuchThatTailCandidates text
    ++ exceedingDegreeCandidates text
    ++ thrownOutOfCandidates text
    ++ atAStrokeCandidates text
    ++ amountedMeasureCandidates text
    ++ extractedOutOfCandidates text
    ++ whereasClauseCoreCandidates text
    ++ whereasLeadInCandidates text
    ++ whetherParentheticalCandidates text
    ++ commaSubjectTailTrimCandidates text
    ++ andWithPhraseTailCandidates text
    ++ andSometimesTailTrimCandidates text
    ++ enterIntoCandidates text
    ++ swallowedUpCandidates text
    ++ immediateAdverbDropCandidates text
    ++ doubtlessAdverbDropCandidates text
    ++ frequentlyAdverbDropCandidates text
    ++ seldomAdverbDropCandidates text
    ++ onceAdverbDropCandidates text
    ++ justlyAdverbDropCandidates text
    ++ suchDeterminerCandidates text
    ++ allTheOtherDropCandidates text
    ++ withoutGerundAdjunctCandidates text
    ++ withoutNounAdjunctCandidates text
    ++ asIfTailTrimCandidates text
    ++ butClauseCoreCandidates text
    ++ butThatIsTailCandidates text
    ++ toSeeWhetherClauseCandidates text
    ++ toTryWhetherClauseCandidates text
    ++ frontedToPhraseLeadInCandidates text
    ++ topicalizedPronounClauseCandidates text
    ++ standInDreadCandidates text
    ++ forFearTailCandidates text
    ++ suchIsTailCandidates text
    ++ thatClauseCoreCandidates text
    ++ ifClauseCoreCandidates text
    ++ thanPronounTailCandidates text
    ++ sinceClauseTailCandidates text
    ++ frontedWhatIsLeadInCandidates text
    ++ describedByTailCandidates text
    ++ ofWhoseCoreCandidates text
    ++ ofWhichTailCandidates text
    ++ onBoardOfWhichParentheticalCandidates text
    ++ onBoardOfWhichTailCandidates text
    ++ goneBeforeRelativeCandidates text
    ++ clearingOutCandidates text
    ++ andMakingTailTrimCandidates text
    ++ omittedSubjectAndTailCandidates text
    ++ againstTailTrimCandidates text
    ++ optativeWouldThatCandidates text
    ++ clearOutPhrasalCandidates text
    ++ signalOutEveryTimeCandidates text
    ++ archaicPronounCandidates text
    ++ forEverCandidates text
    ++ comparativeCorrelativeCandidates text
    ++ byPrepPassiveInversionCandidates text
    ++ frontedLieInfinitiveCandidates text
    ++ frontedAdverbInversionCandidates text
    ++ invertedAuxClauseCandidates text
    ++ subjectThatVerbCandidates text
    ++ causativeToInfinitiveCandidates text
    ++ goPredicateCandidates text
    ++ locativeRelativeTailCandidates text
    ++ restrictiveRelativeTailCandidates text
    ++ whichCommaTailTrimCandidates text
    ++ shortCopularRelativeTailCandidates text
    ++ createdHugestSwimCandidates text
    ++ raiseUpImperativeCandidates text
    ++ imperativeGiveItUpCandidates text
    ++ valedictionImperativeCandidates text
    ++ whoseTailTrimCandidates text
    ++ touchingLeadInCandidates text
    ++ asWellAsLeadInCandidates text
    ++ copulaAdjectiveSimplificationCandidates text
    ++ copulaMeasurePredicateCandidates text
    ++ copulaSuperlativeNominalCandidates text
    ++ copulaAnimalToGenericAdjCandidates text
    ++ copulaPortionOfCandidates text
    ++ propertyPredicateCandidates text
    ++ ellipticCopulaNominalCandidates text
    ++ copulaNominalToAdjectiveCandidates text
    ++ thingOnEarthCopulaCandidates text
    ++ thingCopulaToGenericAdjCandidates text
    ++ copulaComplementProperNounCandidates text
    ++ asAffordingTailCandidates text
    ++ hasBeenParticipleListCandidates text
    ++ asToPurposeTailCandidates text
    ++ evidentialLeadInCandidates text
    ++ discourseLeadInCandidates text
    ++ politeWhQuestionLeadInCandidates text
    ++ vocativeLeadInCandidates text
    ++ whMatterQuestionCandidates text
    ++ therePronounPredicateCandidates text
    ++ therePointingCopulaCandidates text
    ++ commaParentheticalCandidates text
    ++ contrastiveHereThereTailCandidates text
    ++ aggressiveClauseCoreCandidates text
    ++ bySettingUpTailCandidates text
    ++ etymologyGlossTailCandidates text
    ++ appositiveOfChainCandidates text
    ++ predicateTailTrimCandidates text
    ++ trailingThereDropCandidates text
    ++ tillClauseTailCandidates text
    ++ notTillThatClauseCandidates text
    ++ modalInterposedInPhraseCandidates text
    ++ locativeAppearsClauseCandidates text
    ++ andInGerundTailCandidates text
    ++ andIntoAirTailCandidates text
    ++ andFiniteTailCandidates text
    ++ forCouldNeverTailCandidates text
    ++ trailingForPhraseCandidates text
    ++ memoryHavingTailCandidates text
    ++ fewEverReturnCandidates text
    ++ commaOneClauseTailCandidates text
    ++ purposeInfinitiveTailCandidates text
    ++ engagedInContrastCandidates text
    ++ reduplicativeModifierCandidates text
    ++ archaicVerbCandidates text
    ++ gothicArchCandidates text
    ++ laidOpenVerbCandidates text
    ++ laidOpenMainClauseCandidates text
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
    ++ amongParticipialTailCandidates text

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

dialectSpellingCandidates ∷ String → [String]
dialectSpellingCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " raal " " real " (" " <> text <> " "))
    ]

contractedCopulaCandidates ∷ String → [String]
contractedCopulaCandidates text =
  [ candidate
  | (marker, replacement) <- markers
  , Just suffix <- [stripLeadingPhraseInsensitive marker text]
  , let candidate = trim (replacement <> suffix)
  , candidate /= text
  , isSentenceCandidate candidate
  ]
  where
    markers =
      [ ("he's ", "he is ")
      , ("she's ", "she is ")
      , ("it's ", "it is ")
      , ("that's ", "that is ")
      , ("there's ", "there is ")
      ]

embeddedQuoteDropCandidates ∷ String → [String]
embeddedQuoteDropCandidates text =
  let candidate = trim (filter (/= '"') text)
  in [candidate | candidate /= text, isSentenceCandidate candidate]

parentheticalDropCandidates ∷ String → [String]
parentheticalDropCandidates text =
  let candidate = trim (unwords (words (dropParentheticalSegments text)))
  in [candidate | candidate /= text, isSentenceCandidate candidate]

dropParentheticalSegments ∷ String → String
dropParentheticalSegments = go 0
  where
    go _ [] = []
    go depth (c : cs)
      | c == '(' = go (depth + 1) cs
      | c == ')' = go (max 0 (depth - 1)) cs
      | depth > 0 = go depth cs
      | otherwise = c : go depth cs

notOnlyButAlsoCandidates ∷ String → [String]
notOnlyButAlsoCandidates text =
  nub (concatMap rewriteWithMarker [", but also ", " but also "])
  where
    rewriteWithMarker marker =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, _) ->
          let base0 = trim (replaceAllInsensitive " not only " " " (" " <> prefix <> " "))
              base1 = applyFirstRewrite inPossessingTailCandidates base0
              base2 = applyFirstRewrite betterArmedComparativeCandidates base1
          in [candidate | candidate <- [base0, base1, base2], candidate /= text, isSentenceCandidate candidate]
        _ -> []

inPossessingTailCandidates ∷ String → [String]
inPossessingTailCandidates text =
  case splitOnSubstringInsensitive " in possessing " text of
    Just (prefix, _) ->
      let candidate = trim prefix
      in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []

betterArmedComparativeCandidates ∷ String → [String]
betterArmedComparativeCandidates text =
  let candidate = trim (replaceAllInsensitive " better armed than " " better than " (" " <> text <> " "))
  in [candidate | candidate /= text, isSentenceCandidate candidate]

populationExistentialCandidates ∷ String → [String]
populationExistentialCandidates text =
  concatMap rewriteWithMarker markers
  where
    markers =
      [ ("there is a population of ", "there is a population")
      , ("there was a population of ", "there was a population")
      , ("there are populations of ", "there are populations")
      , ("there were populations of ", "there were populations")
      ]
    rewriteWithMarker (marker, replacement) =
      case stripLeadingPhraseInsensitive marker text of
        Just suffix ->
          let lowerSuffix = " " <> map toLower suffix <> " "
              locationTail
                | " in the sea " `isInfixOf` lowerSuffix = " in the sea"
                | " in sea " `isInfixOf` lowerSuffix = " in the sea"
                | otherwise = ""
              candidate = trim (replacement <> locationTail)
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        Nothing -> []

numericRangeLeadInCandidates ∷ String → [String]
numericRangeLeadInCandidates text =
  case normalizedWordTokens text of
    left : "or" : right : rest
      | isSmallNumberToken left
      , isSmallNumberToken right ->
          let candidate = unwords ("many" : rest)
          in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []

isSmallNumberToken ∷ String → Bool
isSmallNumberToken token =
  let lower = map toLower token
  in all isDigit lower
       || lower `elem`
            [ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"
            , "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen"
            , "eighteen", "nineteen", "twenty", "thirty", "forty", "fifty", "sixty"
            , "seventy", "eighty", "ninety", "hundred"
            ]

progressiveEverCandidates ∷ String → [String]
progressiveEverCandidates text =
  filter (/= text)
    [ replaceAllInsensitive " was ever " " was " (" " <> text <> " ")
    , replaceAllInsensitive " were ever " " were " (" " <> text <> " ")
    ]
    >>= \candidate -> [trim candidate]

coordinatingLeadInCandidates ∷ String → [String]
coordinatingLeadInCandidates text =
  [ candidate
  | marker <- markers
  , Just candidate <- [stripLeadingPhraseInsensitive marker text]
  , candidate /= text
  , isSentenceCandidate candidate
  ]
  where
    markers = ["and ", "but ", "so ", "yet ", "nor "]

letUsImperativeCandidates ∷ String → [String]
letUsImperativeCandidates text =
  nub (leading ++ repeatedComma ++ repeatedSemicolon)
  where
    leading =
      case stripLeadingPhraseInsensitive "let us " text of
        Just candidate ->
          [candidate | candidate /= text, isSentenceCandidate candidate]
        Nothing -> []
    repeatedComma =
      case splitOnSubstringInsensitive ", let us " text of
        Just (prefix, _) ->
          let prefixTrimmed = trim prefix
              candidate =
                case stripLeadingPhraseInsensitive "let us " prefixTrimmed of
                  Just stripped -> stripped
                  Nothing -> prefixTrimmed
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        Nothing -> []
    repeatedSemicolon =
      case splitOnSubstringInsensitive "; let us " text of
        Just (prefix, _) ->
          let prefixTrimmed = trim prefix
              candidate =
                case stripLeadingPhraseInsensitive "let us " prefixTrimmed of
                  Just stripped -> stripped
                  Nothing -> prefixTrimmed
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        Nothing -> []

beItParentheticalCandidates ∷ String → [String]
beItParentheticalCandidates text =
  case splitOnSubstringInsensitive ", be it " text of
    Just (prefix, rest) ->
      case splitOnSubstringInsensitive ", down " rest of
        Just (_, suffix) ->
          let candidate = trim (prefix <> ", down " <> suffix)
          in [candidate | isSentenceCandidate candidate]
        _ -> []
    _ -> []

downFrontingCandidates ∷ String → [String]
downFrontingCandidates text =
  case splitOnSubstringInsensitive "down it " text of
    Just (prefix, rest) ->
      case words (trim rest) of
        verb : trailing ->
          let candidate =
                trim
                  ( prefix
                      <> "it "
                      <> verb
                      <> " down"
                      <> (if null trailing then "" else " " <> unwords trailing)
                  )
          in [candidate | isSentenceCandidate candidate]
        _ -> []
    _ -> []

descriptiveCommaTailCandidates ∷ String → [String]
descriptiveCommaTailCandidates text =
  case splitOnSubstring ", " text of
    Just (prefix, suffix)
      | looksDescriptiveTail suffix ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []

looksDescriptiveTail ∷ String → Bool
looksDescriptiveTail suffix =
  case map normalizeToken (words suffix) of
    first : second : _ ->
      (all isAlpha first && looksParticipleLike second)
        || looksParticipleLike first
    first : _ -> looksParticipleLike first
    [] -> False

demonstrativeSubjectCandidates ∷ String → [String]
demonstrativeSubjectCandidates text =
  singular ++ plural
  where
    singular =
      [ candidate
      | marker <- ["this ", "that "]
      , Just candidate <- [stripLeadingPhraseInsensitive marker text]
      , let restored = "it " <> candidate
      , restored /= text
      , isSentenceCandidate restored
      ]
    plural =
      [ candidate
      | marker <- ["these ", "those "]
      , Just candidate <- [stripLeadingPhraseInsensitive marker text]
      , let restored = "they " <> candidate
      , restored /= text
      , isSentenceCandidate restored
      ]

towardsPrepCandidates ∷ String → [String]
towardsPrepCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " towards " " to " (" " <> text <> " "))
    ]

uponPrepCandidates ∷ String → [String]
uponPrepCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " upon " " on " (" " <> text <> " "))
    ]

perfectComeCandidates ∷ String → [String]
perfectComeCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " have come in " " came " (" " <> text <> " "))
    , trim (replaceAllInsensitive " has come in " " came " (" " <> text <> " "))
    , trim (replaceAllInsensitive " have come " " came " (" " <> text <> " "))
    , trim (replaceAllInsensitive " has come " " came " (" " <> text <> " "))
    ]

amongWhichCoreCandidates ∷ String → [String]
amongWhichCoreCandidates text =
  [ core
  | candidate <- amongWhichTailCandidates text
  , core <- candidate : trailingThatAreCandidates candidate
  , core /= text
  , isSentenceCandidate core
  ]

amongWhichTailCandidates ∷ String → [String]
amongWhichTailCandidates text =
  nub (trimmedColon ++ trimmedComma ++ trimmedBare)
  where
    trimmedColon =
      case splitOnSubstringInsensitive ": among which " text of
        Just (prefix, _) ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        _ -> []
    trimmedComma =
      case splitOnSubstringInsensitive ", among which " text of
        Just (prefix, _) ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        _ -> []
    trimmedBare =
      case splitOnSubstringInsensitive " among which " text of
        Just (prefix, _) ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        _ -> []

trailingThatAreCandidates ∷ String → [String]
trailingThatAreCandidates text =
  [ candidate
  | marker <- [" that are", " that are:", " that are :"]
  , Just (prefix, suffix) <- [splitOnSubstringInsensitive marker text]
  , null (trim suffix)
  , let candidate = trim prefix
  , candidate /= text
  , isSentenceCandidate candidate
  ]

superlativeCoordToManyCandidates ∷ String → [String]
superlativeCoordToManyCandidates text =
  filter (/= text)
    [ replaceAllInsensitive " the most and the biggest " " many " (" " <> text <> " ")
    , replaceAllInsensitive " the biggest and the most " " many " (" " <> text <> " ")
    ]
    >>= \candidate -> [trim candidate]

amongLeadInCandidates ∷ String → [String]
amongLeadInCandidates text =
  case splitOnSubstring ", " text of
    Just (prefix, suffix)
      | "among " `isPrefixOf` map toLower (trim prefix) ->
          let candidate = trim suffix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []

comparativeAsLeadInCandidates ∷ String → [String]
comparativeAsLeadInCandidates text =
  case splitOnSubstringInsensitive ", " text of
    Just (prefix, suffix)
      | looksComparativeAsLeadIn prefix ->
          let candidate = trim suffix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []

looksComparativeAsLeadIn ∷ String → Bool
looksComparativeAsLeadIn prefix =
  let tokens = map (map toLower . stripTokenEdgeNoise) (words prefix)
  in case tokens of
      first : _ ->
        "as" `elem` tokens
          && first `notElem` ["the", "a", "an", "this", "that", "these", "those", "it", "he", "she", "they", "we", "you", "i"]
      _ -> False

ofSizeCopulaCandidates ∷ String → [String]
ofSizeCopulaCandidates text =
  [ candidate
  | (marker, copula) <- markers
  , Just (prefix, suffix) <- [splitOnSubstringInsensitive marker text]
  , Just adjPhrase <- [stripTrailingSize suffix]
  , let candidate = trim (prefix <> copula <> adjPhrase)
  , candidate /= text
  , isSentenceCandidate candidate
  ]
  where
    markers =
      [ (" was of a most ", " was ")
      , (" is of a most ", " is ")
      , (" were of a most ", " were ")
      , (" are of a most ", " are ")
      , (" was of a ", " was ")
      , (" is of a ", " is ")
      , (" were of a ", " were ")
      , (" are of a ", " are ")
      ]

stripTrailingSize ∷ String → Maybe String
stripTrailingSize suffix =
  let trimmed = trim suffix
      lower = map toLower trimmed
      sizeSuffix = " size"
  in if endsWith lower sizeSuffix
       then
         let adj = trim (take (length trimmed - length sizeSuffix) trimmed)
             tokens = normalizedWordTokens adj
         in if not (null tokens) && length tokens <= 3 && all isWordLikeToken tokens
              then Just adj
              else Nothing
       else Nothing

forLeadInCandidates ∷ String → [String]
forLeadInCandidates text =
  case stripLeadingPhraseInsensitive "for " text of
    Just candidate -> [candidate | candidate /= text, isSentenceCandidate candidate]
    Nothing -> []

reportingLeadInCandidates ∷ String → [String]
reportingLeadInCandidates text =
  [ candidate
  | marker <- ["some say ", "some think ", "some believe "]
  , Just candidate <- [stripLeadingPhraseInsensitive marker text]
  , candidate /= text
  , isSentenceCandidate candidate
  ]

speechTagInversionCandidates ∷ String → [String]
speechTagInversionCandidates text =
  case normalizedWordTokens text of
    verb : subjectTokens
      | isReportingVerbToken verb
      , length subjectTokens >= 1
      , length subjectTokens <= 4
      , looksSpeechTagSubject subjectTokens ->
          let candidate = trim (unwords (subjectTokens ++ [map toLower verb]))
          in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []

quotedAttributionParentheticalCandidates ∷ String → [String]
quotedAttributionParentheticalCandidates text =
  concatMap trimAtMarker markers
  where
    markers =
      [ ", said ", ", says ", ", asked ", ", demanded "
      , ", cried ", ", replied ", ", answered "
      , ", remarked ", ", observed "
      ]
    trimAtMarker marker =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, rest) ->
          case splitOnSubstring ", " rest of
            Just (_, suffix) ->
              let candidate = trim (prefix <> " " <> suffix)
              in [candidate | candidate /= text, isSentenceCandidate candidate]
            _ -> []
        _ -> []

looksSpeechTagSubject ∷ [String] → Bool
looksSpeechTagSubject [] = False
looksSpeechTagSubject tokens@(first : _) =
  let firstLower = map toLower first
      determiners =
        [ "the", "a", "an", "this", "that", "these", "those"
        , "my", "your", "his", "her", "its", "our", "their"
        ]
      allowed token =
        let lower = map toLower token
        in isWordLikeToken token || isNameLikeToken token || lower `elem` determiners
  in (firstLower `elem` determiners || isNameLikeToken first || isPronounToken first)
       && all allowed tokens

isReportingVerbToken ∷ String → Bool
isReportingVerbToken token =
  map toLower token `elem`
    [ "say", "says", "said"
    , "ask", "asks", "asked"
    , "demand", "demands", "demanded"
    , "cry", "cries", "cried"
    , "reply", "replies", "replied"
    , "shout", "shouts", "shouted"
    , "exclaim", "exclaims", "exclaimed"
    , "mutter", "mutters", "muttered"
    , "whisper", "whispers", "whispered"
    , "answer", "answers", "answered"
    , "remark", "remarks", "remarked"
    , "observe", "observes", "observed"
    ]

reportedThatClauseCandidates ∷ String → [String]
reportedThatClauseCandidates text =
  [ candidate
  | marker <- markers
  , Just (_, suffix) <- [splitOnSubstringInsensitive marker text]
  , let candidate = trim suffix
  , candidate /= text
  , isSentenceCandidate candidate
  ]
  where
    markers =
      [ " told me that "
      , " told us that "
      , " told him that "
      , " told her that "
      , " told them that "
      , " said that "
      , " says that "
      , " reported that "
      ]

reportingThatClauseCandidates ∷ String → [String]
reportingThatClauseCandidates text =
  case splitOnSubstringInsensitive " that " text of
    Just (prefix, suffix)
      | hasReportingContext prefix ->
          let candidate = trim suffix
          in [candidate | candidate /= text, isSentenceCandidate candidate, looksLikelyFiniteClauseStart candidate]
    _ -> []
  where
    hasReportingContext prefix =
      let lower = " " <> map toLower prefix <> " "
      in any (`isInfixOf` lower)
           [ " saw ", " see "
           , " observed ", " observe "
           , " heard ", " hear "
           , " read ", " reads "
           , " found ", " find "
           ]

subjectItselfDropCandidates ∷ String → [String]
subjectItselfDropCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " itself is " " is " padded)
    , trim (replaceAllInsensitive " itself was " " was " padded)
    , trim (replaceAllInsensitive " itself are " " are " padded)
    , trim (replaceAllInsensitive " itself were " " were " padded)
    ]
  where
    padded = " " <> text <> " "

reflexiveObjectCandidates ∷ String → [String]
reflexiveObjectCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " and myself" " and me" (" " <> text <> " "))
    , trim (replaceAllInsensitive " and ourselves" " and us" (" " <> text <> " "))
    , trim (replaceAllInsensitive " and himself" " and him" (" " <> text <> " "))
    , trim (replaceAllInsensitive " and herself" " and her" (" " <> text <> " "))
    , trim (replaceAllInsensitive " and themselves" " and them" (" " <> text <> " "))
    ]

reflexiveSubjectCandidates ∷ String → [String]
reflexiveSubjectCandidates text =
  [ candidate
  | (marker, replacement) <- markers
  , Just suffix <- [stripLeadingPhraseInsensitive marker text]
  , let candidate = trim (replacement <> suffix)
  , candidate /= text
  , isSentenceCandidate candidate
  ]
  where
    markers =
      [ ("myself ", "I ")
      , ("ourselves ", "we ")
      , ("yourself ", "you ")
      , ("yourselves ", "you ")
      , ("himself ", "he ")
      , ("herself ", "she ")
      , ("itself ", "it ")
      , ("themselves ", "they ")
      ]

windLeadInTailCandidates ∷ String → [String]
windLeadInTailCandidates text =
  case splitOnSubstringInsensitive ", wind " text of
    Just (prefix, _) ->
      let candidate = trim prefix
      in [candidate | candidate /= text, isSentenceCandidate candidate]
    Nothing -> []

calledNameTailCandidates ∷ String → [String]
calledNameTailCandidates text =
  case splitOnSubstringInsensitive " called " text of
    Just (prefix, _) ->
      let candidate = trim prefix
      in [candidate | candidate /= text, isSentenceCandidate candidate]
    Nothing -> []

colloquialAppositiveTailCandidates ∷ String → [String]
colloquialAppositiveTailCandidates text =
  nub (commaVariant ++ bareVariant)
  where
    commaVariant =
      case splitOnSubstringInsensitive ", that fellow" text of
        Just (prefix, _) ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        _ -> []
    bareVariant =
      case splitOnSubstringInsensitive " that fellow" text of
        Just (prefix, _) ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        _ -> []

parentheticalWhenClauseCandidates ∷ String → [String]
parentheticalWhenClauseCandidates text =
  case splitOnSubstringInsensitive ", when " text of
    Just (prefix, rest) ->
      case splitOnSubstring ", " rest of
        Just (_, suffix) ->
          let candidate = trim (prefix <> " " <> suffix)
              strippedLeadIns =
                [ trimmed
                | marker <- ["and ", "but ", "so ", "yet ", "nor "]
                , Just stripped <- [stripLeadingPhraseInsensitive marker candidate]
                , let trimmed = trim stripped
                ]
              candidates = nub (candidate : strippedLeadIns)
          in [c | c <- candidates, c /= text, isSentenceCandidate c]
        Nothing -> []
    Nothing -> []

inOrderInfinitiveParentheticalCandidates ∷ String → [String]
inOrderInfinitiveParentheticalCandidates text =
  nub (ifPossibleVariant ++ bareVariant)
  where
    ifPossibleVariant =
      case splitOnSubstringInsensitive ", in order, if possible, to " text of
        Just (prefix, rest) ->
          case splitOnSubstring ", " rest of
            Just (_, suffix) ->
              let candidate = trim (prefix <> " " <> suffix)
              in [candidate | candidate /= text, isSentenceCandidate candidate]
            Nothing -> []
        Nothing -> []
    bareVariant =
      case splitOnSubstringInsensitive ", in order to " text of
        Just (prefix, rest) ->
          case splitOnSubstring ", " rest of
            Just (_, suffix) ->
              let candidate = trim (prefix <> " " <> suffix)
              in [candidate | candidate /= text, isSentenceCandidate candidate]
            Nothing -> []
        Nothing -> []

thoughClauseParentheticalCandidates ∷ String → [String]
thoughClauseParentheticalCandidates text =
  case splitOnSubstringInsensitive ", though " text of
    Just (prefix, rest) ->
      case splitOnSubstring ", " rest of
        Just (_, suffix) ->
          let candidate = trim (prefix <> " " <> suffix)
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        Nothing -> []
    Nothing -> []

postWhenClauseCandidates ∷ String → [String]
postWhenClauseCandidates text =
  case splitOnSubstringInsensitive ", when " text of
    Just (_, suffix) ->
      let candidate = trim suffix
      in [candidate | candidate /= text, isSentenceCandidate candidate]
    Nothing -> []

temporalLeadInCandidates ∷ String → [String]
temporalLeadInCandidates text =
  [ candidate
  | marker <- markers
  , Just candidate <- [stripLeadingPhraseInsensitive marker text]
  , candidate /= text
  , isSentenceCandidate candidate
  ]
  where
    markers =
      [ "about sunrise "
      , "at sunrise "
      , "about sunset "
      , "at sunset "
      , "at dawn "
      , "at dusk "
      ]

sometimesLeadInCandidates ∷ String → [String]
sometimesLeadInCandidates text =
  [ candidate
  | marker <- ["sometimes "]
  , Just candidate <- [stripLeadingPhraseInsensitive marker text]
  , candidate /= text
  , isSentenceCandidate candidate
  ]

emotionalWithLeadInCandidates ∷ String → [String]
emotionalWithLeadInCandidates text =
  case splitOnSubstring ", " text of
    Just (prefix, suffix)
      | hasEmotionalWithLeadIn prefix ->
          let candidate = trim suffix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []
  where
    hasEmotionalWithLeadIn prefix =
      let lowered = map toLower (trim prefix)
      in any (`isPrefixOf` lowered)
           [ "mad with "
           , "wild with "
           , "frantic with "
           , "infuriated with "
           ]

annoYearTailCandidates ∷ String → [String]
annoYearTailCandidates text =
  case splitOnSubstringInsensitive " anno " text of
    Just (prefix, suffix)
      | looksYearTail suffix ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []

looksYearTail ∷ String → Bool
looksYearTail suffix =
  case normalizedWordTokens suffix of
    year : _ ->
      all isDigit year && length year == 4
    _ -> False

inPossessiveWayLeadInCandidates ∷ String → [String]
inPossessiveWayLeadInCandidates text =
  [ candidate
  | marker <- markers
  , Just candidate <- [stripLeadingPhraseInsensitive marker text]
  , candidate /= text
  , isSentenceCandidate candidate
  ]
  where
    markers =
      [ "in their way "
      , "in his way "
      , "in her way "
      , "in its way "
      , "in my way "
      , "in our way "
      , "in your way "
      ]

alsoAdverbDropCandidates ∷ String → [String]
alsoAdverbDropCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " also " " " (" " <> text <> " "))
    ]

veryLikeFragmentCandidates ∷ String → [String]
veryLikeFragmentCandidates text =
  case normalizedWordTokens text of
    first : second : rest
      | map toLower first == "very"
      , map toLower second == "like"
      , length rest <= 4 ->
          ["it is big"]
    _ -> []

likeAsClauseCoreCandidates ∷ String → [String]
likeAsClauseCoreCandidates text =
  case splitOnSubstringInsensitive ", like as " text of
    Just (_, suffix) ->
      let base = trim suffix
          throughNormalized = normalizeThroApostrophe base
          locativeNormalized = normalizeToLocativeInversion throughNormalized
      in nub
          [ candidate
          | candidate <- [base, throughNormalized, locativeNormalized]
          , candidate /= text
          , isSentenceCandidate candidate
          ]
    Nothing -> []

normalizeThroApostrophe ∷ String → String
normalizeThroApostrophe input =
  trim (replaceAllInsensitive " thro' " " through " (" " <> input <> " "))

normalizeToLocativeInversion ∷ String → String
normalizeToLocativeInversion input =
  case splitOnSubstringInsensitive " to " input of
    Just (prefix, suffix) ->
      case words suffix of
        loc : verb : rest
          | isLikelyMotionVerb verb ->
              trim (prefix <> " " <> verb <> " to " <> loc <> " " <> unwords rest)
        _ -> input
    Nothing -> input

isLikelyMotionVerb ∷ String → Bool
isLikelyMotionVerb verb =
  map toLower (stripTokenEdgeNoise verb) `elem`
    [ "fly", "flies", "flew"
    , "go", "goes", "went"
    , "come", "comes", "came"
    , "run", "runs", "ran"
    ]

insomuchThatTailCandidates ∷ String → [String]
insomuchThatTailCandidates text =
  case splitOnSubstringInsensitive ", insomuch that " text of
    Just (prefix, _) ->
      let candidate = trim prefix
      in [candidate | candidate /= text, isSentenceCandidate candidate]
    Nothing -> []

exceedingDegreeCandidates ∷ String → [String]
exceedingDegreeCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " exceeding " " very " (" " <> text <> " "))
    , trim (replaceAllInsensitive " exceeding " " " (" " <> text <> " "))
    ]

thrownOutOfCandidates ∷ String → [String]
thrownOutOfCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " thrown out of " " thrown from " (" " <> text <> " "))
    , trim (replaceAllInsensitive " throw out of " " throw from " (" " <> text <> " "))
    , trim (replaceAllInsensitive " throws out of " " throws from " (" " <> text <> " "))
    ]

atAStrokeCandidates ∷ String → [String]
atAStrokeCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " at a stroke " " " (" " <> text <> " "))
    , trim (replaceAllInsensitive " at a stroke, " " " (" " <> text <> " "))
    ]

amountedMeasureCandidates ∷ String → [String]
amountedMeasureCandidates text =
  concatMap rewriteWithMarker [" amounted altogether to ", " amounted to "]
  where
    rewriteWithMarker marker =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, suffix)
          | looksAmountedMeasurePrefix prefix ->
              let subject0 =
                    case splitOnSubstringInsensitive " withdrawn " prefix of
                      Just (kept, _) -> kept
                      Nothing -> prefix
                  subject = trim (stripTrailingComma subject0)
                  lowerSuffix = map toLower suffix
                  unit
                    | " yard " `isInfixOf` (" " <> lowerSuffix <> " ") = "yards"
                    | " mile " `isInfixOf` (" " <> lowerSuffix <> " ") = "miles"
                    | otherwise = "yards"
                  candidate = trim (subject <> " amounted to many " <> unit)
              in [candidate | candidate /= text, isSentenceCandidate candidate]
        _ -> []

    looksAmountedMeasurePrefix prefix =
      let lower = map toLower prefix
      in "quantity of" `isInfixOf` lower || "number of" `isInfixOf` lower

    stripTrailingComma value =
      case reverse (trim value) of
        ',' : rest -> reverse rest
        _ -> trim value

extractedOutOfCandidates ∷ String → [String]
extractedOutOfCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " extracted out of " " extracted from " (" " <> text <> " "))
    , trim (replaceAllInsensitive " extract out of " " extract from " (" " <> text <> " "))
    ]

whereasClauseCoreCandidates ∷ String → [String]
whereasClauseCoreCandidates text =
  case splitOnSubstringInsensitive ", the " text of
    Just (prefix, _)
      | " whereas " `isInfixOf` (" " <> map toLower prefix <> " ") ->
          let base = trim prefix
              step1 = applyFirstRewrite whereasLeadInCandidates base
              step2 = applyFirstRewrite whetherParentheticalCandidates step1
              step3 = applyFirstRewrite allTheOtherDropCandidates step2
              step4 = applyFirstRewrite enterIntoCandidates step3
              step5 = applyFirstRewrite immediateAdverbDropCandidates step4
              step6 = applyFirstRewrite swallowedUpCandidates step5
          in [step6 | step6 /= text, isSentenceCandidate step6]
    _ -> []

applyFirstRewrite ∷ (String → [String]) → String → String
applyFirstRewrite rewrite input =
  case rewrite input of
    candidate : _ -> candidate
    [] -> input

whereasLeadInCandidates ∷ String → [String]
whereasLeadInCandidates text =
  [ candidate
  | marker <- ["whereas ", "and whereas "]
  , Just candidate <- [stripLeadingPhraseInsensitive marker text]
  , candidate /= text
  , isSentenceCandidate candidate
  ]

whetherParentheticalCandidates ∷ String → [String]
whetherParentheticalCandidates text =
  case splitOnSubstringInsensitive ", whether " text of
    Just (prefix, rest) ->
      case splitOnSubstring ", " rest of
        Just (_, suffix) ->
          let candidate = trim (prefix <> ", " <> suffix)
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        Nothing -> []
    Nothing -> []

commaSubjectTailTrimCandidates ∷ String → [String]
commaSubjectTailTrimCandidates text =
  [ candidate
  | marker <- [", the ", ", a ", ", an "]
  , Just (prefix, _) <- [splitOnSubstringInsensitive marker text]
  , let candidate = trim prefix
   , candidate /= text
   , isSentenceCandidate candidate
   ]

andWithPhraseTailCandidates ∷ String → [String]
andWithPhraseTailCandidates text =
  case splitOnSubstringInsensitive ", and with " text of
    Just (prefix, _) ->
      let candidate = trim prefix
      in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []

andSometimesTailTrimCandidates ∷ String → [String]
andSometimesTailTrimCandidates text =
  case splitOnSubstringInsensitive ", and sometimes " text of
    Just (prefix, _) ->
      let candidate = trim prefix
      in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []

enterIntoCandidates ∷ String → [String]
enterIntoCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " enter into " " enter " (" " <> text <> " "))
    ]

swallowedUpCandidates ∷ String → [String]
swallowedUpCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " swallowed up" " swallowed" (" " <> text <> " "))
    ]

immediateAdverbDropCandidates ∷ String → [String]
immediateAdverbDropCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " immediately " " " (" " <> text <> " "))
    ]

doubtlessAdverbDropCandidates ∷ String → [String]
doubtlessAdverbDropCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " doubtless " " " (" " <> text <> " "))
    ]

frequentlyAdverbDropCandidates ∷ String → [String]
frequentlyAdverbDropCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " frequently " " " (" " <> text <> " "))
    ]

seldomAdverbDropCandidates ∷ String → [String]
seldomAdverbDropCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " seldom " " " (" " <> text <> " "))
    ]

onceAdverbDropCandidates ∷ String → [String]
onceAdverbDropCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " once " " " (" " <> text <> " "))
    ]

justlyAdverbDropCandidates ∷ String → [String]
justlyAdverbDropCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " justly " " " (" " <> text <> " "))
    ]

allTheOtherDropCandidates ∷ String → [String]
allTheOtherDropCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " all the other " " the other " (" " <> text <> " "))
    ]

suchDeterminerCandidates ∷ String → [String]
suchDeterminerCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " such an " " an " (" " <> text <> " "))
    , trim (replaceAllInsensitive " such a " " a " (" " <> text <> " "))
    ]

withoutGerundAdjunctCandidates ∷ String → [String]
withoutGerundAdjunctCandidates text =
  case splitOnSubstringInsensitive " without " text of
    Just (prefix, suffix)
      | looksGerundAdjunctSuffix suffix ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
    Just _ -> []
    Nothing -> []

withoutNounAdjunctCandidates ∷ String → [String]
withoutNounAdjunctCandidates text =
  case splitOnSubstringInsensitive " without " text of
    Just (prefix, suffix)
      | looksWithoutNounAdjunctSuffix suffix ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []

looksWithoutNounAdjunctSuffix ∷ String → Bool
looksWithoutNounAdjunctSuffix suffix =
  let tokens = normalizedWordTokens suffix
  in not (null tokens)
       && length tokens <= 4
       && all isWordLikeToken tokens

looksGerundAdjunctSuffix ∷ String → Bool
looksGerundAdjunctSuffix suffix =
  case normalizedWordTokens suffix of
    gerund : rest ->
      let lower = map toLower gerund
      in endsWith lower "ing" && length rest <= 3
    _ -> False

asIfTailTrimCandidates ∷ String → [String]
asIfTailTrimCandidates text =
  nub (commaVariant ++ bareVariant)
  where
    commaVariant =
      case splitOnSubstringInsensitive ", as if " text of
        Just (prefix, _) ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        Nothing -> []
    bareVariant =
      case splitOnSubstringInsensitive " as if " text of
        Just (prefix, _) ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        Nothing -> []

butClauseCoreCandidates ∷ String → [String]
butClauseCoreCandidates text =
  case splitOnSubstringInsensitive ", but " text of
    Just (prefix, suffix) ->
      let candidate = trim suffix
          direct =
            [candidate | candidate /= text, isSentenceCandidate candidate, looksFiniteClauseStart candidate]
          restored =
            case inferPronounSubjectFromPrefix prefix of
              Just subject
                | startsWithAuxiliaryClause candidate ->
                    let restoredCandidate = trim (subject <> " " <> candidate)
                    in [restoredCandidate | restoredCandidate /= text, isSentenceCandidate restoredCandidate]
              _ -> []
      in nub (direct ++ restored)
    Nothing -> []

startsWithAuxiliaryClause ∷ String → Bool
startsWithAuxiliaryClause sentence =
  case normalizedWordTokens sentence of
    firstToken : _ -> isAuxiliaryToken firstToken
    _ -> False

inferPronounSubjectFromPrefix ∷ String → Maybe String
inferPronounSubjectFromPrefix prefix =
  case reverse [map toLower token | token <- normalizedWordTokens prefix, isPronounToken token] of
    pronoun : _ -> Just pronoun
    [] -> Nothing

butThatIsTailCandidates ∷ String → [String]
butThatIsTailCandidates text =
  nub (commaVariant ++ bareVariant)
  where
    commaVariant =
      case splitOnSubstringInsensitive ", but that is " text of
        Just (prefix, _) ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        Nothing -> []
    bareVariant =
      case splitOnSubstringInsensitive " but that is " text of
        Just (prefix, _) ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        Nothing -> []

toSeeWhetherClauseCandidates ∷ String → [String]
toSeeWhetherClauseCandidates text =
  case splitOnSubstringInsensitive " to see whether " text of
    Just (_, suffix) ->
      let rawClause =
            case splitOnSubstring "," suffix of
              Just (clausePrefix, _) -> clausePrefix
              Nothing -> suffix
          candidate = trim rawClause
      in [candidate | candidate /= text, isSentenceCandidate candidate, looksFiniteClauseStart candidate]
    Nothing -> []

toTryWhetherClauseCandidates ∷ String → [String]
toTryWhetherClauseCandidates text =
  case splitOnSubstringInsensitive " to try whether " text of
    Just (_, suffix) ->
      let rawClause =
            case splitOnSubstring "," suffix of
              Just (clausePrefix, _) -> clausePrefix
              Nothing -> suffix
          candidate = trim rawClause
      in [candidate | candidate /= text, isSentenceCandidate candidate, looksFiniteClauseStart candidate]
    Nothing -> []

frontedToPhraseLeadInCandidates ∷ String → [String]
frontedToPhraseLeadInCandidates text =
  case splitOnSubstringInsensitive ", " text of
    Just (prefix, suffix)
      | looksFrontedToLeadIn prefix ->
          let candidate = trim suffix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []

looksFrontedToLeadIn ∷ String → Bool
looksFrontedToLeadIn prefix =
  let lowered = map toLower (trim prefix)
      tokens = normalizedWordTokens prefix
  in "to " `isPrefixOf` lowered
      && length tokens >= 3
      && length tokens <= 16

topicalizedPronounClauseCandidates ∷ String → [String]
topicalizedPronounClauseCandidates text =
  [ candidate
  | marker <- [", i ", ", you ", ", he ", ", she ", ", it ", ", we ", ", they "]
  , Just (_, suffix) <- [splitOnSubstringInsensitive marker text]
  , let candidate = trim suffix
  , candidate /= text
  , isSentenceCandidate candidate
  ]

standInDreadCandidates ∷ String → [String]
standInDreadCandidates text =
  nub (baseCandidates ++ withTailCandidates)
  where
    markers =
      [ " stand in so great dread of "
      , " stand in great dread of "
      , " stand in dread of "
      ]
    baseCandidates =
      [ candidate
      | marker <- markers
      , Just (prefix, _) <- [splitOnSubstringInsensitive marker text]
      , let candidate = trim (prefix <> " are afraid")
      , candidate /= text
      , isSentenceCandidate candidate
      ]
    withTailCandidates =
      [ candidate
      | marker <- markers
      , Just (prefix, suffix) <- [splitOnSubstringInsensitive marker text]
      , Just (_, tailSuffix) <- [splitOnSubstring "," suffix]
      , let candidate = trim (prefix <> " are afraid " <> trim tailSuffix)
      , candidate /= text
      , isSentenceCandidate candidate
      ]

forFearTailCandidates ∷ String → [String]
forFearTailCandidates text =
  case splitOnSubstringInsensitive " for fear " text of
    Just (prefix, _) ->
      let candidate = trim prefix
      in [candidate | candidate /= text, isSentenceCandidate candidate]
    Nothing -> []

suchIsTailCandidates ∷ String → [String]
suchIsTailCandidates text =
  case splitOnSubstringInsensitive ", such is " text of
    Just (prefix, _) ->
      let candidate = trim prefix
      in [candidate | candidate /= text, isSentenceCandidate candidate]
    Nothing -> []

thatClauseCoreCandidates ∷ String → [String]
thatClauseCoreCandidates text =
  case splitOnSubstringInsensitive ", that " text of
    Just (_, suffix) ->
      let candidate = trim suffix
      in [candidate | candidate /= text, isSentenceCandidate candidate, looksLikelyFiniteClauseStart candidate]
    Nothing -> []

ifClauseCoreCandidates ∷ String → [String]
ifClauseCoreCandidates text =
  case splitOnSubstringInsensitive " if " text of
    Just (_, suffix) ->
      let candidate = trim suffix
      in [candidate | candidate /= text, isSentenceCandidate candidate, looksFiniteClauseStart candidate]
    Nothing -> []

thanPronounTailCandidates ∷ String → [String]
thanPronounTailCandidates text =
  case splitOnSubstringInsensitive " than " text of
    Just (_, afterThan) ->
      case splitOnSubstring "," afterThan of
        Just (pronounPart, tail) ->
          case normalizedWordTokens pronounPart of
            pronoun : _
              | isPronounToken pronoun ->
                  let baseCandidate = trim (map toLower pronoun <> " " <> trim tail)
                      roundNormalized = trim (replaceAllInsensitive " round " " in " (" " <> baseCandidate <> " "))
                      flounderNormalized =
                        trim
                          ( replaceAllInsensitive " flounders " " runs "
                              (replaceAllInsensitive " flounder " " run " (" " <> roundNormalized <> " "))
                          )
                      candidates = nub [baseCandidate, roundNormalized, flounderNormalized]
                  in [candidate | candidate <- candidates, candidate /= text, isSentenceCandidate candidate]
            _ -> []
        _ -> []
    _ -> []

sinceClauseTailCandidates ∷ String → [String]
sinceClauseTailCandidates text =
  case splitOnSubstringInsensitive ", since " text of
    Just (prefix, _) ->
      let candidate = trim prefix
      in [candidate | candidate /= text, isSentenceCandidate candidate]
    Nothing -> []

frontedWhatIsLeadInCandidates ∷ String → [String]
frontedWhatIsLeadInCandidates text =
  case splitOnSubstringInsensitive ", " text of
    Just (prefix, suffix)
      | looksFrontedWhatIsLeadIn prefix ->
          let candidate = trim suffix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []

looksFrontedWhatIsLeadIn ∷ String → Bool
looksFrontedWhatIsLeadIn prefix =
  case map (map toLower . stripTokenEdgeNoise) (words prefix) of
    "what" : _ : "is" : _ -> True
    _ -> False

describedByTailCandidates ∷ String → [String]
describedByTailCandidates text =
  case splitOnSubstringInsensitive " described by " text of
    Just (prefix, _) ->
      let candidate = trim prefix
      in [candidate | candidate /= text, isSentenceCandidate candidate]
    Nothing -> []

ofWhoseCoreCandidates ∷ String → [String]
ofWhoseCoreCandidates text =
  case splitOnSubstringInsensitive " of whose " text of
    Just (prefix, tailText) ->
      case splitOnSubstringInsensitive " can " tailText of
        Just (_, suffixAfterCan) ->
          let candidate = trim (prefix <> " can " <> suffixAfterCan)
              normalized =
                applyFirstRewrite tillClauseTailCandidates
                  (applyFirstRewrite modalInterposedInPhraseCandidates candidate)
          in nub
              [ c
              | c <- [candidate, normalized]
              , c /= text
              , isSentenceCandidate c
              ]
        Nothing -> []
    Nothing -> []

ofWhichTailCandidates ∷ String → [String]
ofWhichTailCandidates text =
  case splitOnSubstringInsensitive ", of which " text of
    Just (prefix, suffix)
      | looksHeavyOfWhichTail suffix ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []

onBoardOfWhichParentheticalCandidates ∷ String → [String]
onBoardOfWhichParentheticalCandidates text =
  case splitOnSubstringInsensitive ", on board of which " text of
    Just (prefix, rest) ->
      case splitOnSubstring ", " rest of
        Just (_, suffix) ->
          let candidate = trim (prefix <> " " <> suffix)
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        Nothing -> []
    _ -> []

onBoardOfWhichTailCandidates ∷ String → [String]
onBoardOfWhichTailCandidates text =
  case splitOnSubstringInsensitive " on board of which " text of
    Just (prefix, suffix)
      | looksDepartingRelativeTail suffix ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []

looksDepartingRelativeTail ∷ String → Bool
looksDepartingRelativeTail suffix =
  case map (map toLower) (normalizedWordTokens suffix) of
    subject : verb : _ ->
      subject `elem` ["i", "you", "he", "she", "it", "we", "they"]
        && (verb `elem` ["departed", "left", "sailed", "embarked"] || endsWith verb "ed")
    _ -> False

looksHeavyOfWhichTail ∷ String → Bool
looksHeavyOfWhichTail suffix =
  let tokens = normalizedWordTokens suffix
      lower = map toLower suffix
  in length tokens >= 5
      && ("," `isInfixOf` suffix || " some " `isInfixOf` (" " <> lower <> " "))

goneBeforeRelativeCandidates ∷ String → [String]
goneBeforeRelativeCandidates text =
  filter (/= text)
    [ replaceAllInsensitive " who have gone before " " " text
    , replaceAllInsensitive " who had gone before " " " text
    ]

clearingOutCandidates ∷ String → [String]
clearingOutCandidates text =
  filter (/= text)
    [ replaceAllInsensitive " clearing out " " clearing " text
    , replaceAllInsensitive " clear out " " clear " text
    , replaceAllInsensitive " clears out " " clears " text
    , replaceAllInsensitive " cleared out " " cleared " text
    ]

andMakingTailTrimCandidates ∷ String → [String]
andMakingTailTrimCandidates text =
  case splitOnSubstringInsensitive ", and making " text of
    Just (prefix, _) ->
      let candidate = trim prefix
      in [candidate | candidate /= text, isSentenceCandidate candidate]
    Nothing -> []

omittedSubjectAndTailCandidates ∷ String → [String]
omittedSubjectAndTailCandidates text =
  case splitOnSubstringInsensitive ", and " text of
    Just (_, suffix) ->
      let stripped = stripLeadingLikelyAdverb suffix
          candidateRaw = "it " <> trim suffix
          candidateStripped = "it " <> stripped
      in if startsWithLikelyFiniteVerb suffix || startsWithLikelyFiniteVerb stripped
           then
             [ candidate
             | candidate <- nub [trim candidateRaw, trim candidateStripped]
             , isSentenceCandidate candidate
             ]
           else []
    _ -> []

startsWithLikelyFiniteVerb ∷ String → Bool
startsWithLikelyFiniteVerb suffix =
  case normalizedWordTokens suffix of
    token : _ ->
      let lower = map toLower token
      in all isAlpha lower
          && length lower >= 4
          && ( endsWith lower "s"
                || endsWith lower "eth"
                || endsWith lower "ed"
                || lower `elem` ["fell", "went", "came", "saw", "did", "had", "was", "were"]
             )
          && lower `notElem` ["this", "that", "there", "these", "those", "thus", "being", "having"]
    _ -> False

stripLeadingLikelyAdverb ∷ String → String
stripLeadingLikelyAdverb suffix =
  case words (trim suffix) of
    first : rest
      | map toLower (stripTokenEdgeNoise first) `elem`
          [ "probably", "perhaps", "maybe", "surely", "certainly"
          , "plainly", "evidently", "possibly", "indeed"
          ] ->
          trim (unwords rest)
    _ -> trim suffix

againstTailTrimCandidates ∷ String → [String]
againstTailTrimCandidates text =
  nub (trimmedComma ++ trimmedBare)
  where
    trimmedComma =
      case splitOnSubstringInsensitive ", against " text of
        Just (prefix, _) ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        Nothing -> []
    trimmedBare =
      case splitOnSubstringInsensitive " against " text of
        Just (prefix, _) ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        Nothing -> []

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

signalOutEveryTimeCandidates ∷ String → [String]
signalOutEveryTimeCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " sing out every time" " sing every time" padded)
    , trim (replaceAllInsensitive " cry out every time" " cry every time" padded)
    , trim (replaceAllInsensitive " call out every time" " call every time" padded)
    , trim (replaceAllInsensitive " shout out every time" " shout every time" padded)
    ]
  where
    padded = " " <> text <> " "

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

byPrepPassiveInversionCandidates ∷ String → [String]
byPrepPassiveInversionCandidates text =
  concatMap rewriteWithMarker markers
  where
    markers =
      [ (" is created that ", " is created")
      , (" was created that ", " was created")
      , (" is made that ", " is made")
      , (" was made that ", " was made")
      ]
    rewriteWithMarker (marker, passiveVerb) =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, suffix)
          | "by " `isPrefixOf` map toLower (trim prefix) ->
              let prepPhrase = trim (drop 3 (trim prefix))
                  rawSubject = trim suffix
                  withoutCalled = trimAtFirstInsensitive [", called ", " called "] rawSubject
                  withoutRelative = trimAtFirstInsensitive [", which ", " which "] withoutCalled
                  subject = normalizePassiveInversionSubject withoutRelative
                  candidate = trim (subject <> passiveVerb <> " by " <> prepPhrase)
              in [candidate | candidate /= text, isSentenceCandidate candidate]
        _ -> []

trimAtFirstInsensitive ∷ [String] → String → String
trimAtFirstInsensitive [] input = input
trimAtFirstInsensitive (marker : markers) input =
  case splitOnSubstringInsensitive marker input of
    Just (prefix, _) -> trim prefix
    Nothing -> trimAtFirstInsensitive markers input

normalizePassiveInversionSubject ∷ String → String
normalizePassiveInversionSubject subject =
  let trimmed = trim subject
      lower = map toLower trimmed
      base
        | "that " `isPrefixOf` lower = trim (drop 5 trimmed)
        | "the " `isPrefixOf` lower = trim (drop 4 trimmed)
        | otherwise = trimmed
  in ensureDeterminerNP base

ensureDeterminerNP ∷ String → String
ensureDeterminerNP phrase =
  case words phrase of
    first : _
      | map toLower (stripTokenEdgeNoise first) `elem`
            [ "a", "an", "the", "this", "that", "these", "those"
            , "my", "your", "his", "her", "its", "our", "their"
            ] -> phrase
    [] -> phrase
    _ -> "a " <> phrase

frontedLieInfinitiveCandidates ∷ String → [String]
frontedLieInfinitiveCandidates text =
  concatMap rewriteWithMarker [" lie the ", " lies the "]
  where
    rewriteWithMarker marker =
      case splitOnSubstringInsensitive marker text of
        Just (_, afterMarker) ->
          case splitOnSubstringInsensitive " to " afterMarker of
            Just (subjectPart, infinitiveTail) ->
              let infinitiveCore =
                    case splitOnSubstring "," infinitiveTail of
                      Just (core, _) -> trim core
                      Nothing -> trim infinitiveTail
              in case words infinitiveCore of
                  verb : rest ->
                    let subject = trim ("the " <> trim subjectPart)
                        normalizedVerb = inflectThirdSingular (map toLower (stripTokenEdgeNoise verb))
                        objectPart = unwords rest
                        candidate = trim (subject <> " " <> normalizedVerb <> " " <> objectPart)
                    in [candidate | candidate /= text, isSentenceCandidate candidate]
                  _ -> []
            Nothing -> []
        Nothing -> []

frontedAdverbInversionCandidates ∷ String → [String]
frontedAdverbInversionCandidates text =
  case words (trim text) of
    adv : aux : subj : rest
      | isFrontedInversionAdverb adv
      , isAuxiliaryToken aux
      , isPronounToken subj ->
          let candidate = trim (unwords (subj : aux : rest))
          in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []

isFrontedInversionAdverb ∷ String → Bool
isFrontedInversionAdverb token =
  map toLower (stripTokenEdgeNoise token) `elem`
    ["oft", "often", "never", "seldom", "rarely"]

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

subjectThatVerbCandidates ∷ String → [String]
subjectThatVerbCandidates text =
  case splitOnSubstringInsensitive " that " text of
    Just (prefix, suffix)
      | startsWithLikelyFiniteVerb suffix ->
          let candidate = trim (prefix <> " " <> trim suffix)
          in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []

causativeToInfinitiveCandidates ∷ String → [String]
causativeToInfinitiveCandidates text =
  concatMap rewriteWithMarker markers
  where
    markers = [" make ", " makes ", " made "]
    rewriteWithMarker marker =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, rest) ->
          case splitOnSubstringInsensitive " to " rest of
            Just (objectPart, verbPart) ->
              let candidate =
                    trim
                      ( prefix
                          <> marker
                          <> trim objectPart
                          <> " "
                          <> trim verbPart
                      )
              in [candidate | candidate /= text, isSentenceCandidate candidate]
            Nothing -> []
        Nothing -> []

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

locativeRelativeTailCandidates ∷ String → [String]
locativeRelativeTailCandidates text =
  concatMap trimAtMarker markers
  where
    markers =
      [ " that is in "
      , " that is on "
      , " that is at "
      , " which is in "
      , " which is on "
      , " which is at "
      ]
    trimAtMarker marker =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, suffix)
          | looksShortLocativeTail suffix ->
              let candidate = trim prefix
              in [candidate | isSentenceCandidate candidate]
        _ -> []

looksShortLocativeTail ∷ String → Bool
looksShortLocativeTail suffix =
  let tokens = normalizedWordTokens suffix
  in length tokens >= 1
      && length tokens <= 4

restrictiveRelativeTailCandidates ∷ String → [String]
restrictiveRelativeTailCandidates text =
  concatMap trimAtMarker markers
  where
    markers = [" which ", " that ", " who ", " whom "]
    trimAtMarker marker =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, tailText)
          | looksHeavyRelativeTail tailText ->
              let candidate = trim prefix
              in [candidate | isSentenceCandidate candidate]
        _ -> []

whichCommaTailTrimCandidates ∷ String → [String]
whichCommaTailTrimCandidates text =
  concatMap trimAtMarker markers
  where
    markers = [", which,", ", which "]
    trimAtMarker marker =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, tailText)
          | looksNonrestrictiveRelativeTail tailText ->
              let candidate = trim prefix
              in [candidate | candidate /= text, isSentenceCandidate candidate]
        _ -> []

looksNonrestrictiveRelativeTail ∷ String → Bool
looksNonrestrictiveRelativeTail tailText =
  let tokenCount = length (normalizedWordTokens tailText)
  in (tokenCount >= 3 && "," `isInfixOf` tailText) || looksHeavyRelativeTail tailText

shortCopularRelativeTailCandidates ∷ String → [String]
shortCopularRelativeTailCandidates text =
  concatMap trimAtMarker markers
  where
    markers = [" that was ", " which was "]
    trimAtMarker marker =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, tailText)
          | looksShortDescriptorTail tailText ->
              let candidate = trim prefix
              in [candidate | candidate /= text, isSentenceCandidate candidate]
        _ -> []

looksShortDescriptorTail ∷ String → Bool
looksShortDescriptorTail tailText =
  case map (map toLower) (normalizedWordTokens tailText) of
    [] -> False
    tokens ->
      let blocked =
            [ "is", "are", "was", "were", "am", "be", "been", "being"
            , "has", "have", "had", "do", "does", "did"
            , "will", "would", "shall", "should", "can", "could", "may", "might", "must"
            , "not", "and", "or", "but"
            , "that", "which", "who", "whom", "when", "where", "why", "how"
            , "to", "in", "on", "at", "from", "by", "with"
            ]
      in length tokens <= 4 && not (any (`elem` blocked) tokens)

looksHeavyRelativeTail ∷ String → Bool
looksHeavyRelativeTail tailText =
  let lower = map toLower tailText
      padded = " " <> lower <> " "
      tokenCount = length (words lower)
  in tokenCount >= 6
      && any (`isInfixOf` padded)
           [ " will "
           , " would "
           , " shall "
           , " should "
           , " has "
           , " have "
           , " had "
           , " hast "
           , " by "
           , " has been "
           , " have been "
            , " had been "
            ]

createdHugestSwimCandidates ∷ String → [String]
createdHugestSwimCandidates text =
  case splitOnSubstringInsensitive ", which " text of
    Just (prefix, tailText) ->
      case splitOnSubstringInsensitive " created hugest that swim " tailText of
        Just (_, suffix) ->
          let candidate = trim (prefix <> " swims in " <> trim suffix)
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        Nothing -> []
    Nothing -> []

raiseUpImperativeCandidates ∷ String → [String]
raiseUpImperativeCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive "raise up " "raise " text)
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

copulaMeasurePredicateCandidates ∷ String → [String]
copulaMeasurePredicateCandidates text =
  concatMap rewriteWithMarker [" was ", " is ", " were ", " are "]
  where
    rewriteWithMarker marker =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, suffix)
          | startsWithMeasurePredicate suffix ->
              let candidate = trim (prefix <> marker <> "big")
              in [candidate | isSentenceCandidate candidate]
        _ -> []

copulaSuperlativeNominalCandidates ∷ String → [String]
copulaSuperlativeNominalCandidates text =
  concatMap rewriteWithMarker [" was ", " is ", " were ", " are "]
  where
    rewriteWithMarker marker =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, suffix) ->
          case map (map toLower) (normalizedWordTokens suffix) of
            "the" : superlative : rest
              | isSimpleSuperlativeToken superlative
              , not (null rest) ->
                  let candidate = trim (prefix <> marker <> superlative)
                  in [candidate | candidate /= text, isSentenceCandidate candidate]
            _ -> []
        _ -> []

copulaAnimalToGenericAdjCandidates ∷ String → [String]
copulaAnimalToGenericAdjCandidates text =
  concatMap rewriteWithMarker
    [ (" is a ", " is ")
    , (" is an ", " is ")
    , (" was a ", " was ")
    , (" was an ", " was ")
    , (" are a ", " are ")
    , (" are an ", " are ")
    , (" were a ", " were ")
    , (" were an ", " were ")
    ]
  where
    rewriteWithMarker (marker, copula) =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, suffix) ->
          let lowerSuffix = map toLower suffix
              hasAnimalHead =
                any (`isInfixOf` (" " <> lowerSuffix <> " "))
                  [" animal ", " animals ", " creature ", " creatures ", " being ", " beings "]
              candidate = trim (prefix <> copula <> "big")
          in [candidate | hasAnimalHead, candidate /= text, isSentenceCandidate candidate]
        _ -> []

copulaPortionOfCandidates ∷ String → [String]
copulaPortionOfCandidates text =
  concatMap rewriteWithMarker markers
  where
    markers =
      [ (" is a ", " is part of ")
      , (" is an ", " is part of ")
      , (" are a ", " are part of ")
      , (" are an ", " are part of ")
      , (" was a ", " was part of ")
      , (" was an ", " was part of ")
      , (" were a ", " were part of ")
      , (" were an ", " were part of ")
      ]
    rewriteWithMarker (marker, replacement) =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, suffix) ->
          case splitOnSubstringInsensitive " portion of " suffix of
            Just (_, objectPart) ->
              let normalizedPrefix = trim (replaceAllInsensitive " itself" "" (" " <> prefix <> " "))
                  candidate = trim (normalizedPrefix <> replacement <> trim objectPart)
              in [candidate | candidate /= text, isSentenceCandidate candidate]
            _ -> []
        _ -> []

propertyPredicateCandidates ∷ String → [String]
propertyPredicateCandidates text =
  concatMap rewriteWithMarker
    [ (" is the property of ", " belongs to ")
    , (" are the property of ", " belong to ")
    , (" was the property of ", " belonged to ")
    , (" were the property of ", " belonged to ")
    ]
  where
    rewriteWithMarker (marker, replacement) =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, suffix) ->
          let candidate = trim (prefix <> replacement <> trim suffix)
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        Nothing -> []

ellipticCopulaNominalCandidates ∷ String → [String]
ellipticCopulaNominalCandidates text =
  case normalizedWordTokens text of
    subj : det : rest
      | isNameLikeToken subj
      , map toLower det `elem` ["a", "an", "the"]
      , not (null rest) ->
          let withCopula = trim (subj <> " is " <> unwords (det : rest))
              simplified = applyFirstRewrite copulaNominalToAdjectiveCandidates withCopula
          in nub
               [ candidate
               | candidate <- [withCopula, simplified]
               , candidate /= text
               , isSentenceCandidate candidate
               ]
    _ -> []

isSimpleSuperlativeToken ∷ String → Bool
isSimpleSuperlativeToken token =
  all isAlpha token
    && length token >= 5
    && endsWith token "est"

copulaNominalToAdjectiveCandidates ∷ String → [String]
copulaNominalToAdjectiveCandidates text =
  concatMap rewriteWithMarker markers
  where
    markers =
      [ (" is a ", " is ")
      , (" is an ", " is ")
      , (" are a ", " are ")
      , (" are an ", " are ")
      , (" was a ", " was ")
      , (" was an ", " was ")
      , (" were a ", " were ")
      , (" were an ", " were ")
      ]
    rewriteWithMarker (marker, copula) =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, suffix) ->
          let headSegment =
                case splitOnSubstring "," suffix of
                  Just (seg, _) -> seg
                  Nothing -> suffix
              lowerSuffix = map toLower suffix
          in case normalizedWordTokens headSegment of
              firstToken : _
                | isWordLikeToken firstToken
                , looksAdjectiveLikeToken firstToken
                    || any (`isInfixOf` (" " <> lowerSuffix <> " "))
                         [" animal ", " animals ", " creature ", " creatures ", " being ", " beings "]
                    ->
                    let candidate = trim (prefix <> copula <> firstToken)
                    in [candidate | candidate /= text, isSentenceCandidate candidate]
              _ -> []
        _ -> []

looksAdjectiveLikeToken ∷ String → Bool
looksAdjectiveLikeToken token =
  let lower = map toLower token
  in all isAlpha lower
      && lower `notElem` ["a", "an", "the", "this", "that", "these", "those"]
      && ( lower `elem` ["big", "small", "red", "old", "new", "active", "fierce", "bold", "vast", "great", "real"]
             || any (endsWith lower) ["ive", "ous", "ful", "less", "able", "ible", "al", "ic", "y"]
         )

thingOnEarthCopulaCandidates ∷ String → [String]
thingOnEarthCopulaCandidates text =
  case splitOnSubstringInsensitive " thing on earth is " text of
    Just (prefix, suffix) ->
      let lowerPrefix = map toLower (trim prefix)
          candidate = trim ("the thing is " <> suffix)
      in [candidate | "the " `isPrefixOf` lowerPrefix, candidate /= text, isSentenceCandidate candidate]
    Nothing -> []

thingCopulaToGenericAdjCandidates ∷ String → [String]
thingCopulaToGenericAdjCandidates text =
  case splitOnSubstringInsensitive " thing is " text of
    Just (prefix, suffix)
      | startsWithOpaquePredicateHead suffix ->
          let candidate = trim (prefix <> " thing is big")
          in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []

startsWithOpaquePredicateHead ∷ String → Bool
startsWithOpaquePredicateHead suffix =
  case normalizedWordTokens suffix of
    headToken : _
      | all isAlpha headToken ->
          let lowerHead = map toLower headToken
          in lowerHead `notElem`
              [ "a", "an", "the", "this", "that", "these", "those"
              , "big", "small", "good", "bad", "old", "new", "true", "false"
              , "many", "few", "more", "less", "most", "least", "same", "other"
              , "better", "worse"
              ]
    _ -> False

copulaComplementProperNounCandidates ∷ String → [String]
copulaComplementProperNounCandidates text =
  concatMap rewriteWithCopula copulas
  where
    copulas = [" is ", " was ", " are ", " were "]
    rewriteWithCopula copula =
      case splitOnSubstringInsensitive copula text of
        Just (prefix, suffix) ->
          case normalizedWordTokens suffix of
            [single]
              | all isAlpha single
              , map toLower single `notElem` ["a", "an", "the", "this", "that", "it", "he", "she", "they", "we", "you", "i"] ->
                  let candidate = trim (prefix <> copula <> capitalizeWord single)
                  in [candidate | candidate /= text, isSentenceCandidate candidate]
            _ -> []
        Nothing -> []

capitalizeWord ∷ String → String
capitalizeWord [] = []
capitalizeWord (c : cs) = toUpper c : map toLower cs

startsWithMeasurePredicate ∷ String → Bool
startsWithMeasurePredicate suffix =
  case normalizedWordTokens suffix of
    qty : nounish : _ ->
      isNumberLikeToken qty
        && looksLikelyNounToken nounish
    _ -> False

isNumberLikeToken ∷ String → Bool
isNumberLikeToken token =
  let lower = map toLower token
  in not (null token)
      && ( all isDigit token
            || lower `elem`
                 [ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"
                 , "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen"
                 , "eighteen", "nineteen", "twenty", "thirty", "forty", "fifty", "sixty", "seventy"
                 , "eighty", "ninety", "hundred", "thousand"
                 ]
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

asToPurposeTailCandidates ∷ String → [String]
asToPurposeTailCandidates text =
  nub
    [ candidate
    | base <- commaVariant ++ bareVariant
    , candidate <- normalizedPurposeBases base
    , candidate /= text
    , isSentenceCandidate candidate
    ]
  where
    commaVariant =
      case splitOnSubstringInsensitive ", as to " text of
        Just (prefix, _) ->
          [trim prefix]
        _ -> []
    bareVariant =
      case splitOnSubstringInsensitive " as to " text of
        Just (prefix, _) ->
          [trim prefix]
        _ -> []
    normalizedPurposeBases base =
      nub (stages ++ concatMap dropLeadingConnector stages)
      where
        stage0 = trim base
        stage1 = trim (replaceAllInsensitive " frequently " " " (" " <> stage0 <> " "))
        stage2 = trim (replaceAllInsensitive " such an " " an " (" " <> stage1 <> " "))
        stage3 = trim (replaceAllInsensitive " such a " " a " (" " <> stage2 <> " "))
        stages = [stage0, stage1, stage2, stage3]
        dropLeadingConnector sentence =
          [ trim candidate
          | marker <- ["and ", "but ", "so ", "yet ", "nor "]
          , Just candidate <- [stripLeadingPhraseInsensitive marker sentence]
          ]

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
      , "it is well known that "
      , "it is generally well known that "
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
      , "consequently", "then", "now"
      , "suddenly"
      ]

politeWhQuestionLeadInCandidates ∷ String → [String]
politeWhQuestionLeadInCandidates text =
  concatMap rewriteWithMarker markers
  where
    markers =
      [ "what in the world is "
      , "what in the world was "
      , "what in the world are "
      , "what in the world were "
      , "what on earth is "
      , "what on earth was "
      , "what on earth are "
      , "what on earth were "
      ]
    rewriteWithMarker marker =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, suffix)
          | looksPoliteQuestionLeadInPrefix prefix ->
              let candidate = trim (marker <> trim suffix)
              in [candidate | candidate /= text, isSentenceCandidate candidate]
        _ -> []

looksPoliteQuestionLeadInPrefix ∷ String → Bool
looksPoliteQuestionLeadInPrefix prefix =
  let tokens =
        filter (not . null) $
          map (map toLower . stripTokenEdgeNoise) (words prefix)
  in not (null tokens)
       && length tokens <= 6
       && all (`elem` politeTokens) tokens
  where
    politeTokens =
      [ "and", "pray", "sir", "madam", "sirs", "please"
      , "my", "dear", "good", "now", "then", "o"
      ]

vocativeLeadInCandidates ∷ String → [String]
vocativeLeadInCandidates text =
  case splitOnSubstring ", " text of
    Just (prefix, suffix)
      | looksVocativePrefix prefix ->
          let candidate = trim suffix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []

looksVocativePrefix ∷ String → Bool
looksVocativePrefix prefix =
  case normalizedWordTokens prefix of
    [title, name] ->
      map toLower title `elem` ["mr", "mr.", "mrs", "mrs.", "ms", "ms.", "dr", "dr.", "captain", "capt", "capt."]
        && isNameLikeToken name
    _ -> False

whMatterQuestionCandidates ∷ String → [String]
whMatterQuestionCandidates text =
  nub (presentVariants ++ pastVariants)
  where
    presentVariants =
      [ "the matter is big"
      | "what is the matter" `isInfixOf` map toLower (" " <> text <> " ")
      ]
    pastVariants =
      [ "the matter was big"
      | "what was the matter" `isInfixOf` map toLower (" " <> text <> " ")
      ]

therePronounPredicateCandidates ∷ String → [String]
therePronounPredicateCandidates text =
  case dropWhile isThereLikeToken (normalizedWordTokens text) of
    pronoun : predicate
      | isPronounToken pronoun
      , not (null predicate)
      , length predicate <= 8 ->
          let headVerb = normalizeNauticalCallVerb (head predicate)
              candidateFull = trim (unwords (pronoun : predicate))
              candidateHead = trim (unwords [pronoun, headVerb])
          in [ candidate
             | candidate <- nub [candidateFull, candidateHead]
             , candidate /= text
             , isSentenceCandidate candidate
             ]
    _ -> []

isThereLikeToken ∷ String → Bool
isThereLikeToken token =
  map toLower token `elem` ["there", "thar"]

normalizeNauticalCallVerb ∷ String → String
normalizeNauticalCallVerb token =
  case map toLower token of
    "bowes" -> "blows"
    "boos" -> "blows"
    "bo-o-os" -> "blows"
    "bo" -> "blows"
    "o" -> "blows"
    "os" -> "blows"
    "blowes" -> "blows"
    lower -> lower

therePointingCopulaCandidates ∷ String → [String]
therePointingCopulaCandidates text =
  nub (rewriteTail text ++ rewriteAfterFirstThere text)
  where
    rewriteAfterFirstThere input =
      case splitOnSubstringInsensitive " there " input of
        Just (_, suffix) -> rewriteTail ("there " <> trim suffix)
        Nothing -> []

    rewriteTail clause =
      let lowered = map toLower clause
      in if "there " `isPrefixOf` lowered
           then
             case splitOnSubstringInsensitive " is " clause of
                Just (preCopula, suffix)
                  | "pointing to " `isInfixOf` map toLower preCopula ->
                      let headSuffix =
                            case splitOnSubstringInsensitive " where " suffix of
                              Just (prefix, _) -> prefix
                              Nothing -> suffix
                          candidate = trim ("there is " <> trim headSuffix)
                      in [candidate | candidate /= text, isSentenceCandidate candidate]
                _ -> []
           else []

bySettingUpTailCandidates ∷ String → [String]
bySettingUpTailCandidates text =
  nub (commaVariant ++ bareVariant)
  where
    commaVariant =
      case splitOnSubstringInsensitive ", by setting up " text of
        Just (prefix, _) ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        _ -> []
    bareVariant =
      case splitOnSubstringInsensitive " by setting up " text of
        Just (prefix, _) ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        _ -> []

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

contrastiveHereThereTailCandidates ∷ String → [String]
contrastiveHereThereTailCandidates text
  | not hasHereMarker = []
  | otherwise = nub (fromThereComma ++ fromThereBare)
  where
    lowered = map toLower (trim text)
    hasHereMarker =
      "here " `isPrefixOf` lowered
        || " here " `isInfixOf` (" " <> lowered <> " ")
    fromThereComma =
      case splitOnSubstringInsensitive " there, " text of
        Just (_, suffix) ->
          let candidate = trim suffix
          in [candidate | isSentenceCandidate candidate, looksFiniteClauseStart candidate]
        Nothing -> []
    fromThereBare =
      case splitOnSubstringInsensitive " there " text of
        Just (_, suffix) ->
          let candidate = trim suffix
          in [candidate | isSentenceCandidate candidate, looksFiniteClauseStart candidate]
        Nothing -> []

looksFiniteClauseStart ∷ String → Bool
looksFiniteClauseStart sentence =
  case words (trim sentence) of
    subj : aux : _ -> isPronounToken subj && isAuxiliaryToken aux
    _ -> False

looksLikelyFiniteClauseStart ∷ String → Bool
looksLikelyFiniteClauseStart sentence =
  looksFiniteClauseStart sentence || startsWithNominalSubjectAux sentence

startsWithNominalSubjectAux ∷ String → Bool
startsWithNominalSubjectAux sentence =
  case normalizedWordTokens sentence of
    subj : aux : _ ->
      isWordLikeToken subj
        && not (isPronounToken subj)
        && isAuxiliaryToken aux
    _ -> False

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
      , "consequently", "then", "now"
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

trailingThereDropCandidates ∷ String → [String]
trailingThereDropCandidates text =
  let trimmed = trim text
      lower = map toLower trimmed
  in if length trimmed > 8 && endsWith lower " there"
       then
         let candidate = trim (take (length trimmed - length " there") trimmed)
         in [candidate | candidate /= text, isSentenceCandidate candidate]
       else []

tillClauseTailCandidates ∷ String → [String]
tillClauseTailCandidates text =
  nub (dropTill ++ dropUntil)
  where
    dropTill =
      case splitOnLastSubstringInsensitive " till " text of
        Just (prefix, _) ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        Nothing -> []
    dropUntil =
      case splitOnLastSubstringInsensitive " until " text of
        Just (prefix, _) ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        Nothing -> []

notTillThatClauseCandidates ∷ String → [String]
notTillThatClauseCandidates text =
  nub (fromTill ++ fromUntil)
  where
    fromTill = extractAfterMarker " not till "
    fromUntil = extractAfterMarker " not until "
    extractAfterMarker marker =
      case splitOnSubstringInsensitive marker text of
        Just (_, tailAfterMarker) ->
          case extractThatTail tailAfterMarker of
            Just clause ->
              let candidate = trim clause
              in [candidate | candidate /= text, isSentenceCandidate candidate]
            Nothing -> []
        Nothing -> []
    extractThatTail input =
      case splitOnSubstringInsensitive ", that " input of
        Just (_, clause) -> Just clause
        Nothing ->
          case splitOnSubstringInsensitive " that " input of
            Just (_, clause) -> Just clause
            Nothing -> Nothing

modalInterposedInPhraseCandidates ∷ String → [String]
modalInterposedInPhraseCandidates text =
  case rewrite [] (words text) of
    Just candidateTokens ->
      let candidate = unwords candidateTokens
      in [candidate | candidate /= text, isSentenceCandidate candidate]
    Nothing -> []
  where
    rewrite _ [] = Nothing
    rewrite _ [_] = Nothing
    rewrite prefixRev (a : b : rest)
      | isModalToken a
      , map toLower (stripTokenEdgeNoise b) == "in" =
          case splitPrepPhraseAtVerb rest of
            Just (prepPhrase, verbToken, tailTokens)
              | length prepPhrase >= 2 ->
                  Just (reverse prefixRev ++ [a, verbToken] ++ tailTokens ++ ("in" : prepPhrase))
            _ -> Nothing
    rewrite prefixRev (a : b : rest) = rewrite (a : prefixRev) (b : rest)

splitPrepPhraseAtVerb ∷ [String] → Maybe ([String], String, [String])
splitPrepPhraseAtVerb tokens = go [] tokens
  where
    go _ [] = Nothing
    go acc (x : xs)
      | isLikelyInterposedVerbToken x = Just (reverse acc, x, xs)
      | otherwise = go (x : acc) xs

isLikelyInterposedVerbToken ∷ String → Bool
isLikelyInterposedVerbToken token =
  let lower = map toLower (stripTokenEdgeNoise token)
  in not (null lower)
      && all isAlpha lower
      && lower `notElem`
          [ "a", "an", "the", "this", "that", "these", "those"
          , "in", "on", "at", "by", "with", "from", "for", "of", "to", "through"
          , "peaceful", "calm", "still", "great", "small", "vast"
          ]

isModalToken ∷ String → Bool
isModalToken token =
  map toLower (stripTokenEdgeNoise token) `elem`
    [ "can", "could", "may", "might", "must", "shall", "should", "will", "would" ]

locativeAppearsClauseCandidates ∷ String → [String]
locativeAppearsClauseCandidates text =
  concatMap rewriteWithMarker markers
  where
    markers =
      [ " on his ", " on her ", " on their ", " on its "
      , " on my ", " on your ", " on our ", " on the "
      ]
    rewriteWithMarker marker =
      case splitOnSubstringInsensitive marker text of
        Just (_, suffix) ->
          case words (trim suffix) of
            locNoun : rest ->
              case reverse rest of
                verbToken : revSubject
                  | isAppearsVerbToken verbToken
                  , not (null revSubject) ->
                      let subjectPhrase = unwords (reverse revSubject)
                          candidate = trim (subjectPhrase <> " " <> verbToken <> marker <> locNoun)
                      in [candidate | candidate /= text, isSentenceCandidate candidate]
                _ -> []
            _ -> []
        Nothing -> []

isAppearsVerbToken ∷ String → Bool
isAppearsVerbToken token =
  map toLower (stripTokenEdgeNoise token) `elem` ["appear", "appears", "appeared"]

andInGerundTailCandidates ∷ String → [String]
andInGerundTailCandidates text =
  nub (commaVariant ++ bareVariant)
  where
    commaVariant =
      case splitOnSubstringInsensitive ", and in " text of
        Just (prefix, suffix)
          | looksAndInGerundTail suffix ->
              let candidate = trim prefix
              in [candidate | candidate /= text, isSentenceCandidate candidate]
        _ -> []
    bareVariant =
      case splitOnSubstringInsensitive " and in " text of
        Just (prefix, suffix)
          | looksAndInGerundTail suffix ->
              let candidate = trim prefix
              in [candidate | candidate /= text, isSentenceCandidate candidate]
        _ -> []

looksAndInGerundTail ∷ String → Bool
looksAndInGerundTail suffix =
  let tokens = normalizedWordTokens suffix
      firstFew = take 4 tokens
      hasGerund = any (\t -> endsWith (map toLower t) "ing") firstFew
  in length tokens >= 3 && hasGerund

andIntoAirTailCandidates ∷ String → [String]
andIntoAirTailCandidates text =
  nub (commaVariant ++ bareVariant)
  where
    commaVariant = trimWithMarker ", and "
    bareVariant = trimWithMarker " and "
    trimWithMarker marker =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, suffix)
          | looksIntoAirTail suffix ->
              let candidate = trim prefix
              in [candidate | candidate /= text, isSentenceCandidate candidate]
        _ -> []

looksIntoAirTail ∷ String → Bool
looksIntoAirTail suffix =
  let lower = " " <> map toLower suffix <> " "
      tokens = normalizedWordTokens suffix
  in length tokens >= 4
      && " into the air " `isInfixOf` lower
      && all isAlpha (map toLower (head tokens))

andFiniteTailCandidates ∷ String → [String]
andFiniteTailCandidates text =
  case splitOnSubstringInsensitive ", and " text of
    Just (prefix, suffix)
      ->
          let candidate = trim suffix
              finiteTail =
                [candidate | looksFiniteAndTail prefix suffix, candidate /= text, isSentenceCandidate candidate]
              simplifiedInferior = simplifyInferiorClause candidate
          in nub (finiteTail ++ simplifiedInferior)
    _ -> []

looksFiniteAndTail ∷ String → String → Bool
looksFiniteAndTail prefix suffix =
  let prefixTokens = normalizedWordTokens prefix
      suffixTokens = normalizedWordTokens suffix
  in length prefixTokens >= 8
       && length suffixTokens >= 3
       && length suffixTokens <= 8
       && startsWithLikelySubjectToken (head suffixTokens)
       && isWordLikeToken (last suffixTokens)

startsWithLikelySubjectToken ∷ String → Bool
startsWithLikelySubjectToken token =
  let lower = map toLower token
  in lower `elem`
       [ "the", "a", "an", "this", "that", "these", "those"
       , "every", "each", "some", "many", "most", "all"
       , "i", "you", "we", "they", "he", "she", "it"
       ]

simplifyInferiorClause ∷ String → [String]
simplifyInferiorClause clause =
  nub (simplifyWithCopula " is inferior " "is inferior" ++ simplifyWithCopula " are inferior " "are inferior")
  where
    simplifyWithCopula marker copula =
      case splitOnSubstringInsensitive marker clause of
        Just (subjectPart, predicateTail) ->
          case extractNounHeadPhrase subjectPart of
            Just subject ->
              let withObject =
                    case splitOnSubstringInsensitive " to " predicateTail of
                      Just (_, objectPart) ->
                        case extractNounHeadPhrase objectPart of
                          Just object ->
                            let candidate = trim (subject <> " " <> copula <> " to " <> object)
                            in [candidate | candidate /= clause, isSentenceCandidate candidate]
                          Nothing -> []
                      Nothing -> []
                  bareCandidate = trim (subject <> " " <> copula)
                  bare = [bareCandidate | bareCandidate /= clause, isSentenceCandidate bareCandidate]
              in withObject ++ bare
            Nothing -> []
        Nothing -> []

extractNounHeadPhrase ∷ String → Maybe String
extractNounHeadPhrase phrase =
  case normalizedWordTokens phrase of
    det : noun : _
      | map toLower det `elem` ["the", "a", "an", "this", "that", "these", "those", "my", "your", "his", "her", "its", "our", "their", "every", "each", "some", "many", "most", "all"]
      , isWordLikeToken noun ->
          Just (det <> " " <> noun)
    noun : _
      | isWordLikeToken noun -> Just noun
    _ -> Nothing

forCouldNeverTailCandidates ∷ String → [String]
forCouldNeverTailCandidates text =
  nub (commaVariant ++ bareVariant)
  where
    commaVariant =
      case splitOnSubstringInsensitive ", for i could never " text of
        Just (prefix, _) ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        _ -> []
    bareVariant =
      case splitOnSubstringInsensitive " for i could never " text of
        Just (prefix, _) ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        _ -> []

trailingForPhraseCandidates ∷ String → [String]
trailingForPhraseCandidates text =
  case splitOnLastSubstringInsensitive " for " text of
    Just (prefix, suffix)
      | looksTrailingForAdjunct suffix ->
          let candidate = trim prefix
          in [candidate | isSentenceCandidate candidate]
    _ -> []

memoryHavingTailCandidates ∷ String → [String]
memoryHavingTailCandidates text =
  nub (markerCandidate " recollect having " " recollect" ++ markerCandidate " remember having " " remember")
  where
    markerCandidate marker verb =
      case splitOnSubstringInsensitive marker text of
        Just (prefix, _) ->
          let candidate = trim (prefix <> verb)
          in [candidate | candidate /= text, isSentenceCandidate candidate]
        Nothing -> []

fewEverReturnCandidates ∷ String → [String]
fewEverReturnCandidates text =
  nub
    [ candidate
    | candidate <- map trim
        [ replaceAllInsensitive " few ever return " " few people return " (" " <> text <> " ")
        , replaceAllInsensitive " few return " " few people return " (" " <> text <> " ")
        ]
    , candidate /= text
    , isSentenceCandidate candidate
    ]

commaOneClauseTailCandidates ∷ String → [String]
commaOneClauseTailCandidates text =
  case splitOnSubstringInsensitive ", one " text of
    Just (prefix, suffix)
      | looksOneClauseTail suffix ->
          let candidate = trim prefix
          in [candidate | candidate /= text, isSentenceCandidate candidate]
    _ -> []

looksOneClauseTail ∷ String → Bool
looksOneClauseTail suffix =
  let lower = map toLower suffix
      padded = " " <> lower <> " "
  in any (`isInfixOf` padded) [" came ", " did ", " was ", " were "]

purposeInfinitiveTailCandidates ∷ String → [String]
purposeInfinitiveTailCandidates text =
  case splitOnSubstringInsensitive " to " text of
    Just (prefix, suffix)
      | looksPurposeInfinitiveTail suffix ->
          let candidate = trim prefix
          in [candidate | isSentenceCandidate candidate]
    _ -> []

looksPurposeInfinitiveTail ∷ String → Bool
looksPurposeInfinitiveTail suffix =
  case normalizedWordTokens suffix of
    verb : rest ->
      isWordLikeToken verb
        && map toLower verb `notElem` ["a", "an", "the", "that", "which", "who", "whom"]
        && length rest >= 1
    _ -> False

engagedInContrastCandidates ∷ String → [String]
engagedInContrastCandidates text =
  case splitOnSubstringInsensitive " air from those engaged in " text of
    Just (_, engagedTail) ->
      case splitOnSubstringInsensitive " has " text of
        Just splitHas -> buildCandidate engagedTail splitHas
        Nothing ->
          case splitOnSubstringInsensitive " have " text of
            Just splitHave -> buildCandidate engagedTail splitHave
            Nothing -> []
    Nothing -> []
  where
    buildCandidate engagedTail (subjectPart, _) =
      case extractNounHeadPhrase subjectPart of
        Just subject ->
          let tailPhrase =
                case splitOnSubstring "," engagedTail of
                  Just (phrase, _) -> trim phrase
                  Nothing -> trim engagedTail
              tokens = take 3 (normalizedWordTokens tailPhrase)
              objectPhrase = unwords tokens
              candidate = trim (subject <> " runs in " <> objectPhrase)
          in [candidate | not (null objectPhrase), candidate /= text, isSentenceCandidate candidate]
        Nothing -> []

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

gothicArchCandidates ∷ String → [String]
gothicArchCandidates text =
  filter (/= text)
    [ trim (replaceAllInsensitive " gothic arch" " arch" (" " <> text <> " "))
    ]

laidOpenVerbCandidates ∷ String → [String]
laidOpenVerbCandidates text =
  nub (hyphenated ++ spaced)
  where
    hyphenated =
      let candidate = trim (replaceAllInsensitive " laid-open " " opened " (" " <> text <> " "))
      in [candidate | candidate /= text, isSentenceCandidate candidate]
    spaced =
      let candidate = trim (replaceAllInsensitive " laid open " " opened " (" " <> text <> " "))
      in [candidate | candidate /= text, isSentenceCandidate candidate]

laidOpenMainClauseCandidates ∷ String → [String]
laidOpenMainClauseCandidates text =
  nub (hyphenated ++ spaced)
  where
    hyphenated = buildFrom ", laid-open "
    spaced = buildFrom ", laid open "
    buildFrom needle =
      case splitOnSubstringInsensitive needle text of
        Just (prefix, predicateTail) ->
          case extractNounHeadPhrase prefix of
            Just subject ->
              let candidate = trim (subject <> " opened " <> predicateTail)
              in [candidate | candidate /= text, isSentenceCandidate candidate]
            Nothing -> []
        Nothing -> []

normalizeArchaicToken ∷ String → String
normalizeArchaicToken token =
  case map toLower token of
    "doth" -> "does"
    "hath" -> "has"
    "maketh" -> "makes"
    "catched" -> "caught"
    lower
      | endsWith lower "eth"
      , length lower > 4 -> inflectThirdSingular (take (length lower - 3) lower)
    lower -> lower

inflectThirdSingular ∷ String → String
inflectThirdSingular stem
  | null stem = stem
  | endsWith stem "sh" || endsWith stem "ch" || endsWith stem "ss" || finalChar `elem` ['s', 'x', 'z', 'o'] =
      stem <> "es"
  | finalChar == 'y'
  , Just prev <- previousChar
  , not (isVowelChar prev) =
      take (length stem - 1) stem <> "ies"
  | otherwise = stem <> "s"
  where
    finalChar =
      case reverse stem of
        c : _ -> c
        [] -> ' '
    previousChar =
      case reverse stem of
        _ : c : _ -> Just c
        _ -> Nothing

isVowelChar ∷ Char → Bool
isVowelChar c = toLower c `elem` ['a', 'e', 'i', 'o', 'u']

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

amongParticipialTailCandidates ∷ String → [String]
amongParticipialTailCandidates text =
  case splitOnSubstringInsensitive " among " text of
    Just (prefix, _) ->
      case reverse (words (trim prefix)) of
        participle : restRev
          | looksParticipleLike participle ->
              let candidate = trim (unwords (reverse restRev))
              in [candidate | candidate /= text, isSentenceCandidate candidate]
        _ -> []
    Nothing -> []

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
      && ( endsWith normalized "ed"
            || endsWith normalized "en"
            || (length normalized > 4 && endsWith normalized "ing")
         )

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
    [token] -> isWordLikeToken token || isAbbreviationToken token
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

isUnbalancedQuoteFragmentSentence ∷ String → Bool
isUnbalancedQuoteFragmentSentence text =
  let quoteCount = length (filter (== '"') text)
      tokens = normalizedWordTokens text
  in odd quoteCount && length tokens >= 6

isNameFragmentSentence ∷ String → Bool
isNameFragmentSentence text =
  let tokens = normalizedWordTokens text
      nonEmptyTokens = filter (not . null) tokens
      nonConnectorTokens = filter (not . isConnectorToken) nonEmptyTokens
  in length nonConnectorTokens >= 2
      && all isNameLikeToken nonConnectorTokens
      && all (\token -> isConnectorToken token || isNameLikeToken token) nonEmptyTokens

isDanglingNameConnectorFragmentSentence ∷ String → Bool
isDanglingNameConnectorFragmentSentence text =
  case normalizedWordTokens text of
    [] -> False
    tokens ->
      let nonConnectorTokens = filter (not . isConnectorToken) tokens
      in length tokens <= 3
          && not (null nonConnectorTokens)
          && length nonConnectorTokens < length tokens
          && all isNameLikeToken nonConnectorTokens
          && all (\token -> isConnectorToken token || isNameLikeToken token) tokens

isPrepositionalNameFragmentSentence ∷ String → Bool
isPrepositionalNameFragmentSentence text =
  case normalizedWordTokens text of
    prep : rest ->
      map toLower prep `elem` ["for", "of", "by", "from"]
        && not (null rest)
        && length rest <= 4
        && not (null nonConnector)
        && all (\token -> isConnectorToken token || isNameLikeToken token) rest
      where
        nonConnector = filter (not . isConnectorToken) rest
    _ -> False

isDanglingPrepositionalFragmentSentence ∷ String → Bool
isDanglingPrepositionalFragmentSentence text =
  case normalizedWordTokens text of
    prep : rest ->
      let lowerPrep = map toLower prep
          lowerRest = map (map toLower) rest
          shortFunctionOrTitleFragment =
            length rest <= 3
              && and
                [ lower `elem`
                    [ "the", "a", "an", "this", "that", "these", "those"
                    , "my", "your", "his", "her", "its", "our", "their"
                    , "and", "or", "&"
                    ]
                    || isNameLikeToken token
                | (token, lower) <- zip rest lowerRest
                ]
      in lowerPrep `elem` ["of", "in", "on", "to", "for", "from", "by", "with", "at", "into", "upon", "round", "around"]
           && not (null rest)
           && ( shortFunctionOrTitleFragment
                   || looksPossessivePrepositionalFragment lowerPrep rest
               )
    _ -> False

looksPossessivePrepositionalFragment ∷ String → [String] → Bool
looksPossessivePrepositionalFragment prep rest =
  prep == "to"
    && length rest >= 3
    && length rest <= 6
    && case map (map toLower) rest of
         det : tailTokens ->
           det `elem` ["the", "a", "an", "this", "that", "these", "those", "my", "your", "his", "her", "its", "our", "their"]
             && case break (\token -> "'s" `isInfixOf` token) tailTokens of
                  (_mods, poss : objectTail) ->
                    not (null objectTail)
                      && all isWordLikeToken (poss : objectTail)
                  _ -> False
         _ -> False

isComparativeMetaFragmentSentence ∷ String → Bool
isComparativeMetaFragmentSentence text =
  let lower = map toLower text
      padded = " " <> lower <> " "
  in " if we compare " `isInfixOf` padded
      && " in respect to " `isInfixOf` padded
      && " in the comparison" `isInfixOf` padded

isDirectionalWhFragmentSentence ∷ String → Bool
isDirectionalWhFragmentSentence text =
  case map (map toLower) (normalizedWordTokens text) of
    ["where", direction] ->
      direction `elem`
        [ "away", "off", "astern", "ahead", "aloft", "abroad", "ashore", "abeam"
        ]
    ["how", "far", "off"] -> True
    _ -> False

isNauticalBearingFragmentSentence ∷ String → Bool
isNauticalBearingFragmentSentence text =
  let tokens = map (map toLower) (normalizedWordTokens text)
      trimmed =
        case reverse tokens of
          tailToken : restRev
            | tailToken `elem` ["sir", "sirs", "captain", "mate"] ->
                reverse restRev
          _ -> tokens
      nauticalCues =
        [ "bow", "beam", "stern", "quarter", "lee", "weather"
        , "starboard", "port", "ahead", "astern", "abeam"
        ]
  in case trimmed of
       numberToken : pointToken : relation : rest ->
         isSmallNumberToken numberToken
           && pointToken `elem` ["point", "points"]
           && relation `elem` ["off", "on", "to"]
           && length rest >= 1
           && length trimmed <= 10
           && any (`elem` rest) nauticalCues
       _ -> False

isNauticalHailFragmentSentence ∷ String → Bool
isNauticalHailFragmentSentence text =
  case map (map toLower) (normalizedWordTokens text) of
    prefix@(_ : _)
      | last prefix == "ahoy"
      , length prefix >= 2
      , length prefix <= 4 ->
           let cues = ["mast", "head", "mast-head", "bow", "stern", "deck", "lookout"]
           in any (`elem` init prefix) cues
    _ -> False

isNauticalDistanceFragmentSentence ∷ String → Bool
isNauticalDistanceFragmentSentence text =
  let tokens = map (map toLower) (normalizedWordTokens text)
      trimmed =
        case reverse tokens of
          tailToken : restRev
            | tailToken `elem` ["sir", "sirs", "captain", "mate"] ->
                reverse restRev
          _ -> tokens
      distanceUnits = ["mile", "miles", "yard", "yards", "league", "leagues", "cable", "cables"]
  in case trimmed of
       numberToken : unit : rest ->
         isSmallNumberToken numberToken
           && unit `elem` distanceUnits
           && ( null rest
                  || rest == ["and", "a", "half"]
                  || rest == ["and", "half"]
              )
       _ -> False

isStormOathFragmentSentence ∷ String → Bool
isStormOathFragmentSentence text =
  case map (map toLower) (normalizedWordTokens text) of
    [left, "and", right] ->
      let oathWords =
            [ "thunder", "lightning", "blazes", "heavens"
            , "fire", "brimstone", "devil", "damnation"
            ]
      in left `elem` oathWords && right `elem` oathWords
    _ -> False

isSoAdjectiveFragmentSentence ∷ String → Bool
isSoAdjectiveFragmentSentence text =
  case map (map toLower) (normalizedWordTokens text) of
    ["so", adjective] -> looksAdjectiveLikeToken adjective
    _ -> False

isShortCopularNominalFragmentSentence ∷ String → Bool
isShortCopularNominalFragmentSentence text =
  case map (map toLower) (normalizedWordTokens text) of
    [subj, cop, det, noun] ->
      subj `elem` ["it", "this", "that", "these", "those", "he", "she", "they", "we", "you", "i"]
        && cop `elem` ["is", "are", "was", "were"]
        && det `elem` ["the", "a", "an", "this", "that", "these", "those", "my", "your", "his", "her", "its", "our", "their"]
        && isWordLikeToken noun
    [det, noun, cop, subj] ->
      det `elem` ["the", "a", "an", "this", "that", "these", "those"]
        && isWordLikeToken noun
        && cop `elem` ["is", "are", "was", "were"]
        && subj `elem` ["it", "this", "that", "he", "she", "they"]
    _ -> False

isShortVocativeInterjectionSentence ∷ String → Bool
isShortVocativeInterjectionSentence text =
  let tokens = map (map toLower) (normalizedWordTokens text)
      vocatives = ["sir", "sirs", "madam", "captain", "mate"]
  in case tokens of
       [word, vocative] ->
         vocative `elem` vocatives
           && isWordLikeToken word
       [first, second, vocative] ->
         vocative `elem` vocatives
           && isWordLikeToken first
           && isWordLikeToken second
       _ -> False

isBibliographicFragmentSentence ∷ String → Bool
isBibliographicFragmentSentence text =
  let tokens = normalizedWordTokens text
      lowerTokens = map (map toLower) tokens
      hasAbbreviation = any isAbbreviationToken tokens
      hasYear = any isYearToken tokens
      hasBylineCue = "by" `elem` lowerTokens
      hasName = any isNameLikeToken tokens
      startsWithConnector =
        case lowerTokens of
          first : _ -> first `elem` ["or", "and"]
          _ -> False
      hasCitationCue =
        any (`elem` lowerTokens) ["narrative", "verbal", "mouth", "chronicle", "history"]
      startsWithVide =
        case lowerTokens of
          first : _ -> first == "vide"
          _ -> False
      hasInitialCitationToken =
        length
          [ token
          | token <- tokens
          , length token == 1
          , all isUpper token
          ] >= 2
      yearBylineName = hasYear && hasBylineCue && hasName
      videReference =
        startsWithVide
          && ("his" `elem` lowerTokens || "her" `elem` lowerTokens || "their" `elem` lowerTokens)
          && length tokens <= 6
          && (hasAbbreviation || hasInitialCitationToken)
      placeYearReference =
        hasYear
          && length tokens >= 2
          && length tokens <= 4
          && any isNameLikeToken tokens
          && all (\token -> isYearToken token || isNameLikeToken token) tokens
      monthDayReference =
        case lowerTokens of
          month : day : rest ->
            isMonthToken month
              && isDayToken day
              && length rest <= 1
              && all isYearToken rest
          _ -> False
  in (hasAbbreviation && yearBylineName)
       || (yearBylineName && startsWithConnector && hasCitationCue)
       || videReference
       || placeYearReference
       || monthDayReference

isBibliographicTitleFragmentSentence ∷ String → Bool
isBibliographicTitleFragmentSentence text =
  let tokens = normalizedWordTokens text
      lowerTokens = map (map toLower) tokens
      titleCues =
        [ "version", "apology", "morals", "history", "chronicle", "narrative"
        , "preface", "chapter", "book", "volume", "part", "voyages"
        , "voyage", "letter", "letters", "memorial", "reference", "visit", "account", "conversation", "conversations", "shipwreck", "world", "triumph", "trans", "transactions", "survivor", "survivors", "journal", "speech", "report"
        , "captor", "captors", "adventure", "adventures", "biography", "cruise"
        ]
      hasTitleCue =
        any (`elem` lowerTokens) titleCues
      hasLeadingTitleCue =
        case lowerTokens of
          first : _ -> first `elem` titleCues
          [] -> False
      hasPossessive = any (\token -> "'s" `isInfixOf` map toLower token) tokens
      endsWithFunctionWord =
        case reverse lowerTokens of
          lastToken : _ -> lastToken `elem` ["the", "a", "an", "of", "for", "in", "to", "with", "at", "on"]
          [] -> False
      classicTitleFragment =
        length tokens >= 2
          && length tokens <= 8
          && hasTitleCue
          && hasPossessive
          && endsWithFunctionWord
      headingTitleFragment =
        length tokens >= 2
          && length tokens <= 4
          && hasLeadingTitleCue
          && endsWithFunctionWord
      prepositionalTitleFragment =
        case (tokens, lowerTokens) of
          (_ : restTokens, "of" : restLower) ->
            let nameLikeCount = length (filter isNameLikeToken restTokens)
                allTitleLike =
                  and
                    [ isNameLikeToken token
                        || lower `elem` ["the", "a", "an"]
                        || lower `elem` ["to", "in", "on", "for", "from", "at", "into", "upon", "by", "with", "of"]
                        || lower `elem` titleCues
                        || isConnectorToken lower
                    | (token, lower) <- zip restTokens restLower
                    ]
            in length restTokens >= 2
                && length restTokens <= 10
                && nameLikeCount >= 2
                && allTitleLike
          _ -> False
      danglingPrepositionalTitleFragment =
        case (tokens, lowerTokens) of
          (_ : _ : restTokens, titleLower : "of" : restLower) ->
            let nameLikeCount = length (filter isNameLikeToken restTokens)
                endsWithConnector =
                  case reverse restLower of
                    tailToken : _ -> isConnectorToken tailToken
                    [] -> False
                allTitleLike =
                  and
                    [ isNameLikeToken token
                        || lower `elem` ["the", "a", "an"]
                        || lower `elem` ["to", "in", "on", "for", "from", "at", "into", "upon", "by", "with", "of"]
                        || lower `elem` titleCues
                        || isConnectorToken lower
                    | (token, lower) <- zip restTokens restLower
                    ]
            in titleLower `elem` titleCues
                && length restTokens >= 2
                && length restTokens <= 10
                && nameLikeCount >= 1
                && endsWithConnector
                && allTitleLike
          _ -> False
      modifiedPrepositionalTitleFragment =
        case (tokens, lowerTokens) of
          (_ : _ : _ : restTokens, modifier : titleLower : "of" : restLower) ->
            let nameLikeCount = length (filter isNameLikeToken restTokens)
                modifierOk = modifier `elem` ["another", "other", "new", "revised"]
                allTitleLike =
                  and
                    [ isNameLikeToken token
                        || isWordLikeToken token
                        || lower `elem` ["the", "a", "an"]
                        || lower `elem` ["to", "in", "on", "for", "from", "at", "into", "upon", "by", "with", "of"]
                        || lower `elem` titleCues
                        || isConnectorToken lower
                    | (token, lower) <- zip restTokens restLower
                    ]
            in modifierOk
                && titleLower `elem` titleCues
                && length restTokens >= 2
                && length restTokens <= 12
                && nameLikeCount >= 1
                && allTitleLike
          _ -> False
      possessiveTitleSpanFragment =
        length tokens >= 3
          && length tokens <= 30
          && hasPossessive
          && hasTitleCue
          && any (`elem` lowerTokens) ["into", "in", "of", "on", "from", "round", "to"]
          && any isNameLikeToken tokens
          && (length tokens <= 14 || "," `isInfixOf` text)
      relativeTitleFragment =
        case lowerTokens of
          "of" : _ ->
            length tokens >= 6
              && length tokens <= 40
              && hasTitleCue
              && "which" `elem` lowerTokens
               && ("was" `elem` lowerTokens || "were" `elem` lowerTokens || "is" `elem` lowerTokens || "are" `elem` lowerTokens)
               && any isNameLikeToken tokens
          _ -> False
      leadingOfPossessiveTitleFragment =
        case lowerTokens of
          "of" : _ ->
            length tokens >= 6
              && length tokens <= 40
              && hasTitleCue
              && hasPossessive
              && any isNameLikeToken tokens
              && ("," `isInfixOf` text || endsWithFunctionWord)
          _ -> False
  in classicTitleFragment
       || headingTitleFragment
       || prepositionalTitleFragment
       || danglingPrepositionalTitleFragment
       || modifiedPrepositionalTitleFragment
       || possessiveTitleSpanFragment
       || relativeTitleFragment
       || leadingOfPossessiveTitleFragment

isBibliographicBylineRoleFragmentSentence ∷ String → Bool
isBibliographicBylineRoleFragmentSentence text =
  case splitOnSubstring ", " text of
    Just (prefix, suffix) ->
      let prefixTokens = normalizedWordTokens prefix
          prefixLower = map (map toLower) prefixTokens
          suffixTokens = normalizedWordTokens suffix
          suffixLower = map (map toLower) suffixTokens
          nameCount = length (filter isNameLikeToken prefixTokens)
          prefixLooksByline =
            nameCount >= 2
              && any (`elem` prefixLower) ["of", "from"]
              && and
                  [ isNameLikeToken token || lower `elem` ["of", "from", "the"]
                  | (token, lower) <- zip prefixTokens prefixLower
                  ]
          prefixLooksKinshipByline =
            case (prefixTokens, prefixLower) of
              (ofTok : nameTok : _possTok : kinTok : restTokens, "of" : _ : poss : kin : restLower) ->
                isNameLikeToken nameTok
                  && map toLower ofTok == "of"
                  && poss `elem` ["his", "her", "their"]
                  && kin `elem` ["brother", "sister", "son", "daughter", "widow", "wife", "husband"]
                  && and
                      [ isNameLikeToken token
                          || lower `elem`
                               [ "of", "the", "his", "her", "their"
                               , "brother", "sister", "son", "daughter", "widow", "wife", "husband"
                               ]
                      | (token, lower) <- zip restTokens restLower
                      ]
              _ -> False
          hasRoleCue =
            any (`elem` suffixLower)
              [ "mate", "captain", "master", "author", "editor"
              , "witness", "narrator", "pilot", "sailor", "mariner"
              ]
          hasSaidVessel =
            "of" `elem` suffixLower && "said" `elem` suffixLower
          suffixLooksNameSpan =
            let suffixNameCount = length (filter isNameLikeToken suffixTokens)
            in suffixNameCount >= 1
                 && and
                      [ isNameLikeToken token || isConnectorToken lower || lower `elem` ["jr", "sr", "rev", "dr", "mr", "mrs", "ms"]
                      | (token, lower) <- zip suffixTokens suffixLower
                      ]
      in not (null suffixTokens)
           && length suffixTokens <= 8
           && ( (prefixLooksByline && (hasRoleCue || hasSaidVessel))
                 || (prefixLooksKinshipByline && suffixLooksNameSpan)
              )
    _ -> False

isYearToken ∷ String → Bool
isYearToken token =
  let digits = filter isDigit token
  in length digits >= 3 && length digits <= 4

isMonthToken ∷ String → Bool
isMonthToken token =
  token `elem`
    [ "january", "february", "march", "april", "may", "june"
    , "july", "august", "september", "october", "november", "december"
    ]

isDayToken ∷ String → Bool
isDayToken token =
  let digits = filter isDigit token
  in not (null digits) && length digits <= 2

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
parseArgs cfg ("--jobs" : n : rest) = do
  parsed <- parsePositiveInt "--jobs" n
  parseArgs cfg {cfgJobs = parsed} rest
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
parseArgs _ ("--jobs" : []) = Left "missing value for --jobs"
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

parsePositiveInt ∷ String → String → Either String Int
parsePositiveInt flag input =
  case reads input of
    [(n, "")] | n > 0 -> Right n
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
  , "  cabal run erato-corpus -- [--file PATH]... [--dir PATH]... [--jobs N]"
  , "                           [--show-failures N]"
  , "                           [--max-sentences N]"
  , "                           [--stop-after-unparsed N]"
  , "                           [--min-controlled-rate R]"
  , "                           [--min-total-rate R]"
  , ""
  , "Options:"
  , "  --file PATH               Add a specific .txt file."
  , "  --dir PATH                Recursively scan a directory for .txt files."
  , "  --jobs N                  Parse up to N sentences in parallel (default: 1)."
  , "  --show-failures N         Print up to N unparsed examples (default: 20)."
  , "  --max-sentences N         Parse at most N sentence candidates."
  , "  --stop-after-unparsed N   Stop early after N unparsed sentences."
  , "  --min-controlled-rate R   Fail if controlled parse rate is below R (0.0-1.0)."
  , "  --min-total-rate R        Fail if controlled+fallback parse rate is below R (0.0-1.0)."
  ]
