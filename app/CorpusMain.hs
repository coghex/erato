{-# LANGUAGE Strict, UnicodeSyntax #-}

module Main where

import Data.Char (isAlpha, isSpace, isUpper, toLower)
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
    (current, startLine, _, acc) = foldl step ([], Nothing, 1, []) input

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
      let sentence = cleanSentence (reverse chars)
      in if isSentenceCandidate sentence
           then (start, sentence) : results
           else results

isSentenceDelimiter ∷ Char → Bool
isSentenceDelimiter c =
  c `elem` (".!?" ∷ String)

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
    . filter (not . startsWithUnderscoreWord)
    . words
    . map normalizeNoiseChar

normalizeNoiseChar ∷ Char → Char
normalizeNoiseChar c
  | isDashChar c = ' '
  | c == '*' = ' '
  | otherwise = c

isDashChar ∷ Char → Bool
isDashChar c = c `elem` ("-–—―" ∷ String)

startsWithUnderscoreWord ∷ String → Bool
startsWithUnderscoreWord token =
  case dropWhile isTokenPrefixNoise token of
    '_' : _ -> True
    _ -> False

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
    Unparsed ->
      stats
        { statTotal = statTotal stats + 1
        , statUnparsed = statUnparsed stats + 1
        , statFailures = appendFailureIfRoom (statShowLimit stats) (statFailures stats) sentence
        }
  where
    classifySentence bundle text
      | isSingleWordSentence text = AutoPassSingleWord
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
  lexiconPluralCandidates text
    ++ progressiveEverCandidates text
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
  case words text of
    [token] -> isWordLikeToken token
    _ -> False

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
