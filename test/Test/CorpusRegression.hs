{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.CorpusRegression (spec) where

import Control.Applicative ((<|>))
import Control.Exception (evaluate)
import Data.Char (isSpace, toLower)
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import System.Timeout (timeout)
import Test.Hspec

import Parser.GFParser

data RegressionExpectation
  = MustControlled
  | MustParse
  | PendingControlled
  deriving (Eq, Show)

data RegressionCase = RegressionCase
  { regressionExpectation ∷ RegressionExpectation
  , regressionLabel       ∷ String
  , regressionSentence    ∷ String
  }

perCaseTimeoutMicros ∷ Int
perCaseTimeoutMicros = 5 * 1000000

spec ∷ GrammarBundle → Spec
spec grammars = do
  cases <- runIO loadRegressionCases
  describe "Corpus regression" $
    mapM_ (mkRegressionSpec grammars) cases

mkRegressionSpec ∷ GrammarBundle → RegressionCase → Spec
mkRegressionSpec grammars regressionCase =
  it (regressionLabel regressionCase) $ do
    let controlledParsed =
          isJust (parsePreferredControlledSentence grammars (regressionSentence regressionCase))
        anyParsed =
          isJust
            ( parsePreferredControlledSentence grammars (regressionSentence regressionCase)
                <|> parsePreferredFallbackSentence grammars (regressionSentence regressionCase)
            )
    case regressionExpectation regressionCase of
      MustControlled -> do
        result <- timeout perCaseTimeoutMicros (evaluate controlledParsed)
        result `shouldBe` Just True
      MustParse -> do
        result <- timeout perCaseTimeoutMicros (evaluate anyParsed)
        result `shouldBe` Just True
      PendingControlled -> do
        result <- timeout perCaseTimeoutMicros (evaluate controlledParsed)
        case result of
          Just True ->
            pure ()
          Just False ->
            pendingWith "Known controlled-coverage target; keep in the pack until grammar support lands."
          Nothing ->
            pendingWith "Timed out while checking known controlled-coverage target."

loadRegressionCases ∷ IO [RegressionCase]
loadRegressionCases = do
  content <- readFile "testtext/corpus_regression.tsv"
  pure (map parseRegressionLine (filter keepRegressionLine (zip [1 :: Int ..] (lines content))))

keepRegressionLine ∷ (Int, String) → Bool
keepRegressionLine (_, line) =
  let trimmed = trim line
  in not (null trimmed) && not ("#" `isPrefixOf` trimmed)

parseRegressionLine ∷ (Int, String) → RegressionCase
parseRegressionLine (lineNo, line) =
  case splitTabs line of
    [rawExpectation, label, sentence] ->
      RegressionCase
        { regressionExpectation = parseExpectation lineNo rawExpectation
        , regressionLabel = label
        , regressionSentence = sentence
        }
    _ ->
      error ("Malformed corpus regression line " <> show lineNo <> ": expected 3 tab-separated fields")

parseExpectation ∷ Int → String → RegressionExpectation
parseExpectation lineNo rawExpectation =
  case map toLower rawExpectation of
    "controlled" -> MustControlled
    "parse"      -> MustParse
    "pending-controlled" -> PendingControlled
    other ->
      error ("Unknown corpus regression expectation on line " <> show lineNo <> ": " <> other)

splitTabs ∷ String → [String]
splitTabs [] = [""]
splitTabs ('\t' : rest) =
  "" : splitTabs rest
splitTabs (char : rest) =
  case splitTabs rest of
    [] -> [[char]]
    first : remaining -> (char : first) : remaining

trim ∷ String → String
trim =
  reverse . dropWhile isSpace . reverse . dropWhile isSpace
