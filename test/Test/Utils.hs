{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Utils
  ( loadTestGrammars
  , shouldParse
  , shouldReject
  , shouldParseAs
  ) where

import Test.Hspec
import System.Directory (doesFileExist)
import Data.Maybe (mapMaybe)

import Parser.GFParser
import Parser.Translate (exprToSentence)
import Parser.AST (Sentence)
import PGF (Expr)

loadTestGrammars ∷ IO (Maybe GrammarBundle)
loadTestGrammars = do
  hasControlled ← doesFileExist "Grammar/EratoAbs.pgf"
  hasFallback   ← doesFileExist "Grammar/AllEngAbs.pgf"
  if hasControlled && hasFallback
    then Just <$> loadGrammars "Grammar/EratoAbs.pgf" "Grammar/AllEngAbs.pgf"
    else pure Nothing

shouldParse ∷ ∀ α. (Show α) ⇒ [α] → Expectation
shouldParse exprs = exprs `shouldSatisfy` (not . null)

shouldReject ∷ ∀ α. (Show α, Eq α) ⇒ [α] → Expectation
shouldReject exprs = exprs `shouldBe` []

shouldParseAs ∷ [Expr] → Sentence → Expectation
shouldParseAs exprs expected = do
  let asts = mapMaybe exprToSentence exprs
  asts `shouldSatisfy` elem expected
