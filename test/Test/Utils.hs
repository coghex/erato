{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Utils
  ( withGrammars
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

withGrammars ∷ (GrammarBundle → IO ()) → IO ()
withGrammars action = do
  hasControlled <- doesFileExist "Grammar/EratoAbs.pgf"
  hasFallback <- doesFileExist "Grammar/AllEngAbs.pgf"
  if not (hasControlled && hasFallback)
    then pendingWith "Missing Grammar/EratoAbs.pgf or Grammar/AllEngAbs.pgf; run gf -make first."
    else loadGrammars "Grammar/EratoAbs.pgf" "Grammar/AllEngAbs.pgf" >>= action

shouldParse ∷ ∀ α. (Show α) ⇒ [α] → Expectation
shouldParse exprs = exprs `shouldSatisfy` (not . null)

shouldReject ∷ ∀ α. (Show α, Eq α) ⇒ [α] → Expectation
shouldReject exprs = exprs `shouldBe` []

shouldParseAs ∷ [Expr] → Sentence → Expectation
shouldParseAs exprs expected = do
  let asts = mapMaybe exprToSentence exprs
  asts `shouldSatisfy` elem expected
