{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Imperative (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Imperatives" $ do
  it "parses imperative: run" $ do
    let exprs = parseControlled grammars "run"
    shouldParse exprs
    exprs `shouldParseAs`
      Imperative Positive (Intransitive "run")

  it "parses imperative transitive: eat the food" $ do
    let exprs = parseControlled grammars "eat the food"
    shouldParse exprs
    exprs `shouldParseAs`
      Imperative Positive
        (Transitive "eat" (CommonNoun (Just "the") [] "food" Singular Nothing))

  it "parses negative imperative: don't run" $ do
    let exprs = parseControlled grammars "don't run"
    shouldParse exprs
    exprs `shouldParseAs`
      Imperative Negative (Intransitive "run")

  it "parses imperative with PP: run in the park" $ do
    let exprs = parseControlled grammars "run in the park"
    shouldParse exprs
    exprs `shouldParseAs`
      Imperative Positive
        (VPWithAdv
          (Intransitive "run")
          (PrepPhrase "in" (CommonNoun (Just "the") [] "park" Singular Nothing)))
