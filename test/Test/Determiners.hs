{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Determiners (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Determiners" $ do
  it "parses singular determiner: a dog runs" $ do
    let exprs = parseControlled grammars "a dog runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "a") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses plural determiner: some dogs run" $ do
    let exprs = parseControlled grammars "some dogs run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "some") [] "dog" Plural Nothing)
        (Intransitive "run")

  it "rejects mismatch: a dogs run" $ do
    let exprs = parseControlled grammars "a dogs run"
    shouldReject exprs
