{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Agreement (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Agreement" $ do
  it "parses singular agreement: the dog runs" $ do
    let exprs = parseControlled grammars "the dog runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses plural agreement: the dogs run" $ do
    let exprs = parseControlled grammars "the dogs run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Plural Nothing)
        (Intransitive "run")

  it "rejects mismatch: the dogs runs" $ do
    let exprs = parseControlled grammars "the dogs runs"
    shouldReject exprs

  it "rejects mismatch: the dog run" $ do
    let exprs = parseControlled grammars "the dog run"
    shouldReject exprs

  it "allows past-tense singular without extra agreement changes: the dog ran" $ do
    let exprs = parseControlled grammars "the dog ran"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Past Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "allows future-tense plural without extra agreement changes: the dogs will run" $ do
    let exprs = parseControlled grammars "the dogs will run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Future Positive
        (CommonNoun (Just "the") [] "dog" Plural Nothing)
        (Intransitive "run")
