{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Agreement (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ Spec
spec = describe "Agreement" $ do
  it "parses singular agreement: the dog runs" $ do
    withGrammars $ \grammars -> do
      let exprs = parseControlled grammars "the dog runs"
      shouldParse exprs
      exprs `shouldParseAs`
        Sentence Present Positive
          (CommonNoun (Just "the") [] "dog" Singular)
          (Intransitive "run")

  it "parses plural agreement: the dogs run" $ do
    withGrammars $ \grammars -> do
      let exprs = parseControlled grammars "the dogs run"
      shouldParse exprs
      exprs `shouldParseAs`
        Sentence Present Positive
          (CommonNoun (Just "the") [] "dog" Plural)
          (Intransitive "run")

  it "rejects mismatch: the dogs runs" $ do
    withGrammars $ \grammars -> do
      let exprs = parseControlled grammars "the dogs runs"
      shouldReject exprs

  it "rejects mismatch: the dog run" $ do
    withGrammars $ \grammars -> do
      let exprs = parseControlled grammars "the dog run"
      shouldReject exprs
