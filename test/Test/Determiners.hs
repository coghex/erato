{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Determiners (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ Spec
spec = describe "Determiners" $ do
  it "parses singular determiner: a dog runs" $ do
    withGrammars $ \grammars -> do
      let exprs = parseControlled grammars "a dog runs"
      shouldParse exprs
      exprs `shouldParseAs`
        Sentence Present Positive
          (CommonNoun (Just "a") [] "dog" Singular)
          (Intransitive "run")

  it "parses plural determiner: some dogs run" $ do
    withGrammars $ \grammars -> do
      let exprs = parseControlled grammars "some dogs run"
      shouldParse exprs
      exprs `shouldParseAs`
        Sentence Present Positive
          (CommonNoun (Just "some") [] "dog" Plural)
          (Intransitive "run")

  it "rejects mismatch: a dogs run" $ do
    withGrammars $ \grammars -> do
      let exprs = parseControlled grammars "a dogs run"
      shouldReject exprs
