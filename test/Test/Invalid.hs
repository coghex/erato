{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Invalid (spec) where

import Test.Hspec

import Parser.GFParser
import Test.Utils

spec ∷ Spec
spec = describe "Invalid sentences" $ do
  it "rejects object case as subject: them eats the food" $ do
    withGrammars $ \grammars -> do
      let exprs = parseControlled grammars "them eats the food"
      shouldReject exprs

  it "rejects agreement mismatch: the dogs runs" $ do
    withGrammars $ \grammars -> do
      let exprs = parseControlled grammars "the dogs runs"
      shouldReject exprs

  it "rejects missing verb: the dog the food" $ do
    withGrammars $ \grammars -> do
      let exprs = parseControlled grammars "the dog the food"
      shouldReject exprs
