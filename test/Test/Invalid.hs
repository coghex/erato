{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Invalid (spec) where

import Test.Hspec

import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Invalid sentences" $ do
  it "rejects object case as subject: them eats the food" $ do
    let exprs = parseControlled grammars "them eats the food"
    shouldReject exprs

  it "rejects agreement mismatch: the dogs runs" $ do
    let exprs = parseControlled grammars "the dogs runs"
    shouldReject exprs

  it "rejects missing verb: the dog the food" $ do
    let exprs = parseControlled grammars "the dog the food"
    shouldReject exprs

  it "rejects subject case as object: the man eats they" $ do
    let exprs = parseControlled grammars "the man eats they"
    shouldReject exprs

  it "rejects plural agreement with singular: the dog run" $ do
    let exprs = parseControlled grammars "the dog run"
    shouldReject exprs

  it "rejects double determiner: the the dog runs" $ do
    let exprs = parseControlled grammars "the the dog runs"
    shouldReject exprs

  it "rejects empty input" $ do
    let exprs = parseControlled grammars ""
    shouldReject exprs
