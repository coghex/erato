{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Passive (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Passive voice" $ do
  it "parses passive: the food is eaten" $ do
    let exprs = parseControlled grammars "the food is eaten"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "food" Singular Nothing)
        (Passive "eat")

  it "parses passive plural: the dogs are seen" $ do
    let exprs = parseControlled grammars "the dogs are seen"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Plural Nothing)
        (Passive "see")

  it "parses passive past: the food was eaten" $ do
    let exprs = parseControlled grammars "the food was eaten"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Past Positive
        (CommonNoun (Just "the") [] "food" Singular Nothing)
        (Passive "eat")

  it "parses passive negation: the food is not eaten" $ do
    let exprs = parseControlled grammars "the food is not eaten"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Negative
        (CommonNoun (Just "the") [] "food" Singular Nothing)
        (Passive "eat")

  it "parses passive with pronoun: he is seen" $ do
    let exprs = parseControlled grammars "he is seen"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Third Singular Subjective)
        (Passive "see")
