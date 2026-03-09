{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Adjectives (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Adjectives" $ do
  it "parses adjective: the red dog runs" $ do
    let exprs = parseControlled grammars "the red dog runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") ["red"] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses adjective with plural: the red dogs run" $ do
    let exprs = parseControlled grammars "the red dogs run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") ["red"] "dog" Plural Nothing)
        (Intransitive "run")

  it "parses multiple adjectives: the red dog eats the red food" $ do
    let exprs = parseControlled grammars "the red dog eats the red food"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") ["red"] "dog" Singular Nothing)
        (Transitive "eat" (CommonNoun (Just "the") ["red"] "food" Singular Nothing))

  it "parses comparative adjective in NP: the bigger dog runs" $ do
    let exprs = parseControlled grammars "the bigger dog runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") ["bigger"] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses superlative adjective in NP: the biggest dog runs" $ do
    let exprs = parseControlled grammars "the biggest dog runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") ["biggest"] "dog" Singular Nothing)
        (Intransitive "run")
