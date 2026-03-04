{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Negation (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Negation" $ do
  it "parses negation: the dog does not run" $ do
    let exprs = parseControlled grammars "the dog does not run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Negative
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses negation: I do not run" $ do
    let exprs = parseControlled grammars "I do not run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Negative
        (Pronoun First Singular Subjective)
        (Intransitive "run")

  it "parses negation: the man does not eat the food" $ do
    let exprs = parseControlled grammars "the man does not eat the food"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Negative
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (Transitive "eat" (CommonNoun (Just "the") [] "food" Singular Nothing))

  it "parses negation: the dogs do not run" $ do
    let exprs = parseControlled grammars "the dogs do not run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Negative
        (CommonNoun (Just "the") [] "dog" Plural Nothing)
        (Intransitive "run")
