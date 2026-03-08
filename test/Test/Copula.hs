{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Copula (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Copula (adjective predicate)" $ do
  it "parses copula: the dog is big" $ do
    let exprs = parseControlled grammars "the dog is big"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Copula "big")

  it "parses copula plural: the dogs are big" $ do
    let exprs = parseControlled grammars "the dogs are big"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Plural Nothing)
        (Copula "big")

  it "parses copula with pronoun: she is old" $ do
    let exprs = parseControlled grammars "she is old"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Third Singular Subjective)
        (Copula "old")

  it "parses copula negation: the dog is not big" $ do
    let exprs = parseControlled grammars "the dog is not big"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Negative
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Copula "big")

  it "parses copula past: the dog was big" $ do
    let exprs = parseControlled grammars "the dog was big"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Past Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Copula "big")

  it "parses copula future: the dog will be big" $ do
    let exprs = parseControlled grammars "the dog will be big"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Future Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Copula "big")
