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

  it "parses comparative copula: the dog is bigger than the cat" $ do
    let exprs = parseControlled grammars "the dog is bigger than the cat"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Copula "bigger")
          (PrepPhrase "than" (CommonNoun (Just "the") [] "cat" Singular Nothing)))

  it "parses comparative copula question: is the dog bigger than the cat" $ do
    let exprs = parseControlled grammars "is the dog bigger than the cat"
    shouldParse exprs
    exprs `shouldParseAs`
      Question Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Copula "bigger")
          (PrepPhrase "than" (CommonNoun (Just "the") [] "cat" Singular Nothing)))

  it "parses degree-modified comparative copula: the dog is much bigger than the cat" $ do
    let exprs = parseControlled grammars "the dog is much bigger than the cat"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Copula "bigger")
          (PrepPhrase "than" (CommonNoun (Just "the") [] "cat" Singular Nothing)))

  it "parses degree-modified comparative question: is the dog slightly bigger than the cat" $ do
    let exprs = parseControlled grammars "is the dog slightly bigger than the cat"
    shouldParse exprs
    exprs `shouldParseAs`
      Question Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Copula "bigger")
          (PrepPhrase "than" (CommonNoun (Just "the") [] "cat" Singular Nothing)))
