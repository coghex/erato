{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Questions (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Questions" $ do
  it "parses present yes/no question: does the dog run" $ do
    let exprs = parseControlled grammars "does the dog run"
    shouldParse exprs
    exprs `shouldParseAs`
      Question Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses plural yes/no question: do the dogs run" $ do
    let exprs = parseControlled grammars "do the dogs run"
    shouldParse exprs
    exprs `shouldParseAs`
      Question Present Positive
        (CommonNoun (Just "the") [] "dog" Plural Nothing)
        (Intransitive "run")

  it "parses transitive past question: did the man eat the food" $ do
    let exprs = parseControlled grammars "did the man eat the food"
    shouldParse exprs
    exprs `shouldParseAs`
      Question Past Positive
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (Transitive "eat" (CommonNoun (Just "the") [] "food" Singular Nothing))

  it "parses future question: will the dog run" $ do
    let exprs = parseControlled grammars "will the dog run"
    shouldParse exprs
    exprs `shouldParseAs`
      Question Future Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses copula question: is the dog big" $ do
    let exprs = parseControlled grammars "is the dog big"
    shouldParse exprs
    exprs `shouldParseAs`
      Question Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Copula "big")

  it "parses negative question: does the dog not run" $ do
    let exprs = parseControlled grammars "does the dog not run"
    shouldParse exprs
    exprs `shouldParseAs`
      Question Present Negative
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Intransitive "run")
