{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Existential (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Existential sentences" $ do
  it "parses existential: there is a dog" $ do
    let exprs = parseControlled grammars "there is a dog"
    shouldParse exprs
    exprs `shouldParseAs`
      Existential Present Positive
        (CommonNoun (Just "a") [] "dog" Singular Nothing)

  it "parses existential plural: there are some dogs" $ do
    let exprs = parseControlled grammars "there are some dogs"
    shouldParse exprs
    exprs `shouldParseAs`
      Existential Present Positive
        (CommonNoun (Just "some") [] "dog" Plural Nothing)

  it "parses existential negation: there is not a dog" $ do
    let exprs = parseControlled grammars "there is not a dog"
    shouldParse exprs
    exprs `shouldParseAs`
      Existential Present Negative
        (CommonNoun (Just "a") [] "dog" Singular Nothing)

  it "parses existential past: there was a dog" $ do
    let exprs = parseControlled grammars "there was a dog"
    shouldParse exprs
    exprs `shouldParseAs`
      Existential Past Positive
        (CommonNoun (Just "a") [] "dog" Singular Nothing)

  it "parses existential with adjective: there is a big dog" $ do
    let exprs = parseControlled grammars "there is a big dog"
    shouldParse exprs
    exprs `shouldParseAs`
      Existential Present Positive
        (CommonNoun (Just "a") ["big"] "dog" Singular Nothing)
