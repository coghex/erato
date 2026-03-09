{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Modals (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Modals" $ do
  it "parses modal intransitive: the dog can run" $ do
    let exprs = parseControlled grammars "the dog can run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VVComplement "can" (Intransitive "run"))

  it "parses modal transitive: the man can eat the food" $ do
    let exprs = parseControlled grammars "the man can eat the food"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (VVComplement "can"
          (Transitive "eat" (CommonNoun (Just "the") [] "food" Singular Nothing)))

  it "parses modal must: the dog must run" $ do
    let exprs = parseControlled grammars "the dog must run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VVComplement "must" (Intransitive "run"))

  it "parses modal may: the dog may run" $ do
    let exprs = parseControlled grammars "the dog may run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VVComplement "may" (Intransitive "run"))

  it "parses modal should: the dog should run" $ do
    let exprs = parseControlled grammars "the dog should run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VVComplement "should" (Intransitive "run"))

  it "parses modal negation: the dog cannot run" $ do
    let exprs = parseControlled grammars "the dog cannot run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Negative
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VVComplement "can" (Intransitive "run"))

  it "parses modal question: can the dog run" $ do
    let exprs = parseControlled grammars "can the dog run"
    shouldParse exprs
    exprs `shouldParseAs`
      Question Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VVComplement "can" (Intransitive "run"))

  it "parses modal question: should the dog run" $ do
    let exprs = parseControlled grammars "should the dog run"
    shouldParse exprs
    exprs `shouldParseAs`
      Question Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VVComplement "should" (Intransitive "run"))
