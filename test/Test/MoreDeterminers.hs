{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.MoreDeterminers (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "More determiners" $ do
  it "parses every: every dog runs" $ do
    let exprs = parseControlled grammars "every dog runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "every") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses this: this dog runs" $ do
    let exprs = parseControlled grammars "this dog runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "this") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses that: that dog runs" $ do
    let exprs = parseControlled grammars "that dog runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "that") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses these: these dogs run" $ do
    let exprs = parseControlled grammars "these dogs run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "these") [] "dog" Plural Nothing)
        (Intransitive "run")

  it "parses those: those dogs run" $ do
    let exprs = parseControlled grammars "those dogs run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "those") [] "dog" Plural Nothing)
        (Intransitive "run")

  it "parses possessive determiner singular: my dog runs" $ do
    let exprs = parseControlled grammars "my dog runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "my") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses possessive determiner plural: their dogs run" $ do
    let exprs = parseControlled grammars "their dogs run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "their") [] "dog" Plural Nothing)
        (Intransitive "run")

  it "rejects plural demonstrative with singular noun: these dog run" $ do
    let exprs = parseControlled grammars "these dog run"
    shouldReject exprs

  it "rejects every with plural: every dogs run" $ do
    let exprs = parseControlled grammars "every dogs run"
    shouldReject exprs
