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

  it "rejects every with plural: every dogs run" $ do
    let exprs = parseControlled grammars "every dogs run"
    shouldReject exprs
