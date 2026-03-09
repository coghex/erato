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

  it "parses all: all dogs run" $ do
    let exprs = parseControlled grammars "all dogs run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "all") [] "dog" Plural Nothing)
        (Intransitive "run")

  it "parses many: many dogs run" $ do
    let exprs = parseControlled grammars "many dogs run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "many") [] "dog" Plural Nothing)
        (Intransitive "run")

  it "parses more with plural noun: more dogs run" $ do
    let exprs = parseControlled grammars "more dogs run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "more") [] "dog" Plural Nothing)
        (Intransitive "run")

  it "parses degree-modified more: a lot more dogs run" $ do
    let exprs = parseControlled grammars "a lot more dogs run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "more") [] "dog" Plural Nothing)
        (Intransitive "run")

  it "parses fewer with plural noun: fewer dogs run" $ do
    let exprs = parseControlled grammars "fewer dogs run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "fewer") [] "dog" Plural Nothing)
        (Intransitive "run")

  it "parses degree-modified fewer: far fewer dogs run" $ do
    let exprs = parseControlled grammars "far fewer dogs run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "fewer") [] "dog" Plural Nothing)
        (Intransitive "run")

  it "parses less with singular noun object: the dog eats less food" $ do
    let exprs = parseControlled grammars "the dog eats less food"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Transitive "eat" (CommonNoun (Just "less") [] "food" Singular Nothing))

  it "parses degree-modified less: the dog eats much less food" $ do
    let exprs = parseControlled grammars "the dog eats much less food"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Transitive "eat" (CommonNoun (Just "less") [] "food" Singular Nothing))

  it "parses singular no: no dog runs" $ do
    let exprs = parseControlled grammars "no dog runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "no") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses plural no: no dogs run" $ do
    let exprs = parseControlled grammars "no dogs run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "no") [] "dog" Plural Nothing)
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

  it "rejects many with singular: many dog runs" $ do
    let exprs = parseControlled grammars "many dog runs"
    shouldReject exprs

  it "rejects all with singular: all dog runs" $ do
    let exprs = parseControlled grammars "all dog runs"
    shouldReject exprs

  it "rejects fewer with singular: fewer dog runs" $ do
    let exprs = parseControlled grammars "fewer dog runs"
    shouldReject exprs
