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

  it "parses superlative noun phrase with PP complement: the biggest dog in the park runs" $ do
    let exprs = parseControlled grammars "the biggest dog in the park runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") ["biggest"] "dog" Singular
          (Just (RelPrep "in" (CommonNoun (Just "the") [] "park" Singular Nothing))))
        (Intransitive "run")

  it "parses hyphenated adjective in NP: the higgledy-piggledy whale runs" $ do
    let exprs = parseControlled grammars "the higgledy-piggledy whale runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") ["higgledy-piggledy"] "whale" Singular Nothing)
        (Intransitive "run")

  it "parses however-authentic as a postnominal adjective phrase" $ do
    let exprs = parseControlled grammars "the statements however authentic run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "statement" Plural
          (Just (PostAdj (ModifiedAdj "however" (BareAdj "authentic")))))
        (Intransitive "run")

  it "parses gospel as a curated attributive adjective: the veritable gospel cetology runs" $ do
    let exprs = parseControlled grammars "the veritable gospel cetology runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") ["veritable", "gospel"] "cetology" Singular Nothing)
        (Intransitive "run")

  it "parses negated attributive adjective phrase: the not unpleasant sadness runs" $ do
    let exprs = parseControlled grammars "the not unpleasant sadness runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") ["not", "unpleasant"] "sadness" Singular Nothing)
        (Intransitive "run")

  it "parses modified negated attributive adjective phrase: the not altogether unpleasant sadness runs" $ do
    let exprs = parseControlled grammars "the not altogether unpleasant sadness runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") ["not", "altogether", "unpleasant"] "sadness" Singular Nothing)
        (Intransitive "run")
