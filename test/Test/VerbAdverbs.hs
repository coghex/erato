{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.VerbAdverbs (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Verb adverbs" $ do
  it "parses comparative adverb on intransitive verb: the dog runs faster" $ do
    let exprs = parseControlled grammars "the dog runs faster"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Intransitive "run")
          (LexicalAdv "faster"))

  it "prefers adverb parse for ambiguous comparative object form: the dog runs faster" $ do
    parsePreferredControlledSentence grammars "the dog runs faster"
      `shouldBe`
        Just
          (Sentence Present Positive
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            (VPWithAdv
              (Intransitive "run")
              (LexicalAdv "faster")))

  it "parses degree-modified comparative adverb: the dog runs much faster" $ do
    let exprs = parseControlled grammars "the dog runs much faster"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Intransitive "run")
          (LexicalAdv "faster"))

  it "prefers adverb parse for degree-modified comparative form: the dog runs much faster" $ do
    parsePreferredControlledSentence grammars "the dog runs much faster"
      `shouldBe`
        Just
          (Sentence Present Positive
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            (VPWithAdv
              (Intransitive "run")
              (LexicalAdv "faster")))

  it "parses reduced-speed lexical adverb: the man eats the food less quickly" $ do
    let exprs = parseControlled grammars "the man eats the food less quickly"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (VPWithAdv
          (Transitive "eat" (CommonNoun (Just "the") [] "food" Singular Nothing))
          (ModifiedAdv "less" (LexicalAdv "quick")))

  it "parses degree-modified reduced-speed adverb: the man eats the food far less quickly" $ do
    let exprs = parseControlled grammars "the man eats the food far less quickly"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (VPWithAdv
          (Transitive "eat" (CommonNoun (Just "the") [] "food" Singular Nothing))
          (ModifiedAdv "less" (LexicalAdv "quick")))
