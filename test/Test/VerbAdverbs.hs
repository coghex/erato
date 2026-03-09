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

  it "prefers adverb parse for ambiguous irregular comparative form: the dog runs better" $ do
    parsePreferredControlledSentence grammars "the dog runs better"
      `shouldBe`
        Just
          (Sentence Present Positive
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            (VPWithAdv
              (Intransitive "run")
              (LexicalAdv "better")))

  it "prefers adverb parse for ambiguous irregular comparative form: the dog runs worse" $ do
    parsePreferredControlledSentence grammars "the dog runs worse"
      `shouldBe`
        Just
          (Sentence Present Positive
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            (VPWithAdv
              (Intransitive "run")
              (LexicalAdv "worse")))

  it "prefers adverb parse for ambiguous comparative form: the dog runs harder" $ do
    parsePreferredControlledSentence grammars "the dog runs harder"
      `shouldBe`
        Just
          (Sentence Present Positive
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            (VPWithAdv
              (Intransitive "run")
              (LexicalAdv "harder")))

  it "prefers adverb parse for degree-modified irregular form: the dog runs much better" $ do
    parsePreferredControlledSentence grammars "the dog runs much better"
      `shouldBe`
        Just
          (Sentence Present Positive
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            (VPWithAdv
              (Intransitive "run")
              (LexicalAdv "better")))

  it "prefers adverb parse for degree-modified irregular form: the dog runs far worse" $ do
    parsePreferredControlledSentence grammars "the dog runs far worse"
      `shouldBe`
        Just
          (Sentence Present Positive
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            (VPWithAdv
              (Intransitive "run")
              (ModifiedAdv "far" (LexicalAdv "worse"))))

  it "parses later as a verb adverb: the dog runs later" $ do
    let exprs = parseControlled grammars "the dog runs later"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Intransitive "run")
          (LexicalAdv "later"))

  it "parses sooner as a verb adverb: the dog runs sooner" $ do
    let exprs = parseControlled grammars "the dog runs sooner"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Intransitive "run")
          (LexicalAdv "sooner"))

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
