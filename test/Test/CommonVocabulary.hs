{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.CommonVocabulary (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Common vocabulary" $ do
  it "parses: the cat runs" $ do
    let exprs = parseControlled grammars "the cat runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "cat" Singular Nothing)
        (Intransitive "run")

  it "parses: the cats run" $ do
    let exprs = parseControlled grammars "the cats run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "cat" Plural Nothing)
        (Intransitive "run")

  it "parses: the man sees the dog" $ do
    let exprs = parseControlled grammars "the man sees the dog"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (Transitive "see" (CommonNoun (Just "the") [] "dog" Singular Nothing))

  it "parses: the woman sees the dog" $ do
    let exprs = parseControlled grammars "the woman sees the dog"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "woman" Singular Nothing)
        (Transitive "see" (CommonNoun (Just "the") [] "dog" Singular Nothing))

  it "parses sentence-initial capitalization: Dog runs" $ do
    parsePreferredControlledSentence grammars "Dog runs"
      `shouldBe`
        Just
          (Sentence Present Positive
            (CommonNoun Nothing [] "dog" Singular Nothing)
            (Intransitive "run"))

  it "parses sentence-initial capitalization for determiners: The dog runs" $ do
    parsePreferredControlledSentence grammars "The dog runs"
      `shouldBe`
        Just
          (Sentence Present Positive
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            (Intransitive "run"))

  it "parses lowercase first word: dog runs" $ do
    let exprs = parseControlled grammars "dog runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun Nothing [] "dog" Singular Nothing)
        (Intransitive "run")
