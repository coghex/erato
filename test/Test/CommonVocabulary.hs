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
        (CommonNoun (Just "the") [] "cat" Singular)
        (Intransitive "run")

  it "parses: the cats run" $ do
    let exprs = parseControlled grammars "the cats run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "cat" Plural)
        (Intransitive "run")

  it "parses: the man sees the dog" $ do
    let exprs = parseControlled grammars "the man sees the dog"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "man" Singular)
        (Transitive "see" (CommonNoun (Just "the") [] "dog" Singular))

  it "parses: the woman sees the dog" $ do
    let exprs = parseControlled grammars "the woman sees the dog"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "woman" Singular)
        (Transitive "see" (CommonNoun (Just "the") [] "dog" Singular))
