{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.ProperNouns (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Proper nouns (unknown words)" $ do
  it "parses unknown subject: iPhone runs" $ do
    let exprs = parseControlled grammars "iPhone runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (ProperNoun "iPhone")
        (Intransitive "run")

  it "parses unknown subject: eBay eats the food" $ do
    let exprs = parseControlled grammars "eBay eats the food"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (ProperNoun "eBay")
        (Transitive "eat" (CommonNoun (Just "the") [] "food" Singular Nothing))

  it "parses unknown object: the man eats iPhone" $ do
    let exprs = parseControlled grammars "the man eats iPhone"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (Transitive "eat" (ProperNoun "iPhone"))

  it "keeps unknown sentence-initial capitalization as proper noun: Zorg runs" $ do
    parsePreferredControlledSentence grammars "Zorg runs"
      `shouldBe`
        Just
          (Sentence Present Positive
            (ProperNoun "Zorg")
            (Intransitive "run"))
