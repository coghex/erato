{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.ProperNouns (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ Spec
spec = describe "Proper nouns (unknown words)" $ do
  it "parses unknown subject: iPhone runs" $ do
    withGrammars $ \grammars -> do
      let exprs = parseControlled grammars "iPhone runs"
      shouldParse exprs
      exprs `shouldParseAs`
        Sentence Present Positive
          (ProperNoun "iPhone")
          (Intransitive "run")

  it "parses unknown subject: eBay eats the food" $ do
    withGrammars $ \grammars -> do
      let exprs = parseControlled grammars "eBay eats the food"
      shouldParse exprs
      exprs `shouldParseAs`
        Sentence Present Positive
          (ProperNoun "eBay")
          (Transitive "eat" (CommonNoun (Just "the") [] "food" Singular))

  it "parses unknown object: the man eats iPhone" $ do
    withGrammars $ \grammars -> do
      let exprs = parseControlled grammars "the man eats iPhone"
      shouldParse exprs
      exprs `shouldParseAs`
        Sentence Present Positive
          (CommonNoun (Just "the") [] "man" Singular)
          (Transitive "eat" (ProperNoun "iPhone"))
