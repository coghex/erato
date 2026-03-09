{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Contractions (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Contractions" $ do
  it "parses modal negation contraction: the dog can't run" $ do
    let exprs = parseControlled grammars "the dog can't run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Negative
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VVComplement "can" (Intransitive "run"))

  it "parses future negation contraction: the dog won't run" $ do
    let exprs = parseControlled grammars "the dog won't run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Future Negative
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses copular contraction: he's big" $ do
    let exprs = parseControlled grammars "he's big"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Third Singular Subjective)
        (Copula "big")

  it "parses perfect contraction: they've run" $ do
    let exprs = parseControlled grammars "they've run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Perfect Positive
        (Pronoun Third Plural Subjective)
        (Intransitive "run")
