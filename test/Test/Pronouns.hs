{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Pronouns (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Pronouns" $ do
  it "parses pronoun subject: I run" $ do
    let exprs = parseControlled grammars "I run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun First Singular Subjective)
        (Intransitive "run")

  it "parses pronoun subject: we run" $ do
    let exprs = parseControlled grammars "we run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun First Plural Subjective)
        (Intransitive "run")

  it "parses pronoun subject: they run" $ do
    let exprs = parseControlled grammars "they run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Third Plural Subjective)
        (Intransitive "run")

  it "parses pronoun object: the man eats her" $ do
    let exprs = parseControlled grammars "the man eats her"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (Transitive "eat" (Pronoun Third Singular Objective))
