{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Tense (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Tense" $ do
  it "parses past tense: the dog ran" $ do
    let exprs = parseControlled grammars "the dog ran"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Past Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses past tense with pronoun: I ran" $ do
    let exprs = parseControlled grammars "I ran"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Past Positive
        (Pronoun First Singular Subjective)
        (Intransitive "run")

  it "parses past tense transitive: the man ate the food" $ do
    let exprs = parseControlled grammars "the man ate the food"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Past Positive
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (Transitive "eat" (CommonNoun (Just "the") [] "food" Singular Nothing))

  it "parses past tense plural: the dogs ran" $ do
    let exprs = parseControlled grammars "the dogs ran"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Past Positive
        (CommonNoun (Just "the") [] "dog" Plural Nothing)
        (Intransitive "run")

  it "parses past tense negation: the dog did not run" $ do
    let exprs = parseControlled grammars "the dog did not run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Past Negative
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses future tense: the dog will run" $ do
    let exprs = parseControlled grammars "the dog will run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Future Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses future tense transitive: the man will eat the food" $ do
    let exprs = parseControlled grammars "the man will eat the food"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Future Positive
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (Transitive "eat" (CommonNoun (Just "the") [] "food" Singular Nothing))

  it "parses future tense negation: the dog will not run" $ do
    let exprs = parseControlled grammars "the dog will not run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Future Negative
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "prefers the future parse for: the dog will run" $ do
    parsePreferredControlledSentence grammars "the dog will run"
      `shouldBe`
        Just
          (Sentence Future Positive
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            (Intransitive "run"))

  it "prefers the future parse for: I will run" $ do
    parsePreferredControlledSentence grammars "I will run"
      `shouldBe`
        Just
          (Sentence Future Positive
            (Pronoun First Singular Subjective)
            (Intransitive "run"))
