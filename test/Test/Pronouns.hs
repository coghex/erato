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

  it "parses lowercase first pronoun subject: i run" $ do
    let exprs = parseControlled grammars "i run"
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

  it "parses pronoun subject: he runs" $ do
    let exprs = parseControlled grammars "he runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Third Singular Subjective)
        (Intransitive "run")

  it "parses pronoun subject: she runs" $ do
    let exprs = parseControlled grammars "she runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Third Singular Subjective)
        (Intransitive "run")

  it "parses pronoun subject: it runs" $ do
    let exprs = parseControlled grammars "it runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Third Singular Subjective)
        (Intransitive "run")

  it "parses pronoun subject: you run" $ do
    let exprs = parseControlled grammars "you run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Second Singular Subjective)
        (Intransitive "run")

  it "parses plural-you subject: you run" $ do
    let exprs = parseControlled grammars "you run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Second Plural Subjective)
        (Intransitive "run")

  it "parses archaic plural-you subject: ye run" $ do
    let exprs = parseControlled grammars "ye run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Second Plural Subjective)
        (Intransitive "run")

  it "normalizes archaic second-singular agreement: thou belongest to that hopeless tribe" $ do
    let exprs = parseControlled grammars "thou belongest to that hopeless tribe"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Second Singular Subjective)
        (VPWithAdv
          (Intransitive "belong")
          (PrepPhrase "to" (CommonNoun (Just "that") ["hopeless"] "tribe" Singular Nothing)))

  it "parses pronoun object: the dog eats me" $ do
    let exprs = parseControlled grammars "the dog eats me"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Transitive "eat" (Pronoun First Singular Objective))

  it "parses pronoun object: the dog eats us" $ do
    let exprs = parseControlled grammars "the dog eats us"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Transitive "eat" (Pronoun First Plural Objective))

  it "parses pronoun object: the dog eats him" $ do
    let exprs = parseControlled grammars "the dog eats him"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Transitive "eat" (Pronoun Third Singular Objective))

  it "parses plural-you object: the dog eats you" $ do
    let exprs = parseControlled grammars "the dog eats you"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Transitive "eat" (Pronoun Second Plural Objective))
