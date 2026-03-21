{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Progressive (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Progressive aspect" $ do
  it "parses progressive: the dog is running" $ do
    let exprs = parseControlled grammars "the dog is running"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Progressive (Intransitive "run"))

  it "parses progressive plural: the dogs are running" $ do
    let exprs = parseControlled grammars "the dogs are running"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Plural Nothing)
        (Progressive (Intransitive "run"))

  it "parses progressive transitive: the man is eating the food" $ do
    let exprs = parseControlled grammars "the man is eating the food"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (Progressive (Transitive "eat" (CommonNoun (Just "the") [] "food" Singular Nothing)))

  it "parses progressive past: the dog was running" $ do
    let exprs = parseControlled grammars "the dog was running"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Past Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Progressive (Intransitive "run"))

  it "parses progressive negation: the dog is not running" $ do
    let exprs = parseControlled grammars "the dog is not running"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Negative
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Progressive (Intransitive "run"))

  it "parses progressive with pronoun: I am running" $ do
    let exprs = parseControlled grammars "I am running"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun First Singular Subjective)
        (Progressive (Intransitive "run"))

  it "parses sentence-initial contraction: i'm running" $ do
    let exprs = parseControlled grammars "i'm running"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun First Singular Subjective)
        (Progressive (Intransitive "run"))

  it "parses the corpus-shaped progressive breakup clause: the icebound stream of Time is breaking up" $ do
    let exprs = parseControlled grammars "the icebound stream of Time is breaking up"
    shouldParse exprs

  it "parses corpus-shaped progressive with ever and lexicons" $ do
    let exprs = parseControlled grammars "he was ever dusting his old lexicons"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Past Positive
        (Pronoun Third Singular Subjective)
        (Progressive
          (VPWithAdv
            (Transitive "dust"
              (CommonNoun (Just "his") ["old"] "lexicon" Plural Nothing))
            (LexicalAdv "ever")))

  it "parses blocker #1 full sentence with embellished handkerchief PP intact" $ do
    parsePreferredControlledSentence grammars
      "He was ever dusting his old lexicons and grammars, with a queer handkerchief, mockingly embellished with all the gay flags of all the known nations of the world"
      `shouldBe`
        Just
          (Sentence Past Positive
            (Pronoun Third Singular Subjective)
            (VPWithAdv
              (Progressive
                (Transitive "dust"
                  (CoordNP And
                    (CommonNoun (Just "his") ["old"] "lexicon" Plural Nothing)
                    (CommonNoun Nothing [] "grammar" Plural Nothing))))
              (LexicalAdv "ever")))
