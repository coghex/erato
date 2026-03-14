{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Imperative (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Imperatives" $ do
  it "parses imperative: run" $ do
    let exprs = parseControlled grammars "run"
    shouldParse exprs
    exprs `shouldParseAs`
      Imperative Positive (Intransitive "run")

  it "parses imperative transitive: eat the food" $ do
    let exprs = parseControlled grammars "eat the food"
    shouldParse exprs
    exprs `shouldParseAs`
      Imperative Positive
        (Transitive "eat" (CommonNoun (Just "the") [] "food" Singular Nothing))

  it "parses negative imperative: don't run" $ do
    let exprs = parseControlled grammars "don't run"
    shouldParse exprs
    exprs `shouldParseAs`
      Imperative Negative (Intransitive "run")

  it "parses imperative with PP: run in the park" $ do
    let exprs = parseControlled grammars "run in the park"
    shouldParse exprs
    exprs `shouldParseAs`
      Imperative Positive
        (VPWithAdv
          (Intransitive "run")
          (PrepPhrase "in" (CommonNoun (Just "the") [] "park" Singular Nothing)))

  it "parses imperative with lexical adverb: run well" $ do
    let exprs = parseControlled grammars "run well"
    shouldParse exprs
    exprs `shouldParseAs`
      Imperative Positive
        (VPWithAdv
          (Intransitive "run")
          (LexicalAdv "well"))

  it "parses archaic farewell idiom: fare thee well" $ do
    let exprs = parseControlled grammars "fare thee well"
    shouldParse exprs
    exprs `shouldParseAs`
      Imperative Positive
        (VPWithAdv
          (Transitive "fare" (Pronoun Second Singular Objective))
          (LexicalAdv "well"))

  it "parses imperative with leading so and vocative: so fare thee well, poor devil of a Sub-Sub" $ do
    let exprs = parseControlled grammars "so fare thee well, poor devil of a Sub-Sub"
    shouldParse exprs
    exprs `shouldParseAs`
      Vocative
        (Imperative Positive
          (VPWithAdv
            (VPWithAdv
              (Transitive "fare" (Pronoun Second Singular Objective))
              (LexicalAdv "well"))
            (LexicalAdv "so")))
        (CommonNoun Nothing ["poor"] "devil" Singular
          (Just (RelPrep "of" (CommonNoun (Just "a") [] "sub-sub" Singular Nothing))))

  it "parses the Moby-Dick farewell sentence" $ do
    let exprs = parseControlled grammars "So fare thee well, poor devil of a Sub-Sub, whose commentator I am."
    shouldParse exprs

  it "parses imperative particle adverb: give it up" $ do
    let exprs = parseControlled grammars "give it up"
    shouldParse exprs
    exprs `shouldParseAs`
      Imperative Positive
        (VPWithAdv
          (Transitive "give" (Pronoun Third Singular Objective))
          (LexicalAdv "up"))

  it "parses the Moby-Dick give-it-up vocative" $ do
    let exprs = parseControlled grammars "Give it up, Sub-Subs!"
    shouldParse exprs
    exprs `shouldParseAs`
      Vocative
        (Imperative Positive
          (VPWithAdv
            (Transitive "give" (Pronoun Third Singular Objective))
            (LexicalAdv "up")))
        (CommonNoun Nothing [] "sub-sub" Plural Nothing)

  it "prefers particle-verb imperative parse: gulp down your tears" $ do
    parsePreferredControlledSentence grammars "gulp down your tears"
      `shouldBe`
        Just
          (Imperative Positive
            (Transitive "gulp down"
              (CommonNoun (Just "your") [] "tear" Plural Nothing)))

  it "parses the Moby-Dick gulp-and-hie imperative" $ do
    let exprs = parseControlled grammars "But gulp down your tears and hie aloft to the royal-mast with your hearts"
    shouldParse exprs
