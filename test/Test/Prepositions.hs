{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Prepositions (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Prepositions / adverbs" $ do
  it "parses intransitive with PP: the dog runs in the park" $ do
    let exprs = parseControlled grammars "the dog runs in the park"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular)
        (VPWithAdv
          (Intransitive "run")
          (PrepPhrase "in" (CommonNoun (Just "the") [] "park" Singular)))

  it "parses transitive with PP: the woman eats the food with a spoon" $ do
    let exprs = parseControlled grammars "the woman eats the food with a spoon"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "woman" Singular)
        (VPWithAdv
          (Transitive "eat" (CommonNoun (Just "the") [] "food" Singular))
          (PrepPhrase "with" (CommonNoun (Just "a") [] "spoon" Singular)))

  it "parses plural PP: the dogs run under the tables" $ do
    let exprs = parseControlled grammars "the dogs run under the tables"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Plural)
        (VPWithAdv
          (Intransitive "run")
          (PrepPhrase "under" (CommonNoun (Just "the") [] "table" Plural)))

  it "parses negation with PP: the dog does not run in the park" $ do
    let exprs = parseControlled grammars "the dog does not run in the park"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Negative
        (CommonNoun (Just "the") [] "dog" Singular)
        (VPWithAdv
          (Intransitive "run")
          (PrepPhrase "in" (CommonNoun (Just "the") [] "park" Singular)))

  it "parses pronoun with PP: I run with them" $ do
    let exprs = parseControlled grammars "I run with them"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun First Singular Subjective)
        (VPWithAdv
          (Intransitive "run")
          (PrepPhrase "with" (Pronoun Third Plural Objective)))

  it "parses multiple PPs: the dog runs in the park with the woman" $ do
    let exprs = parseControlled grammars "the dog runs in the park with the woman"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular)
        (VPWithAdv
          (VPWithAdv
            (Intransitive "run")
            (PrepPhrase "in" (CommonNoun (Just "the") [] "park" Singular)))
          (PrepPhrase "with" (CommonNoun (Just "the") [] "woman" Singular)))
