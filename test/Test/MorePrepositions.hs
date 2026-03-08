{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.MorePrepositions (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "More prepositions" $ do
  it "parses on: the cat runs on the table" $ do
    let exprs = parseControlled grammars "the cat runs on the table"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "cat" Singular Nothing)
        (VPWithAdv
          (Intransitive "run")
          (PrepPhrase "on" (CommonNoun (Just "the") [] "table" Singular Nothing)))

  it "parses to: the man runs to the park" $ do
    let exprs = parseControlled grammars "the man runs to the park"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (VPWithAdv
          (Intransitive "run")
          (PrepPhrase "to" (CommonNoun (Just "the") [] "park" Singular Nothing)))

  it "parses from: the dog runs from the woman" $ do
    let exprs = parseControlled grammars "the dog runs from the woman"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Intransitive "run")
          (PrepPhrase "from" (CommonNoun (Just "the") [] "woman" Singular Nothing)))

  it "parses for: the man eats the food for the woman" $ do
    let exprs = parseControlled grammars "the man eats the food for the woman"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (VPWithAdv
          (Transitive "eat" (CommonNoun (Just "the") [] "food" Singular Nothing))
          (PrepPhrase "for" (CommonNoun (Just "the") [] "woman" Singular Nothing)))

  it "parses by: the food is eaten by the dog" $ do
    let exprs = parseControlled grammars "the man eats by the dog"
    shouldParse exprs

  it "parses before: the man runs before the woman" $ do
    let exprs = parseControlled grammars "the man runs before the woman"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (VPWithAdv
          (Intransitive "run")
          (PrepPhrase "before" (CommonNoun (Just "the") [] "woman" Singular Nothing)))

  it "parses after: the man eats after the woman" $ do
    let exprs = parseControlled grammars "the man eats after the woman"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (VPWithAdv
          (Intransitive "eat")
          (PrepPhrase "after" (CommonNoun (Just "the") [] "woman" Singular Nothing)))
