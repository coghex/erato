{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Coordination (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Coordination" $ do
  it "parses coordinated subjects: the man and the woman run" $ do
    let exprs = parseControlled grammars "the man and the woman run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CoordNP And
          (CommonNoun (Just "the") [] "man" Singular Nothing)
          (CommonNoun (Just "the") [] "woman" Singular Nothing))
        (Intransitive "run")

  it "parses coordinated verbs: the dog runs and eats" $ do
    let exprs = parseControlled grammars "the dog runs and eats"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (CoordVP And
          (Intransitive "run")
          (Intransitive "eat"))

  it "parses coordinated verbs with object: the dog eats the food and runs" $ do
    let exprs = parseControlled grammars "the dog eats the food and runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (CoordVP And
          (Transitive "eat" (CommonNoun (Just "the") [] "food" Singular Nothing))
          (Intransitive "run"))

  it "parses or-coordinated subjects: the man or the woman run" $ do
    let exprs = parseControlled grammars "the man or the woman run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CoordNP Or
          (CommonNoun (Just "the") [] "man" Singular Nothing)
          (CommonNoun (Just "the") [] "woman" Singular Nothing))
        (Intransitive "run")

  it "parses or-coordinated verbs: the dog runs or eats" $ do
    let exprs = parseControlled grammars "the dog runs or eats"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (CoordVP Or
          (Intransitive "run")
          (Intransitive "eat"))
