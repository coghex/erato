{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.RelativeClauses (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Relative clauses" $ do
  it "parses subject relative: the man who runs eats" $ do
    let exprs = parseControlled grammars "the man who runs eats"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "man" Singular
          (Just (RelVP (Intransitive "run"))))
        (Intransitive "eat")

  it "parses object relative: the dog that the woman sees runs" $ do
    let exprs = parseControlled grammars "the dog that the woman sees runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular
          (Just (RelV2 "see" (CommonNoun (Just "the") [] "woman" Singular Nothing))))
        (Intransitive "run")

  it "parses plural relative: the dogs that the man sees run" $ do
    let exprs = parseControlled grammars "the dogs that the man sees run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Plural
          (Just (RelV2 "see" (CommonNoun (Just "the") [] "man" Singular Nothing))))
        (Intransitive "run")

  it "parses relative with adjective: the red dog that runs eats" $ do
    let exprs = parseControlled grammars "the red dog that runs eats"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") ["red"] "dog" Singular
          (Just (RelVP (Intransitive "run"))))
        (Intransitive "eat")

  it "parses relative with PP: the man who runs in the park eats" $ do
    let exprs = parseControlled grammars "the man who runs in the park eats"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "man" Singular
          (Just (RelVP
            (VPWithAdv
              (Intransitive "run")
              (PrepPhrase "in" (CommonNoun (Just "the") [] "park" Singular Nothing))))))
        (Intransitive "eat")

  it "parses object relative with adjective: the red dog that the man sees runs" $ do
    let exprs = parseControlled grammars "the red dog that the man sees runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") ["red"] "dog" Singular
          (Just (RelV2 "see" (CommonNoun (Just "the") [] "man" Singular Nothing))))
        (Intransitive "run")

  it "parses negated subject relative: the dog that does not run is big" $ do
    let exprs = parseControlled grammars "the dog that does not run is big"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular
          (Just (NegRelVP (Intransitive "run"))))
        (Copula "big")

  it "parses negated object relative: the dog that the woman does not see runs" $ do
    let exprs = parseControlled grammars "the dog that the woman does not see runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular
          (Just (NegRelV2 "see" (CommonNoun (Just "the") [] "woman" Singular Nothing))))
        (Intransitive "run")

  it "parses negated copula relative: the dog that is not big runs" $ do
    let exprs = parseControlled grammars "the dog that is not big runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular
          (Just (NegRelVP (Copula "big"))))
        (Intransitive "run")
