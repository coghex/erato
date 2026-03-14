{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Possessives (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Possessives" $ do
  it "parses common noun possessive: the dog's food is big" $ do
    let exprs = parseControlled grammars "the dog's food is big"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (PossessedNoun
          (CommonNoun (Just "the") [] "dog" Singular Nothing)
          [] "food" Singular Nothing)
        (Copula (BareAdj "big"))

  it "parses proper noun possessive: John's dog runs" $ do
    let exprs = parseControlled grammars "John's dog runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (PossessedNoun
          (ProperNoun "John")
          [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses plural possessive: the dogs' food is big" $ do
    let exprs = parseControlled grammars "the dogs' food is big"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (PossessedNoun
          (CommonNoun (Just "the") [] "dog" Plural Nothing)
          [] "food" Singular Nothing)
        (Copula (BareAdj "big"))

  it "parses possessive with adjective: John's red dog runs" $ do
    let exprs = parseControlled grammars "John's red dog runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (PossessedNoun
          (ProperNoun "John")
          ["red"] "dog" Singular Nothing)
        (Intransitive "run")

  it "prefers the possessive parse for the dog's food is big" $ do
    parsePreferredControlledSentence grammars "the dog's food is big" `shouldBe`
      Just
        (Sentence Present Positive
          (PossessedNoun
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            [] "food" Singular Nothing)
          (Copula (BareAdj "big")))

  it "prefers the possessive parse for John's red dog runs" $ do
    parsePreferredControlledSentence grammars "John's red dog runs" `shouldBe`
      Just
        (Sentence Present Positive
          (PossessedNoun
            (ProperNoun "John")
            ["red"] "dog" Singular Nothing)
          (Intransitive "run"))
