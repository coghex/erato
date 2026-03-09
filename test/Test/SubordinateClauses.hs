{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.SubordinateClauses (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Subordinate clauses" $ do
  it "parses because-clause adjunct: the dog runs because the cat runs" $ do
    let exprs = parseControlled grammars "the dog runs because the cat runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Intransitive "run")
          (ClausePhrase "because"
            (Sentence Present Positive
              (CommonNoun (Just "the") [] "cat" Singular Nothing)
              (Intransitive "run"))))

  it "parses if-clause adjunct: the dog runs if the cat runs" $ do
    let exprs = parseControlled grammars "the dog runs if the cat runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Intransitive "run")
          (ClausePhrase "if"
            (Sentence Present Positive
              (CommonNoun (Just "the") [] "cat" Singular Nothing)
              (Intransitive "run"))))

  it "parses when-clause adjunct: the dog runs when the man runs" $ do
    let exprs = parseControlled grammars "the dog runs when the man runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Intransitive "run")
          (ClausePhrase "when"
            (Sentence Present Positive
              (CommonNoun (Just "the") [] "man" Singular Nothing)
              (Intransitive "run"))))
