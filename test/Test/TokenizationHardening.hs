{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.TokenizationHardening (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Tokenization hardening" $ do
  it "parses sentence-final punctuation: The dog runs." $ do
    let exprs = parseControlled grammars "The dog runs."
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses quoted sentence: \"The dog runs.\"" $ do
    let exprs = parseControlled grammars "\"The dog runs.\""
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses contraction with punctuation: the dog can't run!" $ do
    let exprs = parseControlled grammars "the dog can't run!"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Negative
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VVComplement "can" (Intransitive "run"))

  it "parses copular contraction with punctuation: he's big." $ do
    let exprs = parseControlled grammars "he's big."
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Third Singular Subjective)
        (Copula "big")

  it "parses comma-separated subordinate clause: the dog runs, because the cat runs." $ do
    let exprs = parseControlled grammars "the dog runs, because the cat runs."
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
