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
        (Copula (BareAdj "big"))

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

  it "parses capitalized hyphenated common noun: it is a Sub-Sub." $ do
    let exprs = parseControlled grammars "it is a Sub-Sub."
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Third Singular Subjective)
        (CopulaNP (CommonNoun (Just "a") [] "sub-sub" Singular Nothing))

  it "parses hyphenated plural common noun: the street-stalls are big." $ do
    let exprs = parseControlled grammars "the street-stalls are big."
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "street-stall" Plural Nothing)
        (Copula (BareAdj "big"))

  it "parses mid-sentence capitalized common noun: the Vaticans are big." $ do
    let exprs = parseControlled grammars "the Vaticans are big."
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "vatican" Plural Nothing)
        (Copula (BareAdj "big"))

  it "parses sentence-initial interjection lead-in: Oh, the whale will be a king." $ do
    let exprs = parseControlled grammars "Oh, the whale will be a king."
    shouldParse exprs
    exprs `shouldParseAs`
      SentenceWithAdv
        (Sentence Future Positive
          (CommonNoun (Just "the") [] "whale" Singular Nothing)
          (CopulaNP (CommonNoun (Just "a") [] "king" Singular Nothing)))
        (LexicalAdv "oh")
