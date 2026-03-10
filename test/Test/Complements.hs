{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Complements (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Complements" $ do
  it "parses infinitival complement: the dog wants to run" $ do
    let exprs = parseControlled grammars "the dog wants to run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VVComplement "want" (Intransitive "run"))

  it "prefers infinitival complement parse for: the dog wants to run" $ do
    parsePreferredControlledSentence grammars "the dog wants to run"
      `shouldBe`
        Just
          (Sentence Present Positive
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            (VVComplement "want" (Intransitive "run")))

  it "parses raising complement: the dog seems to run" $ do
    let exprs = parseControlled grammars "the dog seems to run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VVComplement "seem" (Intransitive "run"))

  it "parses object infinitival complement: I want the dog to run" $ do
    let exprs = parseControlled grammars "I want the dog to run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun First Singular Subjective)
        (V2VComplement "want"
          (CommonNoun (Just "the") [] "dog" Singular Nothing)
          (Intransitive "run"))

  it "prefers object infinitival complement parse for: I want the dog to run" $ do
    parsePreferredControlledSentence grammars "I want the dog to run"
      `shouldBe`
        Just
          (Sentence Present Positive
            (Pronoun First Singular Subjective)
            (V2VComplement "want"
              (CommonNoun (Just "the") [] "dog" Singular Nothing)
              (Intransitive "run")))

  it "parses infinitival complement with pronoun object: he told her to leave" $ do
    let exprs = parseControlled grammars "he told her to leave"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Past Positive
        (Pronoun Third Singular Subjective)
        (V2VComplement "tell"
          (Pronoun Third Singular Objective)
          (Intransitive "leave"))

  it "parses copular infinitival complement: I believe the dog to be big" $ do
    let exprs = parseControlled grammars "I believe the dog to be big"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun First Singular Subjective)
        (V2VComplement "believe"
          (CommonNoun (Just "the") [] "dog" Singular Nothing)
          (Copula "big"))

  it "parses clausal complement: the man says that the dog runs" $ do
    let exprs = parseControlled grammars "the man says that the dog runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (VSComplement "say"
          (Sentence Present Positive
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            (Intransitive "run")))

  it "parses clausal complement with copula: the woman knows that the dog is big" $ do
    let exprs = parseControlled grammars "the woman knows that the dog is big"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "woman" Singular Nothing)
        (VSComplement "know"
          (Sentence Present Positive
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            (Copula "big")))

  it "parses clausal complement with plural embedded subject: I think that the dogs run" $ do
    let exprs = parseControlled grammars "I think that the dogs run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun First Singular Subjective)
        (VSComplement "think"
          (Sentence Present Positive
            (CommonNoun (Just "the") [] "dog" Plural Nothing)
            (Intransitive "run")))

  it "parses transitive infinitival complement with love: he loved to dust his old grammars" $ do
    let exprs = parseControlled grammars "he loved to dust his old grammars"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Past Positive
        (Pronoun Third Singular Subjective)
        (VVComplement "love"
          (Transitive "dust"
            (CommonNoun (Just "his") ["old"] "grammar" Plural Nothing)))

  it "parses remind with of-PP: it reminded him of his mortality" $ do
    let exprs = parseControlled grammars "it reminded him of his mortality"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Past Positive
        (Pronoun Third Singular Subjective)
        (VPWithAdv
          (Transitive "remind" (Pronoun Third Singular Objective))
          (PrepPhrase "of"
            (CommonNoun (Just "his") [] "mortality" Singular Nothing)))
