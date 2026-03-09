{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Questions (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Questions" $ do
  it "parses present yes/no question: does the dog run" $ do
    let exprs = parseControlled grammars "does the dog run"
    shouldParse exprs
    exprs `shouldParseAs`
      Question Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses plural yes/no question: do the dogs run" $ do
    let exprs = parseControlled grammars "do the dogs run"
    shouldParse exprs
    exprs `shouldParseAs`
      Question Present Positive
        (CommonNoun (Just "the") [] "dog" Plural Nothing)
        (Intransitive "run")

  it "parses transitive past question: did the man eat the food" $ do
    let exprs = parseControlled grammars "did the man eat the food"
    shouldParse exprs
    exprs `shouldParseAs`
      Question Past Positive
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (Transitive "eat" (CommonNoun (Just "the") [] "food" Singular Nothing))

  it "parses future question: will the dog run" $ do
    let exprs = parseControlled grammars "will the dog run"
    shouldParse exprs
    exprs `shouldParseAs`
      Question Future Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses perfect question: has the dog run" $ do
    let exprs = parseControlled grammars "has the dog run"
    shouldParse exprs
    exprs `shouldParseAs`
      Question Perfect Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses negative perfect question: has the dog not run" $ do
    let exprs = parseControlled grammars "has the dog not run"
    shouldParse exprs
    exprs `shouldParseAs`
      Question Perfect Negative
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses copula question: is the dog big" $ do
    let exprs = parseControlled grammars "is the dog big"
    shouldParse exprs
    exprs `shouldParseAs`
      Question Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Copula "big")

  it "parses negative question: does the dog not run" $ do
    let exprs = parseControlled grammars "does the dog not run"
    shouldParse exprs
    exprs `shouldParseAs`
      Question Present Negative
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Intransitive "run")

  it "parses subject wh-question: who runs" $ do
    let exprs = parseControlled grammars "who runs"
    shouldParse exprs
    exprs `shouldParseAs`
      WhQuestion Present Positive
        (SubjectWh Who (Intransitive "run"))

  it "prefers subject wh-question parse for: who runs" $ do
    parsePreferredControlledSentence grammars "who runs"
      `shouldBe`
        Just
          (WhQuestion Present Positive
            (SubjectWh Who (Intransitive "run")))

  it "parses subject wh-question with object: who eats the food" $ do
    let exprs = parseControlled grammars "who eats the food"
    shouldParse exprs
    exprs `shouldParseAs`
      WhQuestion Present Positive
        (SubjectWh Who
          (Transitive "eat" (CommonNoun (Just "the") [] "food" Singular Nothing)))

  it "parses object wh-question: what does the man eat" $ do
    let exprs = parseControlled grammars "what does the man eat"
    shouldParse exprs
    exprs `shouldParseAs`
      WhQuestion Present Positive
        (ObjectWh What
          (CommonNoun (Just "the") [] "man" Singular Nothing)
          "eat")

  it "parses object wh-question with whom: whom does the man see" $ do
    let exprs = parseControlled grammars "whom does the man see"
    shouldParse exprs
    exprs `shouldParseAs`
      WhQuestion Present Positive
        (ObjectWh Who
          (CommonNoun (Just "the") [] "man" Singular Nothing)
          "see")

  it "parses adverb wh-question: where does the dog run" $ do
    let exprs = parseControlled grammars "where does the dog run"
    shouldParse exprs
    exprs `shouldParseAs`
      WhQuestion Present Positive
        (AdvWh Where
          (CommonNoun (Just "the") [] "dog" Singular Nothing)
          (Intransitive "run"))

  it "parses adverb wh-question: when does the dog run" $ do
    let exprs = parseControlled grammars "when does the dog run"
    shouldParse exprs
    exprs `shouldParseAs`
      WhQuestion Present Positive
        (AdvWh When
          (CommonNoun (Just "the") [] "dog" Singular Nothing)
          (Intransitive "run"))

  it "parses adverb wh-question: why does the dog run" $ do
    let exprs = parseControlled grammars "why does the dog run"
    shouldParse exprs
    exprs `shouldParseAs`
      WhQuestion Present Positive
        (AdvWh Why
          (CommonNoun (Just "the") [] "dog" Singular Nothing)
          (Intransitive "run"))

  it "parses adverb wh-question: how does the dog run" $ do
    let exprs = parseControlled grammars "how does the dog run"
    shouldParse exprs
    exprs `shouldParseAs`
      WhQuestion Present Positive
        (AdvWh How
          (CommonNoun (Just "the") [] "dog" Singular Nothing)
          (Intransitive "run"))

  it "rejects inflected object wh-question verb: what does the man eats" $ do
    let exprs = parseControlled grammars "what does the man eats"
    shouldReject exprs
