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
        (Copula (BareAdj "big"))

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
          (Just (NegRelVP (Copula (BareAdj "big")))))
        (Intransitive "run")

  it "parses corpus-shaped relative with which and future adverbials" $ do
    let exprs = parseControlled grammars "the tribe which no wine of this world will ever warm runs"
    shouldParse exprs

  it "parses possessive relative with copular gap: the devil whose commentator I am runs" $ do
    let exprs = parseControlled grammars "the devil whose commentator I am runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "devil" Singular
          (Just (RelWhoseBe "commentator" (Pronoun First Singular Subjective))))
        (Intransitive "run")

  it "parses prep-whom relative with corpus-shaped copula: the tribe for whom pale sherry would be too rosy-strong runs" $ do
    let exprs = parseControlled grammars "the tribe for whom pale sherry would be too rosy-strong runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "tribe" Singular
          (Just (RelPrepSentence "for"
            (WhQuestion Conditional Positive
              (AdvWh Who
                (CommonNoun Nothing ["pale"] "sherry" Singular Nothing)
                (Copula (ModifiedAdj "too" (BareAdj "rosy-strong"))))))))
        (Intransitive "run")

  it "parses coordinated relative chain with prep-whom clause" $ do
    let exprs = parseControlled grammars "the tribe which no wine of this world will ever warm and for whom pale sherry would be too rosy-strong runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "tribe" Singular
          (Just (RelChain
            (RelV2 "warm"
              (CommonNoun (Just "no") [] "wine" Singular
                (Just (RelPrep "of" (CommonNoun (Just "this") [] "world" Singular Nothing)))))
            (RelPrepSentence "for"
              (WhQuestion Conditional Positive
                (AdvWh Who
                  (CommonNoun Nothing ["pale"] "sherry" Singular Nothing)
                  (Copula (ModifiedAdj "too" (BareAdj "rosy-strong")))))))))
        (Intransitive "run")

  it "parses prep-whom relative with infinitival complement: the tribe with whom one loves to sit runs" $ do
    let exprs = parseControlled grammars "the tribe with whom one loves to sit runs"
    shouldParse exprs

  it "parses prep-whom relative with preverbal adverb and infinitival complement" $ do
    let exprs = parseControlled grammars "the tribe with whom one sometimes loves to sit runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "tribe" Singular
          (Just (RelPrepSentence "with"
            (WhQuestion Present Positive
              (AdvWh Who
                (CommonNoun Nothing [] "one" Singular Nothing)
                (VPWithAdv
                  (VVComplement "love" (Intransitive "sit"))
                  (LexicalAdv "sometimes")))))))
        (Intransitive "run")

  it "parses contrastive relative chain with prep-whom clause" $ do
    let exprs = parseControlled grammars "the tribe for whom even pale sherry would be too rosy-strong but with whom one sometimes loves to sit runs"
    shouldParse exprs

  it "parses the fuller corpus-shaped relative chain through but-with-whom" $ do
    let exprs = parseControlled grammars "the tribe which no wine of this world will ever warm and for whom even pale sherry would be too rosy-strong but with whom one sometimes loves to sit runs"
    shouldParse exprs

  it "parses the next fuller corpus-shaped chain through feel poor-devilish too" $ do
    let exprs = parseControlled grammars "the tribe which no wine of this world will ever warm and for whom even pale sherry would be too rosy-strong but with whom one sometimes loves to sit and feel poor-devilish too runs"
    shouldParse exprs

  it "parses the next corpus-shaped chain through grow convivial upon tears" $ do
    let exprs = parseControlled grammars "the tribe which no wine of this world will ever warm and for whom even pale sherry would be too rosy-strong but with whom one sometimes loves to sit and feel poor-devilish too and grow convivial upon tears runs"
    shouldParse exprs
