{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.VerbAdverbs (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Verb adverbs" $ do
  it "parses comparative adverb on intransitive verb: the dog runs faster" $ do
    let exprs = parseControlled grammars "the dog runs faster"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Intransitive "run")
          (LexicalAdv "faster"))

  it "prefers adverb parse for ambiguous comparative object form: the dog runs faster" $ do
    parsePreferredControlledSentence grammars "the dog runs faster"
      `shouldBe`
        Just
          (Sentence Present Positive
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            (VPWithAdv
              (Intransitive "run")
              (LexicalAdv "faster")))

  it "parses degree-modified comparative adverb: the dog runs much faster" $ do
    let exprs = parseControlled grammars "the dog runs much faster"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Intransitive "run")
          (LexicalAdv "faster"))

  it "prefers adverb parse for degree-modified comparative form: the dog runs much faster" $ do
    parsePreferredControlledSentence grammars "the dog runs much faster"
      `shouldBe`
        Just
          (Sentence Present Positive
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            (VPWithAdv
              (Intransitive "run")
              (LexicalAdv "faster")))

  it "prefers adverb parse for ambiguous irregular comparative form: the dog runs better" $ do
    parsePreferredControlledSentence grammars "the dog runs better"
      `shouldBe`
        Just
          (Sentence Present Positive
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            (VPWithAdv
              (Intransitive "run")
              (LexicalAdv "better")))

  it "prefers adverb parse for ambiguous irregular comparative form: the dog runs worse" $ do
    parsePreferredControlledSentence grammars "the dog runs worse"
      `shouldBe`
        Just
          (Sentence Present Positive
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            (VPWithAdv
              (Intransitive "run")
              (LexicalAdv "worse")))

  it "prefers adverb parse for ambiguous comparative form: the dog runs harder" $ do
    parsePreferredControlledSentence grammars "the dog runs harder"
      `shouldBe`
        Just
          (Sentence Present Positive
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            (VPWithAdv
              (Intransitive "run")
              (LexicalAdv "harder")))

  it "prefers adverb parse for degree-modified irregular form: the dog runs much better" $ do
    parsePreferredControlledSentence grammars "the dog runs much better"
      `shouldBe`
        Just
          (Sentence Present Positive
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            (VPWithAdv
              (Intransitive "run")
              (LexicalAdv "better")))

  it "prefers adverb parse for degree-modified irregular form: the dog runs far worse" $ do
    parsePreferredControlledSentence grammars "the dog runs far worse"
      `shouldBe`
        Just
          (Sentence Present Positive
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            (VPWithAdv
              (Intransitive "run")
              (ModifiedAdv "far" (LexicalAdv "worse"))))

  it "parses later as a verb adverb: the dog runs later" $ do
    let exprs = parseControlled grammars "the dog runs later"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Intransitive "run")
          (LexicalAdv "later"))

  it "parses sooner as a verb adverb: the dog runs sooner" $ do
    let exprs = parseControlled grammars "the dog runs sooner"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Intransitive "run")
          (LexicalAdv "sooner"))

  it "parses reduced-speed lexical adverb: the man eats the food less quickly" $ do
    let exprs = parseControlled grammars "the man eats the food less quickly"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (VPWithAdv
          (Transitive "eat" (CommonNoun (Just "the") [] "food" Singular Nothing))
          (ModifiedAdv "less" (LexicalAdv "quick")))

  it "parses degree-modified reduced-speed adverb: the man eats the food far less quickly" $ do
    let exprs = parseControlled grammars "the man eats the food far less quickly"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (VPWithAdv
          (Transitive "eat" (CommonNoun (Just "the") [] "food" Singular Nothing))
          (ModifiedAdv "less" (LexicalAdv "quick")))

  it "parses preverbal adverb on intransitive verb: the dog now runs" $ do
    let exprs = parseControlled grammars "the dog now runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Intransitive "run")
          (LexicalAdv "now"))

  it "parses preverbal adverb on progressive verb: he was ever dusting his old grammars" $ do
    let exprs = parseControlled grammars "he was ever dusting his old grammars"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Past Positive
        (Pronoun Third Singular Subjective)
        (VPWithAdv
          (Progressive
            (Transitive "dust"
              (CommonNoun (Just "his") ["old"] "grammar" Plural Nothing)))
          (LexicalAdv "ever"))

  it "parses preverbal adverb with PP-taking verb: it somehow reminded him of his mortality" $ do
    let exprs = parseControlled grammars "it somehow reminded him of his mortality"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Past Positive
        (Pronoun Third Singular Subjective)
        (VPWithAdv
          (VPWithAdv
            (Transitive "remind" (Pronoun Third Singular Objective))
            (PrepPhrase "of" (CommonNoun (Just "his") [] "mortality" Singular Nothing)))
          (LexicalAdv "somehow"))

  it "parses preverbal adverb with VV complement: one sometimes loves to sit" $ do
    let exprs = parseControlled grammars "one sometimes loves to sit"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun Nothing [] "one" Singular Nothing)
        (VPWithAdv
          (VVComplement "love" (Intransitive "sit"))
          (LexicalAdv "sometimes"))

  it "parses stacked preverbal adverbs: it somehow mildly reminded him of his mortality" $ do
    let exprs = parseControlled grammars "it somehow mildly reminded him of his mortality"
    shouldParse exprs

  it "parses corpus preverbal adverb under a modal: he could anyways find allusions to whales in any book" $ do
    let exprs = parseControlled grammars "he could anyways find allusions to whales in any book"
    shouldParse exprs

  it "parses sentence-initial PP adverbial: in the park the dog runs" $ do
    let exprs = parseControlled grammars "in the park the dog runs"
    shouldParse exprs
    exprs `shouldParseAs`
      SentenceWithAdv
        (Sentence Present Positive
          (CommonNoun (Just "the") [] "dog" Singular Nothing)
          (Intransitive "run"))
        (PrepPhrase "in" (CommonNoun (Just "the") [] "park" Singular Nothing))

  it "parses the simplified fronted-winds corpus clause: a mazy way winds into distant woodlands" $ do
    let exprs = parseControlled grammars "a mazy way winds into distant woodlands"
    shouldParse exprs

  it "parses fronted discourse adverb: therefore you must not take the whale statements" $ do
    let exprs = parseControlled grammars "therefore you must not take the whale statements"
    shouldParse exprs

  it "parses preverbal full adverb under a modal: you must in every case run" $ do
    let exprs = parseControlled grammars "you must in every case run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Second Singular Subjective)
        (VVComplement "must"
          (VPWithAdv
            (Intransitive "run")
            (PrepPhrase "in" (CommonNoun (Just "every") [] "case" Singular Nothing))))

  it "parses the next corpus fronted-adverbial fragment" $ do
    let exprs = parseControlled grammars "therefore you must not in every case at least take the whale statements"
    shouldParse exprs

  it "parses the next corpus fragment with the hyphenated adjective" $ do
    let exprs = parseControlled grammars "therefore you must not in every case at least take the higgledy-piggledy whale statements"
    shouldParse exprs

  it "parses the next corpus fragment with however authentic" $ do
    let exprs = parseControlled grammars "therefore you must not in every case at least take the higgledy-piggledy whale statements however authentic"
    shouldParse exprs

  it "parses the next corpus fragment with veritable gospel cetology" $ do
    let exprs = parseControlled grammars "therefore you must not in every case at least take the higgledy-piggledy whale statements however authentic in these extracts for veritable gospel cetology"
    shouldParse exprs

  it "parses corpus lead-in with curated transitive verb and coordinated object" $ do
    let exprs = parseControlled grammars "for my part I abominate all honorable respectable toils, trials, and tribulations of every kind whatsoever"
    shouldParse exprs

  it "parses touching as a sentence-initial discourse opener" $ do
    let exprs = parseControlled grammars "touching the ancient authors these extracts are valuable"
    shouldParse exprs
    exprs `shouldParseAs`
      SentenceWithAdv
        (Sentence Present Positive
          (CommonNoun (Just "these") [] "extract" Plural Nothing)
          (Copula (BareAdj "valuable")))
        (PrepPhrase "touching" (CommonNoun (Just "the") ["ancient"] "author" Plural Nothing))

  it "parses as touching as a sentence-initial discourse opener" $ do
    let exprs = parseControlled grammars "as touching the ancient authors these extracts are valuable"
    shouldParse exprs
    exprs `shouldParseAs`
      SentenceWithAdv
        (Sentence Present Positive
          (CommonNoun (Just "these") [] "extract" Plural Nothing)
          (Copula (BareAdj "valuable")))
        (PrepPhrase "as touching" (CommonNoun (Just "the") ["ancient"] "author" Plural Nothing))

  it "parses as well as as a sentence-initial additive opener" $ do
    let exprs = parseControlled grammars "as well as the poets these extracts are valuable"
    shouldParse exprs
    exprs `shouldParseAs`
      SentenceWithAdv
        (Sentence Present Positive
          (CommonNoun (Just "these") [] "extract" Plural Nothing)
          (Copula (BareAdj "valuable")))
        (PrepPhrase "as well as" (CommonNoun (Just "the") [] "poet" Plural Nothing))

  it "parses the trimmed as touching / as well as corpus opener" $ do
    let exprs = parseControlled grammars "as touching the ancient authors generally as well as the poets these extracts are solely valuable or entertaining"
    shouldParse exprs

  it "parses as affording as a VP-like explanatory adjunct" $ do
    let exprs = parseControlled grammars "these extracts are valuable as affording a view"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "these") [] "extract" Plural Nothing)
        (VPWithAdv
          (Copula (BareAdj "valuable"))
          (PrepPhrase "as affording" (CommonNoun (Just "a") [] "view" Singular Nothing)))

  it "parses as affording a view of an embedded what-clause" $ do
    let exprs = parseControlled grammars "these extracts are valuable as affording a view of what has been said of Leviathan"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "these") [] "extract" Plural Nothing)
        (VPWithAdv
          (Copula (BareAdj "valuable"))
          (PrepPhrase "as affording"
            (CommonNoun (Just "a") [] "view" Singular
              (Just (RelPrepSentence "of"
                (WhQuestion Present Positive
                  (SubjectWh What
                    (VPWithAdv
                      (Perfective (Passive "say"))
                      (PrepPhrase "of" (ProperNoun "Leviathan"))))))))))
