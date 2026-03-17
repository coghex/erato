{-# LANGUAGE Strict, UnicodeSyntax #-}

module Test.Copula (spec) where

import Test.Hspec

import Parser.AST
import Parser.GFParser
import Test.Utils

spec ∷ GrammarBundle → Spec
spec grammars = describe "Copula (adjective predicate)" $ do
  it "parses copula: the dog is big" $ do
    let exprs = parseControlled grammars "the dog is big"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Copula (BareAdj "big"))

  it "parses copula plural: the dogs are big" $ do
    let exprs = parseControlled grammars "the dogs are big"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Plural Nothing)
        (Copula (BareAdj "big"))

  it "parses copula with pronoun: she is old" $ do
    let exprs = parseControlled grammars "she is old"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Third Singular Subjective)
        (Copula (BareAdj "old"))

  it "parses copula negation: the dog is not big" $ do
    let exprs = parseControlled grammars "the dog is not big"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Negative
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Copula (BareAdj "big"))

  it "parses copula past: the dog was big" $ do
    let exprs = parseControlled grammars "the dog was big"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Past Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Copula (BareAdj "big"))

  it "parses copula future: the dog will be big" $ do
    let exprs = parseControlled grammars "the dog will be big"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Future Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (Copula (BareAdj "big"))

  it "parses copula with locative PP: Lazarus is in the park" $ do
    let exprs = parseControlled grammars "Lazarus is in the park"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (ProperNoun "Lazarus")
        (CopulaAdv (PrepPhrase "in" (CommonNoun (Just "the") [] "park" Singular Nothing)))

  it "parses comparative copula: the dog is bigger than the cat" $ do
    let exprs = parseControlled grammars "the dog is bigger than the cat"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Copula (BareAdj "bigger"))
          (PrepPhrase "than" (CommonNoun (Just "the") [] "cat" Singular Nothing)))

  it "parses comparative copula question: is the dog bigger than the cat" $ do
    let exprs = parseControlled grammars "is the dog bigger than the cat"
    shouldParse exprs
    exprs `shouldParseAs`
      Question Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Copula (BareAdj "bigger"))
          (PrepPhrase "than" (CommonNoun (Just "the") [] "cat" Singular Nothing)))

  it "parses degree-modified comparative copula: the dog is much bigger than the cat" $ do
    let exprs = parseControlled grammars "the dog is much bigger than the cat"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Copula (BareAdj "bigger"))
          (PrepPhrase "than" (CommonNoun (Just "the") [] "cat" Singular Nothing)))

  it "parses degree-modified comparative question: is the dog slightly bigger than the cat" $ do
    let exprs = parseControlled grammars "is the dog slightly bigger than the cat"
    shouldParse exprs
    exprs `shouldParseAs`
      Question Present Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (VPWithAdv
          (Copula (BareAdj "bigger"))
          (PrepPhrase "than" (CommonNoun (Just "the") [] "cat" Singular Nothing)))

  it "parses degree-modified predicate adjective: it is too expensive" $ do
    let exprs = parseControlled grammars "it is too expensive"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Third Singular Subjective)
        (Copula (ModifiedAdj "too" (BareAdj "expensive")))

  it "parses hyphenated predicate adjective: it is rosy-strong" $ do
    let exprs = parseControlled grammars "it is rosy-strong"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Third Singular Subjective)
        (Copula (BareAdj "rosy-strong"))

  it "parses coordinated predicate adjectives: it is expensive and jolly" $ do
    let exprs = parseControlled grammars "it is expensive and jolly"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Third Singular Subjective)
        (Copula (CoordAdj And (BareAdj "expensive") (BareAdj "jolly")))

  it "parses degree-modified coordinated predicate adjectives: it is too expensive and jolly" $ do
    let exprs = parseControlled grammars "it is too expensive and jolly"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Third Singular Subjective)
        (Copula (CoordAdj And
          (ModifiedAdj "too" (BareAdj "expensive"))
          (BareAdj "jolly")))

  it "parses the corpus-shaped hyphenated predicate adjective: pale sherry would be too rosy-strong" $ do
    let exprs = parseControlled grammars "pale sherry would be too rosy-strong"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Conditional Positive
        (CommonNoun Nothing ["pale"] "sherry" Singular Nothing)
        (Copula (ModifiedAdj "too" (BareAdj "rosy-strong")))

  describe "Copula (nominal predicate)" $ do
    it "parses nominal copula: it is a dog" $ do
      let exprs = parseControlled grammars "it is a dog"
      shouldParse exprs
      exprs `shouldParseAs`
        Sentence Present Positive
          (Pronoun Third Singular Subjective)
          (CopulaNP (CommonNoun (Just "a") [] "dog" Singular Nothing))

    it "parses demonstrative nominal copula with noun complement modifiers" $ do
      let exprs = parseControlled grammars "this is my substitute for pistol and ball"
      shouldParse exprs
      exprs `shouldParseAs`
        Sentence Present Positive
          (Demonstrative "this" Singular)
          (VPWithAdv
            (CopulaNP (CommonNoun (Just "my") [] "substitute" Singular Nothing))
            (PrepPhrase "for"
              (CoordNP And
                (CommonNoun Nothing [] "pistol" Singular Nothing)
                (CommonNoun Nothing [] "ball" Singular Nothing))))

    it "parses bare abstract noun subject: might is right" $ do
      let exprs = parseControlled grammars "might is right"
      shouldParse exprs
      exprs `shouldParseAs`
        Sentence Present Positive
          (CommonNoun Nothing [] "might" Singular Nothing)
          (Copula (BareAdj "right"))

    it "parses nominal copula with bare abstract PP: the whale will be a giant in might" $ do
      let exprs = parseControlled grammars "the whale will be a giant in might"
      shouldParse exprs
      exprs `shouldParseAs`
        Sentence Future Positive
          (CommonNoun (Just "the") [] "whale" Singular Nothing)
          (VPWithAdv
            (CopulaNP (CommonNoun (Just "a") [] "giant" Singular Nothing))
            (PrepPhrase "in" (CommonNoun Nothing [] "might" Singular Nothing)))

    it "parses the corpus-shaped nominal copula with curated adjective: it is a Hyperborean winter scene" $ do
      let exprs = parseControlled grammars "it is a Hyperborean winter scene"
      shouldParse exprs

    it "parses nominal copula question: is the man a sailor" $ do
      let exprs = parseControlled grammars "is the man a sailor"
      shouldParse exprs
      exprs `shouldParseAs`
        Question Present Positive
          (CommonNoun (Just "the") [] "man" Singular Nothing)
          (CopulaNP (CommonNoun (Just "a") [] "sailor" Singular Nothing))

    it "parses locative copula question: is Lazarus in the park" $ do
      let exprs = parseControlled grammars "is Lazarus in the park"
      shouldParse exprs
      exprs `shouldParseAs`
        Question Present Positive
          (ProperNoun "Lazarus")
          (CopulaAdv (PrepPhrase "in" (CommonNoun (Just "the") [] "park" Singular Nothing)))

    it "parses demonstrative nominal copula: that is the whale" $ do
      let exprs = parseControlled grammars "that is the whale"
      shouldParse exprs
      exprs `shouldParseAs`
        Sentence Present Positive
          (Demonstrative "that" Singular)
          (CopulaNP (CommonNoun (Just "the") [] "whale" Singular Nothing))

    it "parses plural demonstrative nominal copula: these are sailors" $ do
      let exprs = parseControlled grammars "these are sailors"
      shouldParse exprs
      exprs `shouldParseAs`
        Sentence Present Positive
          (Demonstrative "these" Plural)
          (CopulaNP (CommonNoun Nothing [] "sailor" Plural Nothing))

    it "parses plural demonstrative nominal copula: those are the men" $ do
      let exprs = parseControlled grammars "those are the men"
      shouldParse exprs
      exprs `shouldParseAs`
        Sentence Present Positive
          (Demonstrative "those" Plural)
          (CopulaNP (CommonNoun (Just "the") [] "man" Plural Nothing))
