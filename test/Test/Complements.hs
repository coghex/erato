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

  it "parses appear infinitival complement: the man appears to run" $ do
    let exprs = parseControlled grammars "the man appears to run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (VVComplement "appear" (Intransitive "run"))

  it "parses appear perfect infinitival complement: the man appears to have gone" $ do
    let exprs = parseControlled grammars "the man appears to have gone"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "man" Singular Nothing)
        (VVComplement "appear" (Perfective (Intransitive "go")))

  it "parses corpus lexical override sentence: this burrower appears to have gone through the earth" $ do
    let exprs = parseControlled grammars "this burrower appears to have gone through the earth"
    shouldParse exprs

  it "parses predicative seem with adjective complement: the dog seemed big" $ do
    let exprs = parseControlled grammars "the dog seemed big"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Past Positive
        (CommonNoun (Just "the") [] "dog" Singular Nothing)
        (SeemAdj (BareAdj "big"))

  it "parses predicative seem with nominal complement: that seemed this" $ do
    let exprs = parseControlled grammars "that seemed this"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Past Positive
        (Demonstrative "that" Singular)
        (SeemNP (Demonstrative "this" Singular))

  it "parses predicative feel with adjective complement: one feels poor-devilish" $ do
    let exprs = parseControlled grammars "one feels poor-devilish"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun Nothing [] "one" Singular Nothing)
        (FeelAdj (BareAdj "poor-devilish"))

  it "parses predicative grow with adjective complement and PP: one grows convivial upon tears" $ do
    let exprs = parseControlled grammars "one grows convivial upon tears"
    shouldParse exprs

  it "parses predicative go with adjective complement: ye shall go thankless" $ do
    let exprs = parseControlled grammars "ye shall go thankless"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Second Plural Subjective)
        (VVComplement "shall" (GoAdj (BareAdj "thankless")))

  it "parses predicative go with preverbal adverb: ye shall forever go thankless" $ do
    let exprs = parseControlled grammars "ye shall forever go thankless"
    shouldParse exprs

  it "parses the Moby-Dick correlative pains sentence" $ do
    let exprs = parseControlled grammars "For by how much the more pains ye take to please the world, by so much the more shall ye forever go thankless"
    shouldParse exprs

  it "parses the Moby-Dick would-that Hampton Court sentence" $ do
    let exprs = parseControlled grammars "Would that I could clear out Hampton Court and the Tuileries for ye"
    shouldParse exprs

  it "parses object infinitival complement: I want the dog to run" $ do
    let exprs = parseControlled grammars "I want the dog to run"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun First Singular Subjective)
        (V2VComplement "want"
          (CommonNoun (Just "the") [] "dog" Singular Nothing)
          (Intransitive "run"))

  it "parses corpus take-pains complement: ye take the more pains to please the world" $ do
    let exprs = parseControlled grammars "ye take the more pains to please the world"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (Pronoun Second Plural Subjective)
        (V2VComplement "take"
          (CommonNoun (Just "the") ["more"] "pain" Plural Nothing)
          (Transitive "please" (CommonNoun (Just "the") [] "world" Singular Nothing)))

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
          (Copula (BareAdj "big")))

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
            (Copula (BareAdj "big"))))

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

  it "parses clausal complement with coordinated predicate adjectives: I thought that it was too expensive and jolly" $ do
    let exprs = parseControlled grammars "I thought that it was too expensive and jolly"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Past Positive
        (Pronoun First Singular Subjective)
        (VSComplement "think"
          (Sentence Past Positive
            (Pronoun Third Singular Subjective)
            (Copula (CoordAdj And
              (ModifiedAdj "too" (BareAdj "expensive"))
              (BareAdj "jolly")))))

  it "parses passive clausal complement: it will be seen that the dog runs" $ do
    let exprs = parseControlled grammars "it will be seen that the dog runs"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Future Positive
        (Pronoun Third Singular Subjective)
        (PassiveVSComplement "see"
          (Sentence Present Positive
            (CommonNoun (Just "the") [] "dog" Singular Nothing)
            (Intransitive "run")))

  it "parses corpus passive clausal frame with nested noun complements" $ do
    let exprs = parseControlled grammars "it will be seen that this burrower of a poor devil of a man appears to have gone through the earth"
    shouldParse exprs

  it "parses corpus passive clausal frame with hyphenated common nouns" $ do
    let exprs = parseControlled grammars "it will be seen that this burrower and grub-worm of a poor devil of a Sub-Sub appears to have gone through the earth"
    shouldParse exprs

  it "parses corpus passive clausal frame with capitalized commonized proper nouns" $ do
    let exprs = parseControlled grammars "it will be seen that this burrower appears to have gone through the long Vaticans and street-stalls of the earth"
    shouldParse exprs

  it "parses corpus postnominal whatsoever under modal and adverb" $ do
    let exprs = parseControlled grammars "he could anyways find allusions to whales in any book whatsoever"
    shouldParse exprs

  it "parses stacked postnominal modifiers: the book whatsoever sacred or profane is big" $ do
    let exprs = parseControlled grammars "the book whatsoever sacred or profane is big"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun (Just "the") [] "book" Singular
          (Just (RelChain
            (PostAdj (BareAdj "whatsoever"))
            (PostAdj (CoordAdj Or
              (BareAdj "sacred")
              (BareAdj "profane"))))))
        (Copula (BareAdj "big"))

  it "parses corpus stacked postnominal modifiers with punctuation" $ do
    let exprs = parseControlled grammars "he could anyways find allusions to whales in any book whatsoever, sacred or profane"
    shouldParse exprs

  it "parses transitive infinitival complement with love: he loved to dust his old grammars" $ do
    let exprs = parseControlled grammars "he loved to dust his old grammars"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Past Positive
        (Pronoun Third Singular Subjective)
        (VVComplement "love"
          (Transitive "dust"
            (CommonNoun (Just "his") ["old"] "grammar" Plural Nothing)))

  it "parses coordinated infinitival complement with love: one sometimes loves to sit and feel poor-devilish too" $ do
    let exprs = parseControlled grammars "one sometimes loves to sit and feel poor-devilish too"
    shouldParse exprs
    exprs `shouldParseAs`
      Sentence Present Positive
        (CommonNoun Nothing [] "one" Singular Nothing)
        (VPWithAdv
          (VPWithAdv
            (VVComplement "love"
              (CoordVP And
                (Intransitive "sit")
                (FeelAdj (BareAdj "poor-devilish"))))
            (LexicalAdv "sometimes"))
          (LexicalAdv "too"))

  it "parses ternary coordinated infinitival complement with love: one sometimes loves to sit and feel poor-devilish too and grow convivial upon tears" $ do
    let exprs = parseControlled grammars "one sometimes loves to sit and feel poor-devilish too and grow convivial upon tears"
    shouldParse exprs

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
