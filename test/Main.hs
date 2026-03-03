{-# LANGUAGE Strict, UnicodeSyntax #-}

module Main where

import Test.Hspec
import System.Directory (doesFileExist)

import Parser.AST
import Parser.GFParser
import Parser.Translate

main ∷ IO ()
main = hspec $ do
  describe "Erato controlled grammar (pronouns)" $ do
    it "parses pronoun subject: I run" $ do
      withGrammars $ \grammars -> do
        let exprs = parseControlled grammars "I run"
        exprs `shouldSatisfy` (not . null)
        exprToSentence (head exprs)
          `shouldBe`
            Just (Sentence Present Positive
                    (Pronoun First Singular Subjective)
                    (Intransitive "run"))

    it "parses pronoun object: the man eats her" $ do
      withGrammars $ \grammars -> do
        let exprs = parseControlled grammars "the man eats her"
        exprs `shouldSatisfy` (not . null)
        exprToSentence (head exprs)
          `shouldBe`
            Just (Sentence Present Positive
                    (CommonNoun (Just "the") [] "man")
                    (Transitive "eat" (Pronoun Third Singular Objective)))

    it "rejects object case as subject: them eats the food" $ do
      withGrammars $ \grammars -> do
        let exprs = parseControlled grammars "them eats the food"
        exprs `shouldBe` []

    it "parses negation: the dog does not run" $ do
      withGrammars $ \grammars -> do
        let exprs = parseControlled grammars "the dog does not run"
        exprs `shouldSatisfy` (not . null)
        exprToSentence (head exprs)
          `shouldBe`
            Just (Sentence Present Negative
                    (CommonNoun (Just "the") [] "dog")
                    (Intransitive "run"))

    it "parses negation: I do not run" $ do
      withGrammars $ \grammars -> do
        let exprs = parseControlled grammars "I do not run"
        exprs `shouldSatisfy` (not . null)
        exprToSentence (head exprs)
          `shouldBe`
            Just (Sentence Present Negative
                    (Pronoun First Singular Subjective)
                    (Intransitive "run"))

    it "parses negation: the man does not eat the food" $ do
      withGrammars $ \grammars -> do
        let exprs = parseControlled grammars "the man does not eat the food"
        exprs `shouldSatisfy` (not . null)
        exprToSentence (head exprs)
          `shouldBe`
            Just (Sentence Present Negative
                    (CommonNoun (Just "the") [] "man")
                    (Transitive "eat" (CommonNoun (Just "the") [] "food")))

withGrammars ∷ (GrammarBundle → IO ()) → IO ()
withGrammars action = do
  hasControlled <- doesFileExist "Grammar/EratoAbs.pgf"
  hasFallback <- doesFileExist "Grammar/AllEngAbs.pgf"
  if not (hasControlled && hasFallback)
    then pendingWith "Missing Grammar/EratoAbs.pgf or Grammar/AllEngAbs.pgf; run gf -make first."
    else loadGrammars "Grammar/EratoAbs.pgf" "Grammar/AllEngAbs.pgf" >>= action
