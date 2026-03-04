{-# LANGUAGE Strict, UnicodeSyntax #-}

module Main where

import Test.Hspec

import Test.Utils (loadTestGrammars)

import qualified Test.Pronouns as Pronouns
import qualified Test.Negation as Negation
import qualified Test.Agreement as Agreement
import qualified Test.Determiners as Determiners
import qualified Test.Adjectives as Adjectives
import qualified Test.CommonVocabulary as CommonVocabulary
import qualified Test.Invalid as Invalid
import qualified Test.ProperNouns as ProperNouns
import qualified Test.Prepositions as Prepositions

main ∷ IO ()
main = do
  mg ← loadTestGrammars
  case mg of
    Nothing → putStrLn "Missing Grammar/*.pgf files; run gf -make first."
    Just g  → hspec $ do
      Pronouns.spec g
      Negation.spec g
      Agreement.spec g
      Determiners.spec g
      Adjectives.spec g
      CommonVocabulary.spec g
      ProperNouns.spec g
      Prepositions.spec g
      Invalid.spec g
