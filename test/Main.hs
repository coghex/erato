{-# LANGUAGE Strict, UnicodeSyntax #-}

module Main where

import Test.Hspec

import qualified Test.Pronouns as Pronouns
import qualified Test.Negation as Negation
import qualified Test.Agreement as Agreement
import qualified Test.Determiners as Determiners
import qualified Test.Adjectives as Adjectives
import qualified Test.CommonVocabulary as CommonVocabulary
import qualified Test.Invalid as Invalid

main ∷ IO ()
main = hspec $ do
  Pronouns.spec
  Negation.spec
  Agreement.spec
  Determiners.spec
  Adjectives.spec
  CommonVocabulary.spec
  Invalid.spec
