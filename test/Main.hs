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
import qualified Test.RelativeClauses as RelativeClauses
import qualified Test.CorpusRegression as CorpusRegression
import qualified Test.Coordination as Coordination
import qualified Test.Complements as Complements
import qualified Test.Tense as Tense
import qualified Test.Copula as Copula
import qualified Test.MoreDeterminers as MoreDeterminers
import qualified Test.MorePrepositions as MorePrepositions
import qualified Test.Passive as Passive
import qualified Test.Possessives as Possessives
import qualified Test.Progressive as Progressive
import qualified Test.Existential as Existential
import qualified Test.Imperative as Imperative
import qualified Test.Questions as Questions
import qualified Test.Modals as Modals
import qualified Test.Contractions as Contractions
import qualified Test.SubordinateClauses as SubordinateClauses
import qualified Test.TokenizationHardening as TokenizationHardening
import qualified Test.VerbAdverbs as VerbAdverbs

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
      RelativeClauses.spec g
      CorpusRegression.spec g
      Coordination.spec g
      Complements.spec g
      Tense.spec g
      Copula.spec g
      MoreDeterminers.spec g
      MorePrepositions.spec g
      Passive.spec g
      Possessives.spec g
      Progressive.spec g
      Existential.spec g
      Imperative.spec g
      Modals.spec g
      Contractions.spec g
      SubordinateClauses.spec g
      TokenizationHardening.spec g
      VerbAdverbs.spec g
      Questions.spec g
      Invalid.spec g
