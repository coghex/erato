{-# LANGUAGE Strict, UnicodeSyntax #-}

module Parser.AST
  ( Tense(..)
  , Polarity(..)
  , Person(..)
  , Number(..)
  , PronounCase(..)
  , NounPhrase(..)
  , VerbPhrase(..)
  , Sentence(..)
  ) where

data Tense = Present | Past | Future
  deriving (Eq, Show)

data Polarity = Positive | Negative
  deriving (Eq, Show)

data Person = First | Second | Third
  deriving (Eq, Show)

data Number = Singular | Plural
  deriving (Eq, Show)

data PronounCase = Subjective | Objective
  deriving (Eq, Show)

data NounPhrase
  = ProperNoun String
  | Pronoun
      { person      ∷ Person
      , number      ∷ Number
      , pronounCase ∷ PronounCase
      }
  | CommonNoun
      { det   ∷ Maybe String
      , adjs  ∷ [String]
      , noun  ∷ String
      , number ∷ Number
      }
  deriving (Eq, Show)

data VerbPhrase
  = Intransitive String
  | Transitive String NounPhrase
  deriving (Eq, Show)

data Sentence = Sentence
  { tense    ∷ Tense
  , polarity ∷ Polarity
  , subject  ∷ NounPhrase
  , verb     ∷ VerbPhrase
  } deriving (Eq, Show)
