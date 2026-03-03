{-# LANGUAGE Strict, UnicodeSyntax #-}

module Parser.AST
  ( Tense(..)
  , Polarity(..)
  , NounPhrase(..)
  , VerbPhrase(..)
  , Sentence(..)
  ) where

data Tense = Present | Past | Future
  deriving (Eq, Show)

data Polarity = Positive | Negative
  deriving (Eq, Show)

data NounPhrase
  = ProperNoun String
  | CommonNoun
      { det   ∷ Maybe String
      , adjs  ∷ [String]
      , noun  ∷ String
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
