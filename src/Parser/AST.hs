{-# LANGUAGE Strict, UnicodeSyntax #-}

module Parser.AST
  ( Tense(..)
  , Polarity(..)
  , Person(..)
  , Number(..)
  , PronounCase(..)
  , NounPhrase(..)
  , VerbPhrase(..)
  , AdvPhrase(..)
  , RelClause(..)
  , Sentence(..)
  , Conj(..)
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

data Conj = And | Or
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
      , rel    ∷ Maybe RelClause
      }
  | CoordNP Conj NounPhrase NounPhrase
  deriving (Eq, Show)

data RelClause
  = RelVP VerbPhrase
  | NegRelVP VerbPhrase
  | RelV2 String NounPhrase
  | NegRelV2 String NounPhrase
  deriving (Eq, Show)

data AdvPhrase
  = PrepPhrase String NounPhrase
  deriving (Eq, Show)

data VerbPhrase
  = Intransitive String
  | Transitive String NounPhrase
  | Copula String
  | Passive String
  | Progressive VerbPhrase
  | VPWithAdv VerbPhrase AdvPhrase
  | CoordVP Conj VerbPhrase VerbPhrase
  deriving (Eq, Show)

data Sentence
  = Sentence
    { tense    ∷ Tense
    , polarity ∷ Polarity
    , subject  ∷ NounPhrase
    , verb     ∷ VerbPhrase
    }
  | Question Tense Polarity NounPhrase VerbPhrase
  | Existential Tense Polarity NounPhrase
  | Imperative Polarity VerbPhrase
  deriving (Eq, Show)
