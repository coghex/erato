{-# LANGUAGE Strict, UnicodeSyntax #-}

module Parser.AST
  ( Tense(..)
  , Polarity(..)
  , Person(..)
  , Number(..)
  , PronounCase(..)
  , QuestionWord(..)
  , AdjPhrase(..)
  , NounPhrase(..)
  , VerbPhrase(..)
  , AdvPhrase(..)
  , RelClause(..)
  , WhClause(..)
  , Sentence(..)
  , Conj(..)
  ) where

data Tense = Present | Past | Future | Conditional | Perfect
  deriving (Eq, Show)

data Polarity = Positive | Negative
  deriving (Eq, Show)

data Person = First | Second | Third
  deriving (Eq, Show)

data Number = Singular | Plural
  deriving (Eq, Show)

data PronounCase = Subjective | Objective
  deriving (Eq, Show)

data QuestionWord = Who | What | Where | When | Why | How | Which | HowMany | HowMuch
  deriving (Eq, Show)

data Conj = And | Or
  deriving (Eq, Show)

data AdjPhrase
  = BareAdj String
  | ModifiedAdj String AdjPhrase
  | CoordAdj Conj AdjPhrase AdjPhrase
  deriving (Eq, Show)

data NounPhrase
  = ProperNoun String
  | Demonstrative String Number
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
  | PossessedNoun NounPhrase [String] String Number (Maybe RelClause)
  | CoordNP Conj NounPhrase NounPhrase
  deriving (Eq, Show)

data RelClause
  = RelVP VerbPhrase
  | NegRelVP VerbPhrase
  | RelV2 String NounPhrase
  | NegRelV2 String NounPhrase
  | RelWhoseBe String NounPhrase
  | RelPrep String NounPhrase
  | RelPrepSentence String Sentence
  | PostAdj AdjPhrase
  | RelChain RelClause RelClause
  deriving (Eq, Show)

data AdvPhrase
  = PrepPhrase String NounPhrase
  | ClausePhrase String Sentence
  | LexicalAdv String
  | ModifiedAdv String AdvPhrase
  | CoordAdv Conj AdvPhrase AdvPhrase
  deriving (Eq, Show)

data VerbPhrase
  = Intransitive String
  | Transitive String NounPhrase
  | VVComplement String VerbPhrase
  | V2VComplement String NounPhrase VerbPhrase
  | VSComplement String Sentence
  | PassiveVSComplement String Sentence
  | Copula AdjPhrase
  | CopulaNP NounPhrase
  | CopulaAdv AdvPhrase
  | SeemAdj AdjPhrase
  | SeemNP NounPhrase
  | SeemAdv AdvPhrase
  | FeelAdj AdjPhrase
  | GrowAdj AdjPhrase
  | GoAdj AdjPhrase
  | Passive String
  | Progressive VerbPhrase
  | Perfective VerbPhrase
  | VPWithAdv VerbPhrase AdvPhrase
  | CoordVP Conj VerbPhrase VerbPhrase
  deriving (Eq, Show)

data WhClause
  = SubjectWh QuestionWord VerbPhrase
  | ObjectWh QuestionWord NounPhrase String
  | SubjectDetWh QuestionWord NounPhrase VerbPhrase
  | ObjectDetWh QuestionWord NounPhrase NounPhrase String
  | AdvWh QuestionWord NounPhrase VerbPhrase
  deriving (Eq, Show)

data Sentence
  = Sentence
    { tense    ∷ Tense
    , polarity ∷ Polarity
    , subject  ∷ NounPhrase
    , verb     ∷ VerbPhrase
    }
  | SentenceWithAdv Sentence AdvPhrase
  | Vocative Sentence NounPhrase
  | Question Tense Polarity NounPhrase VerbPhrase
  | WhQuestion Tense Polarity WhClause
  | Existential Tense Polarity NounPhrase
  | Imperative Polarity VerbPhrase
  deriving (Eq, Show)
