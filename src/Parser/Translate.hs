{-# LANGUAGE Strict, UnicodeSyntax #-}

module Parser.Translate
  ( exprToSentence
  , translateSentence
  , translateFallback
  ) where

import PGF (Expr, showExpr)
import Data.Char (isAlpha, isSpace, toLower)
import Parser.AST

-- Public-API-only: parse from showExpr
exprToSentence ∷ Expr → Maybe Sentence
exprToSentence expr = do
  sexp <- parseSExp (showExpr [] expr)
  parseSentence sexp

-- S-expression model for parsing showExpr output
data SExp
  = Atom String
  | StrLit String
  | List [SExp]
  deriving (Eq, Show)

parseSentence ∷ SExp → Maybe Sentence
parseSentence (List [Atom "MkS", t, p, cl]) = do
  tense'    <- parseTense t
  polarity' <- parsePolarity p
  (subj, vp) <- parseCl cl
  pure (Sentence tense' polarity' subj vp)
parseSentence _ = Nothing

parseTense ∷ SExp → Maybe Tense
parseTense (Atom "TPres") = Just Present
parseTense (Atom "TPast") = Just Past
parseTense (Atom "TFut")  = Just Future
parseTense _              = Nothing

parsePolarity ∷ SExp → Maybe Polarity
parsePolarity (Atom "PPos") = Just Positive
parsePolarity (Atom "PNeg") = Just Negative
parsePolarity _             = Nothing

parseCl ∷ SExp → Maybe (NounPhrase, VerbPhrase)
parseCl (List [Atom "Pred", np, vp]) = do
  subj <- parseNP np
  vps  <- parseVP vp
  pure (subj, vps)
parseCl _ = Nothing

parseNP ∷ SExp → Maybe NounPhrase
parseNP (List [Atom "DetCN", det, n]) = do
  det' <- parseDet det
  (adjs, noun) <- parseN n
  pure (CommonNoun (Just det') adjs noun)
parseNP (List [Atom "UseN", n]) = do
  (adjs, noun) <- parseN n
  pure (CommonNoun Nothing adjs noun)
parseNP (List [Atom "UsePN", pn]) = ProperNoun <$> parsePN pn
parseNP _ = Nothing

parseVP ∷ SExp → Maybe VerbPhrase
parseVP (List [Atom "UseV", v]) = Intransitive <$> parseV v
parseVP (List [Atom "UseV2", v2, np]) = do
  v <- parseV2 v2
  obj <- parseNP np
  pure (Transitive v obj)
parseVP _ = Nothing

parseDet ∷ SExp → Maybe String
parseDet (Atom "the_Det") = Just "the"
parseDet (Atom "a_Det")   = Just "a"
parseDet _                = Nothing

parseN ∷ SExp → Maybe ([String], String)
parseN (List [Atom "AdjCN", a, n]) = do
  adj <- parseA a
  (adjs, noun) <- parseN n
  pure (adj : adjs, noun)
parseN (Atom "dog_N")  = Just ([], "dog")
parseN (Atom "man_N")  = Just ([], "man")
parseN (Atom "food_N") = Just ([], "food")
parseN _               = Nothing

parseV ∷ SExp → Maybe String
parseV (Atom "run_V") = Just "run"
parseV _              = Nothing

parseV2 ∷ SExp → Maybe String
parseV2 (Atom "eat_V2") = Just "eat"
parseV2 _               = Nothing

parseA ∷ SExp → Maybe String
parseA (Atom "red_A") = Just "red"
parseA _              = Nothing

parsePN ∷ SExp → Maybe String
parsePN (List [Atom "MkPN", StrLit s]) = Just s
parsePN (StrLit s) = Just s
parsePN _ = Nothing

translateSentence ∷ Sentence → String
translateSentence s =
  unwords
    [ renderTense (tense s)
    , renderPolarity (polarity s)
    , renderNP (subject s)
    , renderVP (verb s)
    ]

translateFallback ∷ String → String
translateFallback input =
  unwords (map fantasyToken (words input))

renderTense ∷ Tense → String
renderTense Present = "ta"
renderTense Past    = "na"
renderTense Future  = "va"

renderPolarity ∷ Polarity → String
renderPolarity Positive = "ke"
renderPolarity Negative = "no"

renderNP ∷ NounPhrase → String
renderNP (ProperNoun s) = fantasyToken s
renderNP (CommonNoun det adjs noun) =
  unwords (map fantasyToken (maybe [] pure det ++ reverse adjs ++ [noun]))

renderVP ∷ VerbPhrase → String
renderVP (Intransitive v) =
  fantasyToken v
renderVP (Transitive v obj) =
  unwords [fantasyToken v, renderNP obj]

fantasyToken ∷ String → String
fantasyToken s =
  let base = map toLower (filter isAlpha s)
      seed = foldl (\acc c -> acc * 33 + fromEnum c) 7 base
      sylls = ["ka","ri","so","na","tu","mi","la","vo","xe","du"]
      pick i = sylls !! (i `mod` length sylls)
  in pick seed <> pick (seed `div` 7)

-- S-expression parsing for showExpr output

parseSExp ∷ String → Maybe SExp
parseSExp input = do
  (sexp, rest) <- parseOne (tokenize input)
  if null rest then Just sexp else Nothing

parseOne ∷ [Token] → Maybe (SExp, [Token])
parseOne [] = Nothing
parseOne (TokLParen:ts) = go [] ts
  where
    go acc (TokRParen:rest) = Just (List (reverse acc), rest)
    go acc ts' = do
      (sexp, rest) <- parseOne ts'
      go (sexp:acc) rest
parseOne (TokStr s:ts) = Just (StrLit s, ts)
parseOne (TokAtom a:ts) = Just (Atom a, ts)
parseOne (TokRParen:_) = Nothing

data Token
  = TokLParen
  | TokRParen
  | TokAtom String
  | TokStr String
  deriving (Eq, Show)

tokenize ∷ String → [Token]
tokenize [] = []
tokenize (c:cs)
  | isSpace c = tokenize cs
  | c == '('  = TokLParen : tokenize cs
  | c == ')'  = TokRParen : tokenize cs
  | c == '"'  = let (s, rest) = span (/= '"') cs
                in TokStr s : tokenize (drop 1 rest)
  | otherwise =
      let (atom, rest) = span isAtomChar (c:cs)
      in TokAtom atom : tokenize rest

isAtomChar ∷ Char → Bool
isAtomChar c = not (isSpace c) && c /= '(' && c /= ')'
