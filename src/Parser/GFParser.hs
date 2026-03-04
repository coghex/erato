{-# LANGUAGE Strict, UnicodeSyntax #-}

module Parser.GFParser
  ( GrammarBundle(..)
  , loadGrammars
  , parseControlled
  , parseFallbackAllEng
  ) where

import PGF
import Parser.Translate (exprToSentence, exprProperNouns, validateExpr)

data GrammarBundle = GrammarBundle
  { controlledPgf   ∷ PGF
  , fallbackPgf     ∷ PGF
  , controlledLang  ∷ CId
  , controlledMorpho ∷ Morpho
  }

loadGrammars ∷ FilePath → FilePath → IO GrammarBundle
loadGrammars controlledPath fallbackPath = do
  c <- readPGF controlledPath
  f <- readPGF fallbackPath
  let lang   = mkCId "EratoEng"
      morpho = buildMorpho c lang
  pure (GrammarBundle c f lang morpho)

parseControlled ∷ GrammarBundle → String → [Expr]
parseControlled bundle input =
  let pgf    = controlledPgf bundle
      lang   = controlledLang bundle
      morpho = controlledMorpho bundle
      typ    = startCat pgf
      parses = parse pgf lang typ input
  in filter (validParse morpho) parses

parseFallbackAllEng ∷ GrammarBundle → String → [Expr]
parseFallbackAllEng bundle input =
  let pgf  = fallbackPgf bundle
      lang = mkCId "AllEng"
      typ  = startCat pgf
  in parse pgf lang typ input

validParse ∷ Morpho → Expr → Bool
validParse morpho expr =
  case validateExpr expr of
    Nothing         → False
    Just (_, pns)   → all (\w → null (lookupMorpho morpho w)) pns
