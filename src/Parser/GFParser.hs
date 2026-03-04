{-# LANGUAGE Strict, UnicodeSyntax #-}

module Parser.GFParser
  ( GrammarBundle(..)
  , loadGrammars
  , parseControlled
  , parseFallbackAllEng
  ) where

import PGF
import Parser.Translate (exprToSentence)

data GrammarBundle = GrammarBundle
  { controlledPgf ∷ PGF
  , fallbackPgf   ∷ PGF
  }

loadGrammars ∷ FilePath → FilePath → IO GrammarBundle
loadGrammars controlledPath fallbackPath = do
  c <- readPGF controlledPath
  f <- readPGF fallbackPath
  pure (GrammarBundle c f)

parseControlled ∷ GrammarBundle → String → [Expr]
parseControlled bundle input =
  let pgf  = controlledPgf bundle
      lang = mkCId "EratoEng"
      typ  = startCat pgf
      parses = parse pgf lang typ input
  in filter (maybe False (const True) . exprToSentence) parses

parseFallbackAllEng ∷ GrammarBundle → String → [Expr]
parseFallbackAllEng bundle input =
  let pgf  = fallbackPgf bundle
      lang = mkCId "AllEng"
      typ  = startCat pgf
  in parse pgf lang typ input
