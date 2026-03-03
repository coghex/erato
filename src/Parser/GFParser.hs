{-# LANGUAGE Strict, UnicodeSyntax #-}

module Parser.GFParser
  ( GrammarBundle(..)
  , loadGrammars
  , parseControlled
  , parseFallbackAllEng
  ) where

import PGF

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
  in parse pgf lang typ input

parseFallbackAllEng ∷ GrammarBundle → String → [Expr]
parseFallbackAllEng bundle input =
  let pgf  = fallbackPgf bundle
      lang = mkCId "AllEng"
      typ  = startCat pgf
  in parse pgf lang typ input
