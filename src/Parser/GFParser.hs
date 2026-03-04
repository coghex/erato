{-# LANGUAGE Strict, UnicodeSyntax #-}

module Parser.GFParser
  ( GrammarBundle(..)
  , loadGrammars
  , parseControlled
  , parseFallbackAllEng
  ) where

import PGF
import Parser.Translate (exprToSentence, exprProperNouns)
import Data.Char (isAlpha, toLower)

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
  let pgf    = controlledPgf bundle
      lang   = mkCId "EratoEng"
      typ    = startCat pgf
      morpho = buildMorpho pgf lang
      parses = parse pgf lang typ input
      -- Accept a parse only if:
      --   1. exprToSentence succeeds (well-formed AST)
      --   2. Every SymbPN "x" in the tree is for a word unknown to the morpho
      filtered = filter (validParse morpho) parses
  in filtered

validParse ∷ Morpho → Expr → Bool
validParse morpho expr =
  case exprToSentence expr of
    Nothing -> False
    Just _  ->
      let pns = exprProperNouns expr
      in all (\w -> null (lookupMorpho morpho w)) pns

parseFallbackAllEng ∷ GrammarBundle → String → [Expr]
parseFallbackAllEng bundle input =
  let pgf  = fallbackPgf bundle
      lang = mkCId "AllEng"
      typ  = startCat pgf
  in parse pgf lang typ input
