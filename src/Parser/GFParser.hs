{-# LANGUAGE Strict, UnicodeSyntax #-}

module Parser.GFParser
  ( GrammarBundle(..)
  , loadGrammars
  , parseControlled
  , parseFallbackAllEng
  ) where

import PGF
import Parser.Translate (exprToSentence)
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
  let pgf  = controlledPgf bundle
      lang = mkCId "EratoEng"
      typ  = startCat pgf
      parses = parse pgf lang typ input
      filtered = filter (maybe False (const True) . exprToSentence) parses
  in if not (null filtered)
       then filtered
       else
         let inputs = quoteFallbacks input
             parses' = concatMap (parse pgf lang typ) inputs
         in filter (maybe False (const True) . exprToSentence) parses'

parseFallbackAllEng ∷ GrammarBundle → String → [Expr]
parseFallbackAllEng bundle input =
  let pgf  = fallbackPgf bundle
      lang = mkCId "AllEng"
      typ  = startCat pgf
  in parse pgf lang typ input

quoteFallbacks ∷ String → [String]
quoteFallbacks input =
  let toks = words input
      idxs = [0 .. length toks - 1]
      quoteable = [ i | i <- idxs, not (isFunctionWord (toks !! i)) ]
  in if null quoteable
       then []
       else
         [ renderQuoted toks subset
         | k <- [1 .. length quoteable]
         , subset <- combinations k quoteable
         ]

renderQuoted ∷ [String] → [Int] → String
renderQuoted toks subset =
  unwords [ if i `elem` subset then quote tok else tok
          | (i, tok) <- zip [0..] toks
          ]

quote ∷ String → String
quote tok = "\"" <> tok <> "\""

isFunctionWord ∷ String → Bool
isFunctionWord tok =
  let t = map toLower (filter isAlpha tok)
  in t `elem`
     [ "the","a","some","does","do","not"
     , "i","me","you","he","him","she","her","it","we","us","they","them"
     ]

combinations ∷ Int → [a] → [[a]]
combinations 0 _      = [[]]
combinations _ []     = []
combinations k (x:xs) =
  map (x:) (combinations (k-1) xs) ++ combinations k xs
