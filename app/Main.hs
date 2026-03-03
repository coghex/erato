{-# LANGUAGE Strict, UnicodeSyntax #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Data.IORef
import System.IO (hFlush, stdout)

import PGF (languages, showExpr)
import Engine.Core.Queue
import Parser.AST
import Parser.GFParser
import Parser.Translate
import Parser.Pretty

data Command
  = TranslateSentence String
  | ShowAst
  | Quit
  deriving (Eq, Show)

main ∷ IO ()
main = do
  putStrLn "Erato (controlled + AllEng fallback)"
  putStrLn "Commands:"
  putStrLn "  :q                      quit"
  putStrLn "  :t <sentence>           translate sentence"
  putStrLn "  :d                      show last AST"
  putStrLn ""

  grammars <- loadGrammars "Grammar/EratoAbs.pgf" "Grammar/AllEngAbs.pgf"
  q <- newQueue
  lastAstRef <- newIORef Nothing

  _ <- forkIO (processLoop grammars lastAstRef q)
  inputLoop q

inputLoop ∷ Queue Command → IO ()
inputLoop q = do
  putStr "> "
  hFlush stdout
  line <- getLine
  case line of
    (':':'q':_) -> writeQueue q Quit
    (':':'d':_) -> writeQueue q ShowAst >> inputLoop q
    (':':'t':' ':rest) -> writeQueue q (TranslateSentence (stripQuotes rest)) >> inputLoop q
    _ -> putStrLn "Unknown command. Use :t <sentence>, :d, or :q" >> inputLoop q

processLoop ∷ GrammarBundle → IORef (Maybe Sentence) → Queue Command → IO ()
processLoop grammars lastAstRef q = go
  where
    go = do
      m <- tryReadQueue q
      case m of
        Nothing -> threadDelay 10000 >> go
        Just Quit -> putStrLn "Bye."
        Just ShowAst -> do
          mAst <- readIORef lastAstRef
          case mAst of
            Nothing -> putStrLn "[info] no AST available yet"
            Just ast -> putStrLn (renderSentenceTree ast)
          go
        Just (TranslateSentence s) -> do
          let controlled = parseControlled grammars s
          case controlled of
            (e:_) ->
              case exprToSentence e of
                Just ast -> do
                  writeIORef lastAstRef (Just ast)
                  putStrLn ("[ok] " <> translateSentence ast)
                Nothing  -> do
                  putStrLn "[warn] controlled parse succeeded but AST conversion failed — using fallback"
                  fallback s
            [] -> do
              putStrLn "[warn] controlled parse failed (0 parses) — using fallback"
              fallback s
          go

    fallback s = do
      let fall = parseFallbackAllEng grammars s
      if null fall
        then putStrLn ("[fallback] " <> translateFallback s)
        else putStrLn ("[fallback] " <> translateFallback s)

stripQuotes ∷ String → String
stripQuotes s =
  case s of
    ('"':rest) -> reverse (dropWhile (== '"') (reverse rest))
    _          -> s
