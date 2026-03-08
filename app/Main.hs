{-# LANGUAGE Strict, UnicodeSyntax #-}

module Main where

import Control.Concurrent (forkIO)
import Data.IORef
import System.IO (hFlush, stdout)

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

  _ <- forkIO (inputLoop q)
  processLoop grammars lastAstRef q

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
      command <- readQueue q
      case command of
        Quit -> putStrLn "Bye."
        ShowAst -> do
          mAst <- readIORef lastAstRef
          case mAst of
            Nothing -> putStrLn "[info] no AST available yet"
            Just ast -> putStrLn (renderSentenceTree ast)
          go
        TranslateSentence s -> do
          case parsePreferredControlledSentence grammars s of
            Just ast -> do
              writeIORef lastAstRef (Just ast)
              putStrLn ("[ok] " <> translateSentence ast)
            Nothing -> do
              putStrLn "[warn] controlled parse failed (0 accepted parses) — using fallback"
              fallback s
          go

    fallback s = do
      case parsePreferredFallbackSentence grammars s of
        Just ast -> do
          writeIORef lastAstRef (Just ast)
          putStrLn ("[fallback] " <> translateSentence ast)
        Nothing ->
          putStrLn ("[fallback] " <> translateFallback s)

stripQuotes ∷ String → String
stripQuotes s =
  case s of
    ('"':rest) -> reverse (dropWhile (== '"') (reverse rest))
    _          -> s
