{-# LANGUAGE Strict, UnicodeSyntax #-}

module Parser.Pretty
  ( renderSentenceTree
  ) where

import Parser.AST

renderSentenceTree ∷ Sentence → String
renderSentenceTree s =
  unlines
    [ "Sentence"
    , "|- tense: " <> show (tense s)
    , "|- polarity: " <> show (polarity s)
    , "|- subject"
    , renderNP "   " (subject s)
    , "\\- verb"
    , renderVP "   " (verb s)
    ]

renderNP ∷ String → NounPhrase → String
renderNP indent np =
  case np of
    ProperNoun name ->
      unlines
        [ indent <> "|- ProperNoun"
        , indent <> "\\- " <> name
        ]
    Pronoun p n c ->
      unlines
        [ indent <> "|- Pronoun"
        , indent <> "|- person: " <> show p
        , indent <> "|- number: " <> show n
        , indent <> "\\- case: " <> show c
        ]
    CommonNoun det adjs noun num ->
      unlines
        [ indent <> "|- CommonNoun"
        , indent <> "|- det: " <> show det
        , indent <> "|- adjs: " <> show adjs
        , indent <> "|- number: " <> show num
        , indent <> "\\- noun: " <> noun
        ]

renderVP ∷ String → VerbPhrase → String
renderVP indent vp =
  case vp of
    Intransitive v ->
      unlines
        [ indent <> "|- Intransitive"
        , indent <> "\\- " <> v
        ]
    Transitive v obj ->
      unlines
        [ indent <> "|- Transitive"
        , indent <> "|- verb: " <> v
        , indent <> "\\- object"
        , renderNP (indent <> "   ") obj
        ]
