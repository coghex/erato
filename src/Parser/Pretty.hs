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
    CommonNoun det adjs noun num rel ->
      unlines
        [ indent <> "|- CommonNoun"
        , indent <> "|- det: " <> show det
        , indent <> "|- adjs: " <> show adjs
        , indent <> "|- number: " <> show num
        , indent <> "|- rel: " <> show rel
        , indent <> "\\- noun: " <> noun
        ]
    CoordNP c a b ->
      unlines
        [ indent <> "|- CoordNP (" <> show c <> ")"
        , renderNP (indent <> "   ") a
        , renderNP (indent <> "   ") b
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
    Copula adj ->
      unlines
        [ indent <> "|- Copula"
        , indent <> "\\- " <> adj
        ]
    VPWithAdv base adv ->
      unlines
        [ indent <> "|- VPWithAdv"
        , renderVP (indent <> "   ") base
        , indent <> "\\- adv: " <> show adv
        ]
    CoordVP c a b ->
      unlines
        [ indent <> "|- CoordVP (" <> show c <> ")"
        , renderVP (indent <> "   ") a
        , renderVP (indent <> "   ") b
        ]
