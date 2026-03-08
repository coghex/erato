{-# LANGUAGE Strict, UnicodeSyntax #-}

module Parser.Pretty
  ( renderSentenceTree
  ) where

import Parser.AST

renderSentenceTree ∷ Sentence → String
renderSentenceTree (Sentence t p subj vp) =
  unlines
    [ "Sentence"
    , "|- tense: " <> show t
    , "|- polarity: " <> show p
    , "|- subject"
    , renderNP "   " subj
    , "\\- verb"
    , renderVP "   " vp
    ]
renderSentenceTree (Question t p subj vp) =
  unlines
    [ "Question"
    , "|- tense: " <> show t
    , "|- polarity: " <> show p
    , "|- subject"
    , renderNP "   " subj
    , "\\- verb"
    , renderVP "   " vp
    ]
renderSentenceTree (Existential t p np) =
  unlines
    [ "Existential"
    , "|- tense: " <> show t
    , "|- polarity: " <> show p
    , "\\- noun"
    , renderNP "   " np
    ]
renderSentenceTree (Imperative p vp) =
  unlines
    [ "Imperative"
    , "|- polarity: " <> show p
    , "\\- verb"
    , renderVP "   " vp
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
    Passive v ->
      unlines
        [ indent <> "|- Passive"
        , indent <> "\\- " <> v
        ]
    Progressive base ->
      unlines
        [ indent <> "|- Progressive"
        , renderVP (indent <> "   ") base
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
