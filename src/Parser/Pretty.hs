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
renderSentenceTree (WhQuestion t p whClause) =
  unlines
    [ "WhQuestion"
    , "|- tense: " <> show t
    , "|- polarity: " <> show p
    , "\\- clause"
    , renderWhClause "   " whClause
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
    PossessedNoun owner adjs noun num rel ->
      unlines
        [ indent <> "|- PossessedNoun"
        , indent <> "|- possessor"
        , renderNP (indent <> "   ") owner
        , indent <> "|- adjs: " <> show adjs
        , indent <> "|- number: " <> show num
        , indent <> "|- rel: " <> show rel
        , indent <> "\\- noun: " <> noun
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
    VVComplement v comp ->
      unlines
        [ indent <> "|- VVComplement"
        , indent <> "|- verb: " <> v
        , indent <> "\\- complement"
        , renderVP (indent <> "   ") comp
        ]
    V2VComplement v obj comp ->
      unlines
        [ indent <> "|- V2VComplement"
        , indent <> "|- verb: " <> v
        , indent <> "|- object"
        , renderNP (indent <> "   ") obj
        , indent <> "\\- complement"
        , renderVP (indent <> "   ") comp
        ]
    VSComplement v sentence ->
      unlines
        [ indent <> "|- VSComplement"
        , indent <> "|- verb: " <> v
        , indent <> "\\- clause"
        , indentBlock (indent <> "   ") (renderSentenceTree sentence)
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
    Perfective base ->
      unlines
        [ indent <> "|- Perfective"
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

renderWhClause ∷ String → WhClause → String
renderWhClause indent whClause =
  case whClause of
    SubjectWh qword vp ->
      unlines
        [ indent <> "|- SubjectWh (" <> show qword <> ")"
        , renderVP (indent <> "   ") vp
        ]
    ObjectWh qword subj verb ->
      unlines
        [ indent <> "|- ObjectWh (" <> show qword <> ")"
        , indent <> "|- subject"
        , renderNP (indent <> "   ") subj
        , indent <> "\\- verb: " <> verb
        ]
    SubjectDetWh qword queried vp ->
      unlines
        [ indent <> "|- SubjectDetWh (" <> show qword <> ")"
        , indent <> "|- queried"
        , renderNP (indent <> "   ") queried
        , indent <> "\\- verb"
        , renderVP (indent <> "   ") vp
        ]
    ObjectDetWh qword queried subj verb ->
      unlines
        [ indent <> "|- ObjectDetWh (" <> show qword <> ")"
        , indent <> "|- queried"
        , renderNP (indent <> "   ") queried
        , indent <> "|- subject"
        , renderNP (indent <> "   ") subj
        , indent <> "\\- verb: " <> verb
        ]
    AdvWh qword subj vp ->
      unlines
        [ indent <> "|- AdvWh (" <> show qword <> ")"
        , indent <> "|- subject"
        , renderNP (indent <> "   ") subj
        , indent <> "\\- verb"
        , renderVP (indent <> "   ") vp
        ]

indentBlock ∷ String → String → String
indentBlock prefix =
  unlines . map (prefix <>) . lines
