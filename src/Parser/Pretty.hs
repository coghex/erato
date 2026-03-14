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
renderSentenceTree (SentenceWithAdv sentence adv) =
  unlines
    [ "SentenceWithAdv"
    , "|- adv: " <> show adv
    , "\\- sentence"
    , indentBlock "   " (renderSentenceTree sentence)
    ]
renderSentenceTree (Vocative sentence np) =
  unlines
    [ "Vocative"
    , "|- sentence"
    , indentBlock "   " (renderSentenceTree sentence)
    , "\\- addressee"
    , renderNP "   " np
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
    Demonstrative form num ->
      unlines
        [ indent <> "|- Demonstrative"
        , indent <> "|- form: " <> form
        , indent <> "\\- number: " <> show num
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
        , indent <> "|- rel"
        , renderRelClauseMaybe (indent <> "   ") rel
        , indent <> "\\- noun: " <> noun
        ]
    CommonNoun det adjs noun num rel ->
      unlines
        [ indent <> "|- CommonNoun"
        , indent <> "|- det: " <> show det
        , indent <> "|- adjs: " <> show adjs
        , indent <> "|- number: " <> show num
        , indent <> "|- rel"
        , renderRelClauseMaybe (indent <> "   ") rel
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
    PassiveVSComplement v sentence ->
      unlines
        [ indent <> "|- PassiveVSComplement"
        , indent <> "|- verb: " <> v
        , indent <> "\\- clause"
        , indentBlock (indent <> "   ") (renderSentenceTree sentence)
        ]
    Copula adj ->
      unlines
        [ indent <> "|- Copula"
        , renderAdjPhrase (indent <> "   ") adj
        ]
    CopulaNP np ->
      unlines
        [ indent <> "|- CopulaNP"
        , indent <> "\\- complement"
        , renderNP (indent <> "   ") np
        ]
    CopulaAdv adv ->
      unlines
        [ indent <> "|- CopulaAdv"
        , indent <> "\\- adv: " <> show adv
        ]
    SeemAdj adj ->
      unlines
        [ indent <> "|- SeemAdj"
        , renderAdjPhrase (indent <> "   ") adj
        ]
    SeemNP np ->
      unlines
        [ indent <> "|- SeemNP"
        , indent <> "\\- complement"
        , renderNP (indent <> "   ") np
        ]
    SeemAdv adv ->
      unlines
        [ indent <> "|- SeemAdv"
        , indent <> "\\- adv: " <> show adv
        ]
    FeelAdj adj ->
      unlines
        [ indent <> "|- FeelAdj"
        , renderAdjPhrase (indent <> "   ") adj
        ]
    GrowAdj adj ->
      unlines
        [ indent <> "|- GrowAdj"
        , renderAdjPhrase (indent <> "   ") adj
        ]
    GoAdj adj ->
      unlines
        [ indent <> "|- GoAdj"
        , renderAdjPhrase (indent <> "   ") adj
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

renderAdjPhrase ∷ String → AdjPhrase → String
renderAdjPhrase indent adjPhrase =
  case adjPhrase of
    BareAdj adj ->
      indent <> "\\- " <> adj <> "\n"
    ModifiedAdj modifier base ->
      unlines
        [ indent <> "|- ModifiedAdj"
        , indent <> "|- modifier: " <> modifier
        , renderAdjPhrase (indent <> "   ") base
        ]
    CoordAdj c a b ->
      unlines
        [ indent <> "|- CoordAdj (" <> show c <> ")"
        , renderAdjPhrase (indent <> "   ") a
        , renderAdjPhrase (indent <> "   ") b
        ]

renderRelClauseMaybe ∷ String → Maybe RelClause → String
renderRelClauseMaybe indent Nothing =
  indent <> "\\- Nothing\n"
renderRelClauseMaybe indent (Just relClause) =
  renderRelClauseTree indent relClause

renderRelClauseTree ∷ String → RelClause → String
renderRelClauseTree indent relClause =
  case relClause of
    RelVP vp ->
      unlines
        [ indent <> "|- RelVP"
        , renderVP (indent <> "   ") vp
        ]
    NegRelVP vp ->
      unlines
        [ indent <> "|- NegRelVP"
        , renderVP (indent <> "   ") vp
        ]
    RelV2 verb np ->
      unlines
        [ indent <> "|- RelV2"
        , indent <> "|- verb: " <> verb
        , indent <> "\\- object"
        , renderNP (indent <> "   ") np
        ]
    NegRelV2 verb np ->
      unlines
        [ indent <> "|- NegRelV2"
        , indent <> "|- verb: " <> verb
        , indent <> "\\- object"
        , renderNP (indent <> "   ") np
        ]
    RelWhoseBe noun np ->
      unlines
        [ indent <> "|- RelWhoseBe"
        , indent <> "|- noun: " <> noun
        , indent <> "\\- subject"
        , renderNP (indent <> "   ") np
        ]
    RelPrep prep np ->
      unlines
        [ indent <> "|- RelPrep"
        , indent <> "|- prep: " <> prep
        , indent <> "\\- object"
        , renderNP (indent <> "   ") np
        ]
    RelPrepSentence prep sentence ->
      unlines
        [ indent <> "|- RelPrepSentence"
        , indent <> "|- prep: " <> prep
        , indent <> "\\- clause"
        , indentBlock (indent <> "   ") (renderSentenceTree sentence)
        ]
    PostAdj adj ->
      unlines
        [ indent <> "|- PostAdj"
        , renderAdjPhrase (indent <> "   ") adj
        ]
    RelChain left right ->
      unlines
        [ indent <> "|- RelChain"
        , renderRelClauseTree (indent <> "   ") left
        , renderRelClauseTree (indent <> "   ") right
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
