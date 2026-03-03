abstract EratoAbs = {
  flags startcat = Utt ;

  cat
    Utt ; S ; Cl ; NP ; VP ;
    V ; V2 ; N ; A ; Det ; PN ;
    Tense ; Pol ; Name ;

  fun
    UttS     : S -> Utt ;

    Pred     : NP -> VP -> Cl ;
    MkS      : Tense -> Pol -> Cl -> S ;

    UseV     : V -> VP ;
    UseV2    : V2 -> NP -> VP ;
    DetCN    : Det -> N -> NP ;
    AdjCN    : A -> N -> N ;
    UseN     : N -> NP ;
    UsePN    : PN -> NP ;

    TPres    : Tense ;
    TPast    : Tense ;
    TFut     : Tense ;
    PPos     : Pol ;
    PNeg     : Pol ;

    the_Det  : Det ;
    a_Det    : Det ;
    dog_N    : N ;
    man_N    : N ;
    food_N   : N ;
    eat_V2   : V2 ;
    run_V    : V ;
    red_A    : A ;

    MkPN     : Name -> PN ;
}
