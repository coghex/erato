abstract EratoAbs = {
  flags startcat = Utt ;

  cat
    Utt ; S ; Cl ; NP ; VP ;
    V ; V2 ; N ; A ; Det ; PN ; Pron ;
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
    UsePron  : Pron -> NP ;

    PlurN    : N -> N ;

    TPres    : Tense ;
    TPast    : Tense ;
    TFut     : Tense ;
    PPos     : Pol ;
    PNeg     : Pol ;
    UncNeg   : Pol ;

    the_Det  : Det ;
    a_Det    : Det ;
    thePl_Det : Det ;
    aPl_Det   : Det ;
    dog_N    : N ;
    man_N    : N ;
    food_N   : N ;
    eat_V2   : V2 ;
    run_V    : V ;
    red_A    : A ;

    i_Pron    : Pron ;
    we_Pron   : Pron ;
    you_Pron  : Pron ;
    youPl_Pron : Pron ;
    he_Pron   : Pron ;
    she_Pron  : Pron ;
    it_Pron   : Pron ;
    they_Pron : Pron ;

    MkPN     : Name -> PN ;
}
