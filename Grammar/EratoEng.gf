concrete EratoEng of EratoAbs =
  open SyntaxEng, ParadigmsEng, GrammarEng, ResEng, ParamX in {

  lincat
    Utt   = SyntaxEng.Utt ;
    S     = SyntaxEng.S ;
    Cl    = SyntaxEng.Cl ;
    NP    = SyntaxEng.NP ;
    VP    = SyntaxEng.VP ;
    V     = SyntaxEng.V ;
    V2    = SyntaxEng.V2 ;
    N     = SyntaxEng.CN ;
    A     = SyntaxEng.A ;
    Det   = SyntaxEng.Det ;
    PN    = SyntaxEng.PN ;
    Tense = GrammarEng.Tense ;
    Pol   = GrammarEng.Pol ;
    Name  = Str ;

  lin
    UttS s = mkUtt s ;

    Pred np vp = mkCl np vp ;
    MkS t p cl = mkS t p cl ;

    UseV v = mkVP v ;
    UseV2 v2 np = mkVP v2 np ;
    DetCN det n = SyntaxEng.mkNP det n ;
    AdjCN a n = SyntaxEng.mkCN a n ;
    UseN n = SyntaxEng.mkNP n ;
    UsePN pn = SyntaxEng.mkNP pn ;

    TPres = GrammarEng.TPres ;
    TPast = GrammarEng.TPast ;
    TFut  = GrammarEng.TFut ;

    PPos = GrammarEng.PPos ;
    PNeg = GrammarEng.PNeg ;

    the_Det = the_Det ;
    a_Det   = a_Det ;

    dog_N  = lin CN {
      s = table {
        ParamX.Sg => table {ResEng.Nom => "dog" ; ResEng.Gen => "dog"} ;
        ParamX.Pl => table {ResEng.Nom => "dog" ; ResEng.Gen => "dog"}
      } ;
      g = ResEng.Neutr ;
      lock_CN = {}
    } ;

    man_N  = lin CN {
      s = table {
        ParamX.Sg => table {ResEng.Nom => "man" ; ResEng.Gen => "man"} ;
        ParamX.Pl => table {ResEng.Nom => "man" ; ResEng.Gen => "man"}
      } ;
      g = ResEng.Masc ;
      lock_CN = {}
    } ;

    food_N = lin CN {
      s = table {
        ParamX.Sg => table {ResEng.Nom => "food" ; ResEng.Gen => "food"} ;
        ParamX.Pl => table {ResEng.Nom => "food" ; ResEng.Gen => "food"}
      } ;
      g = ResEng.Neutr ;
      lock_CN = {}
    } ;

    eat_V2 = mkV2 "eat" ;
    run_V  = mkV "run" ;
    red_A  = mkA "red" ;

    MkPN n = lin PN {
      s = table {ResEng.Nom => n ; ResEng.Gen => n} ;
      g = ResEng.Neutr ;
      lock_PN = {}
    } ;
}
