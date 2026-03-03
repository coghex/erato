concrete EratoEng of EratoAbs =
  open SyntaxEng, ParadigmsEng, GrammarEng, ResEng, ParamX, StructuralEng, ConstructorsEng, ExtraEng in {

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
    Pron  = SyntaxEng.Pron ;
    Tense = GrammarEng.Tense ;
    Pol   = GrammarEng.Pol ;
    Name  = Str ;

    oper
  pluralize : Str -> Str = \s -> case s of {
    "man" => "men" ;
    "food" => "foods" ;
    _ => s + "s"
  } ;

  lin
    PlurN n = lin CN {
      s = table {
        ParamX.Sg => n.s ! ParamX.Pl ;
        ParamX.Pl => n.s ! ParamX.Pl
      } ;
      g = n.g ;
      lock_CN = {}
    } ;
  
    dog_N  = lin CN {
      s = table {
        ParamX.Sg => table {ResEng.Nom => "dog" ; ResEng.Gen => "dog"} ;
        ParamX.Pl => table {ResEng.Nom => "dog" ; ResEng.Gen => "dog"}
      } ;
      g = ResEng.Neutr ;
      lock_CN = {}
    } ;

    dogPl_N = lin CN {
      s = table {
        ParamX.Sg => table {ResEng.Nom => "dogs" ; ResEng.Gen => "dogs"} ;
        ParamX.Pl => table {ResEng.Nom => "dogs" ; ResEng.Gen => "dogs"}
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

    manPl_N = lin CN {
      s = table {
        ParamX.Sg => table {ResEng.Nom => "men" ; ResEng.Gen => "men"} ;
        ParamX.Pl => table {ResEng.Nom => "men" ; ResEng.Gen => "men"}
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

    foodPl_N = lin CN {
      s = table {
        ParamX.Sg => table {ResEng.Nom => "foods" ; ResEng.Gen => "foods"} ;
        ParamX.Pl => table {ResEng.Nom => "foods" ; ResEng.Gen => "foods"}
      } ;
      g = ResEng.Neutr ;
      lock_CN = {}
    } ;

    UttS s = mkUtt s ;

    Pred np vp = mkCl np vp ;
    MkS t p cl = mkS t p cl ;

    UseV v = mkVP v ;
    UseV2 v2 np = mkVP v2 np ;
    DetCN det n = SyntaxEng.mkNP det n ;
    AdjCN a n = SyntaxEng.mkCN a n ;
    UseN n = SyntaxEng.mkNP n ;
    UsePN pn = SyntaxEng.mkNP pn ;
    UsePron p = SyntaxEng.mkNP p ;

    TPres = GrammarEng.TPres ;
    TPast = GrammarEng.TPast ;
    TFut  = GrammarEng.TFut ;

    PPos = GrammarEng.PPos ;
    PNeg = GrammarEng.PNeg ;
    UncNeg = ExtraEng.UncNeg ;

    the_Det = SyntaxEng.mkDet SyntaxEng.the_Quant SyntaxEng.sgNum ;
    a_Det   = SyntaxEng.mkDet SyntaxEng.a_Quant SyntaxEng.sgNum ;
    thePl_Det = SyntaxEng.mkDet SyntaxEng.the_Quant SyntaxEng.plNum ;
    aPl_Det   = SyntaxEng.mkDet SyntaxEng.a_Quant SyntaxEng.plNum ;

    eat_V2 = dirV2 (irregV "eat" "ate" "eaten") ;
    run_V  = mkV "run" ;
    runS_V  = mkV "runs" ;
    red_A  = mkA "red" ;

    i_Pron    = StructuralEng.i_Pron ;
    we_Pron   = StructuralEng.we_Pron ;
    you_Pron  = StructuralEng.youSg_Pron ;
    youPl_Pron = StructuralEng.youPl_Pron ;
    he_Pron   = StructuralEng.he_Pron ;
    she_Pron  = StructuralEng.she_Pron ;
    it_Pron   = StructuralEng.it_Pron ;
    they_Pron = StructuralEng.they_Pron ;

    MkPN n = lin PN {
      s = table {ResEng.Nom => n ; ResEng.Gen => n} ;
      g = ResEng.Neutr ;
      lock_PN = {}
    } ;
}
