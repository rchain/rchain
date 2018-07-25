module SPECIFIC-MATCHING-RULES
imports MATCHING-FUNCTION

// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //
// Specific rules for each different match function
// (1) How they interact with variables
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //

// The Standard match function
// This checks to see if two patterns match, and then substitutes free variables
// from the second argument with terms from the first
// 1. Production rules : what we're putting into the function
//    In the standard case we do not allow variables in the second component; if we encounter them
//    we simply substitute
rule <thread> ... <k> ...
            match["STDMATCH"]( P:HigherProcOrChan ; D:HigherProcOrChanNoVars )
            => matchstdform["STDMATCH"]( intostdmatchform(P) ; intostdmatchform(D))
        ... </k> ... </thread>

// 1. Rules for encountering variables in the second component
rule <thread> ...
          <k> match["STDMATCH"]( P:HigherProcOrChan ; X:Id ) => fv("STDMATCH", P , X) ... </k>
     ... </thread>

// If there are no free vars, we're left with a #(VARPAUSE)["STDMATCH"]
rule <thread> ...
      <k> #(VARPAUSE)["STDMATCH" , BaseProc:HigherProcOrChan , BindingVar:Id][.List] => . ... </k>
      <subst>  Q => Q[ BaseProc / BindingVar] </subst> ... </thread>

// If there are free vars, the match fails
rule <thread> ...
      <k>
      #(VARPAUSE)["STDMATCH" , BaseProc:HigherProcOrChan , BindingVar:Id][ListItem(X) FreeVars:List]
      => #(MATCHFAIL)["STDMATCH"]
  ... </k> ... </thread>


// The "DoTheseMatch" function
// This only checks to see if two patterns will match **in the sense of the standard
// match function**, but does not do any substituting. If it encounters a variable,
// it just rewrites to nothing. Rewriting to nothing is a sign of success
// This still has the same needs as the standard match function given above
// 1. Production rules : what we're putting into the function
//    Like in the standard case, we do not allow variables in the second component; if we encounter
//    them we simply substitute
rule <thread> ... <k> ...
                 match["TFSTDMATCH"]( P:HigherProcOrChan ; D:HigherProcOrChanNoVars )
              => matchstdform["TFSTDMATCH"]( intostdmatchform(P) ; intostdmatchform(D))
              ... </k> ... </thread>

rule <thread> ... <k> ... match["TFSTDMATCH"]( P:HigherProcOrChan ; X:Id )
              => fv("TFSTDMATCH", P , X) ... </k> ... </thread>

// If there are no free vars, we're left with a #(VARPAUSE)["TFSTDMATCH"] and we just erase it,
// signifying a success.
rule <thread> ... <k> #(VARPAUSE)["TFSTDMATCH" , BaseProc:HigherProcOrChan , BindingVar:Id][.List]
                      => . </k> ... </thread>

// If there are free vars, the match fails
rule <thread> ... <k> #(VARPAUSE)[
                          "TFSTDMATCH" ,
                          BaseProc:HigherProcOrChan ,
                          BindingVar:Id][ListItem(X) FreeVars:List]
                      => #(MATCHFAIL)["TFSTDMATCH"] </k> ... </thread>


// The "DoTheseChansMatch" function
// This function will see
// 1. We allow variables in the second component
rule <thread> ... <k> ... match["CHANMATCH"]( P:HigherProcOrChan ; D:HigherProcOrChan )
              => matchstdform["CHANMATCH"]( intostdmatchform(P) ; intostdmatchform(D))
              ... </k> ... </thread>

// This isn't quite right. It won't be necessary once we have alpha equivalence
rule <thread> ... <k> ... matchstdform["CHANMATCH"](
                            "var"[X:Id] ;
                            "var"[Y:Id] ;
                            true)
                          => . ... </k> ... </thread>


// The T/F version of the "DoTheseChansMatch" function
// This function will see
// 1. We allow variables in the second component
rule <thread> ... <k> ... match["TFCHANMATCH"](
                                  P:HigherProcOrChan ;
                                  D:HigherProcOrChan )
              => matchstdform["TFCHANMATCH"](
                                  intostdmatchform(P) ;
                                  intostdmatchform(D))
                  ... </k> ... </thread>

// This isn't quite right. It won't be necessary once we have alpha equivalence
rule <thread> ... <k> ... matchstdform["TFCHANMATCH"](
                                                "var"[X:Id] ;
                                                "var"[Y:Id] ;
                                                true)
                          => . ... </k> ... </thread>


endmodule
