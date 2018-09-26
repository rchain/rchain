module SPECIFIC-MATCHING-RULES
imports EXACT-MATCHING-FUNCTION

// *******************************************************************
//                   SPECIFIC-MATCHING-RULES
// *******************************************************************

// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //
// Specific rules for each different match function
// (1) How they interact with variables
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //

// *******************************************************************
// STANDARD MATCH

// * * * * * * * * * * * * * * * * * * * *
// This checks to see if two patterns match, and then substitutes free variables
// from the second argument with terms from the first
// 1. Production rules : what we're putting into the function
//    In the standard case we do not allow variables in the second component; if we encounter them
//    we simply substitute
rule <thread> ... <k> ...
            match["STDMATCH"]( P:HigherPat ; D:HigherPatNoVars )
            => matchstdform["STDMATCH"]( intostdmatchform(P) ; intostdmatchform(D))
        ... </k> ... </thread>

// 2. Rules for encountering variables in the second component
rule <thread> ...
          <k> match["STDMATCH"]( P:HigherPat ; X:Id ) => fv("STDMATCH", P , X) ... </k>
     ... </thread>

// If there are no free vars, we're left with a #(VARPAUSE)["STDMATCH"]
rule <thread> ...
      <k> #(VARPAUSE)["STDMATCH" , BaseProc:HigherPat , BindingVar:Id]{.Set} => .K ... </k>
      <subst>  Q => Q[ BaseProc / BindingVar] </subst> ... </thread>

// If there are free vars, the match fails
rule <thread> ...
      <k>
      #(VARPAUSE)["STDMATCH" , BaseProc:HigherPat , BindingVar:Id]{SetItem(X) FreeVars:Set}
      => #(MATCHFAIL)["STDMATCH"]
  ... </k> ... </thread>

// 3. Rules for encountering a wildcard in the second component
//    We do nothing.
rule <thread> ...
          <k> match["STDMATCH"]( P:HigherPat ; X:Wildcard ) => .K ... </k>
     ... </thread>



// * * * * * * * * * * * * * * * * * * * *
// The "DoTheseMatch" function (TF version)

// This only checks to see if two patterns will match **in the sense of the standard
// match function**, but does not do any substituting. If it encounters a variable,
// it just rewrites to nothing. Rewriting to nothing is a sign of success
// This still has the same needs as the standard match function given above
// 1. Production rules : what we're putting into the function
//    Like in the standard case, we do not allow variables in the second component; if we encounter
//    them we simply substitute
rule <thread> ... <k> ...
                 match["TFSTDMATCH"]( P:HigherPat ; D:HigherPatNoVars )
              => matchstdform["TFSTDMATCH"]( intostdmatchform(P) ; intostdmatchform(D))
              ... </k> ... </thread>

// 2. Rules for encountering variables in the second component
rule <thread> ... <k> ... match["TFSTDMATCH"]( P:HigherPat ; X:Id )
              => fv("TFSTDMATCH", P , X) ... </k> ... </thread>

// If there are no free vars, we're left with a #(VARPAUSE)["TFSTDMATCH"] and we just erase it,
// signifying a success.
rule <thread> ... <k> #(VARPAUSE)["TFSTDMATCH" , BaseProc:HigherPat , BindingVar:Id]{.Set}
                      => .K </k> ... </thread>

// If there are free vars, the match fails
rule <thread> ... <k> #(VARPAUSE)[
                          "TFSTDMATCH" ,
                          BaseProc:HigherPat ,
                          BindingVar:Id]{SetItem(X) FreeVars:Set}
                      => #(MATCHFAIL)["TFSTDMATCH"] </k> ... </thread>

// 3. Rules for encountering a wildcard in the second component
rule <thread> ... <k> ... match["TFSTDMATCH"]( P:HigherPat ; X:Wildcard )
              => .K ... </k> ... </thread>


// *******************************************************************
// MATCHING NAMES

// * * * * * * * * * * * * * * * * * * * *
// The "DoTheseHigherNamesMatch" function

// 1. We allow variables in the second component
rule <thread> ... <k> ... match["HIGHERNAMEMATCH"]( P:HigherPat ; D:HigherPat )
              => matchstdform["HIGHERNAMEMATCH"]( intostdmatchform(P) ; intostdmatchform(D))
              ... </k> ... </thread>

// This isn't quite right. It won't be necessary once we have alpha equivalence
rule <thread> ... <k> ... matchstdform["HIGHERNAMEMATCH"](
                            "variable"[X:Id] ;
                            "variable"[Y:Id] ;
                            true)
                          => .K ... </k> ... </thread>

rule <thread> ... <k> ... matchstdform["HIGHERNAMEMATCH"](
                            "wildcard"[ X:Wildcard ] ;
                            "wildcard"[ X:Wildcard ] ;
                            true)
                          => .K ... </k> ... </thread>


// The T/F version of the "DoTheseHigherNamesMatch" function
// This function will see
// 1. We allow variables in the second component
rule <thread> ... <k> ... match["TFHIGHERNAMEMATCH"](
                                  P:HigherPat ;
                                  D:HigherPat )
              => matchstdform["TFHIGHERNAMEMATCH"](
                                  intostdmatchform(P) ;
                                  intostdmatchform(D))
                  ... </k> ... </thread>

// This isn't quite right. It won't be necessary once we have alpha equivalence
rule <thread> ... <k> ... matchstdform["TFHIGHERNAMEMATCH"](
                                                "variable"[X:Id] ;
                                                "variable"[Y:Id] ;
                                                true)
                          => .K ... </k> ... </thread>

rule <thread> ... <k> ... matchstdform["TFHIGHERNAMEMATCH"](
                                                "wildcard"[ X:Wildcard ] ;
                                                "wildcard"[ X:Wildcard ] ;
                                                true)
                          => .K ... </k> ... </thread>

endmodule
