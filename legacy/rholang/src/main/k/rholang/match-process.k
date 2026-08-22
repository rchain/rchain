module MATCH-PROCESS
imports PERSISTENT-SENDING-RECEIVING

// *******************************************************************
//                        MATCH-PROCESS
// *******************************************************************

// Take the first option out and see if there's a match
rule <k> match P:HigherProcPat { Q:HigherProcPat => { R:HigherProcPat } L:HigherMatchCasesPat }
        => match["STDMATCH"](P ; Q) ~> #(MATCHPAUSE) ~> match P { L } </k>
     (.Bag => <subst> R </subst>)

// Sucess!
rule <k> #(MATCHPAUSE) ~> REST:K => P </k>
     <subst> P:HigherProcPat => .K </subst>

// Failure (we move on to the next case and try again, in order)
rule <k> #(MATCHFAIL)[ "STDMATCH" ]
      ~> Rest:K ~> #(MATCHPAUSE)
      ~> match P:HigherProcPat { L:HigherMatchCasesPat }
      => match P:HigherProcPat { L:HigherMatchCasesPat } </k>
     (<subst> SUBST:K </subst> => .Bag)

// Failure trying to match a pattern within a pattern
// (we move on to the next case and try again, in order)
rule <k> #(MATCHFAIL)[ "HIGHERNAMEMATCH" ]
      ~> Rest:K ~> #(MATCHPAUSE)
      ~> match P:HigherProcPat { L:HigherMatchCasesPat }
      => match P:HigherProcPat { L:HigherMatchCasesPat } </k>
     (<subst> SUBST:K </subst> => .Bag)

// If we run out of options, we execute Nil. (i.e. there's an implicit
// _ => {Nil} option at the end of each of these.)
rule <k> match P:HigherProcPat {.HigherMatchCasesPat } => Nil </k>

endmodule
