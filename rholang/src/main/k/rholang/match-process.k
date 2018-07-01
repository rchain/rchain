module MATCH-PROCESS
imports PERSISTENT-SENDING-RECEIVING 
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //
// THE DEFINITION OF THE "match" PROCESS (NOT the match function)
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //

// Take the first option out and see if there's a match
rule <k> (match P:HigherProcOrChan { Q:HigherProcOrChan => { R:HigherProc } L:HigherMatchCases })
        => match["STDMATCH"](P ; Q) ~> #(MATCHPAUSE) ~> match P { L } </k>
     (.Bag => <subst> R </subst>)

// Sucess!
rule <k> #(MATCHPAUSE) ~> _ => P </k>
     <subst> P:HigherProc => . </subst>

// Failure (we move on to the next case and try again, in order)
rule <k> #(MATCHFAIL)[ "STDMATCH" ] ~> #(MATCHPAUSE) ~> match P:HigherProcOrChan { L:HigherMatchCases }
      => match P:HigherProcOrChan { L:HigherMatchCases } </k>
     (<subst> _ </subst> => .)

// If we run out of options, we execute Nil. (i.e. there's an implicit
// _ => {Nil} option at the end of each of these.)
rule <k> match P:HigherProcOrChan {.HigherMatchCases } => Nil </k>

endmodule
