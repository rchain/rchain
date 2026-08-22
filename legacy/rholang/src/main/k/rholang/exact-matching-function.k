module EXACT-MATCHING-FUNCTION
imports MATCHING-FUNCTION

// *******************************************************************
//                      EXACT-MATCHING-FUNCTION
// *******************************************************************
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //
// THE DEFINITION OF THE EXACT "match" FUNCTION (NOT the match process)
// This is used to match patterns within patterns, which have to be exact text matches.
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //

rule <thread> ... <k>
        match["TEXTMATCH"]( P:HigherPat ; Q:HigherPat )
     => #if toString( P:HigherPat ) ==String toString( Q:HigherPat ) #then
        Nil
        #else
        #(MATCHFAIL)["TEXTMATCH"]
        #fi
        ... </k> ... </thread>


endmodule
