module MATCHING-FUNCTION
imports MATCH-PROCESS
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //
// THE DEFINITION OF THE GENERAL "match" FUNCTION (NOT the match process)
// This is the main engine for pattern matching. We will need this same algorithm
// in slightly nuanced forms throughout the framework, so we attach the identifying
// string to each instance of the match function.
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //

// Take the tuples one at a time
rule <thread> ... <k> ... listmatch[ S:String ]( P:HigherProc, Q:HigherProcs ; D:Chan, E:Chans )
                      => listmatch[ S ]( Q ; E ) ~> match[S]( @P ; D )  ... </k> ... </thread>

// When they're both done, get rid of the listmatch bit and get going on matching
rule <thread> ... <k> ... listmatch[ S:String ]( .HigherProcs ; .Chans )
                      => . ... </k> ... </thread>

// They fail if the tuples are not the same length
rule <thread> ... <k> ... listmatch[ S:String ]( .HigherProcs ; D:Chan, E:Chans )
                      => #(MATCHFAIL)[S] ... </k> ... </thread>
rule <thread> ... <k> ... listmatch[ S:String ]( P:HigherProc, Q:HigherProcs ; .Chans )
                      => #(MATCHFAIL)[S] ... </k> ... </thread>

// We also split things up if there are logical connectives: "and's" or "or's"
// (i) The "and's" are straightforward, since we need to match with all of them
rule <thread> ... <k> ... match[S:String]( P:HigherProcOrChan ; C:Chan /\ D:Chan )
               => match[S](P ; C) ~> match[S]( P ; D ) ... </k> ... </thread>

// (ii) The "or's" are a little more complicated, but not by much
syntax OrCheckSyntax ::= "["HigherProcOrChan"]["Chan"]["List"]"

rule <thread> ... <k> match[S:String]( P:HigherProcOrChan ; C:Chan \/ D:Chan )
               =>  #(ORPAUSE)[S] ... </k>
               (.Bag => <OrCheck> [P][.K][ListItem(C) ListItem(D)]  </OrCheck>) ... </thread>

// Make the first choice nondetermistically
rule <thread> ... <k> #(ORPAUSE)[S:String] => match["TF" +String S](P ; C) ~> #(ORPAUSE)[S] ... </k>
               <OrCheck> [P][(.K => C)][ListHead:List (ListItem(C:Chan) => .) ListTail:List] </OrCheck> ... </thread>

// If the match fails, keep checking
rule <thread> ... <k> ... #(MATCHFAIL)[ TF:String ] ~> #(ORPAUSE)[S:String] => match[TF](P ; D) ~> #(ORPAUSE)[S] ... </k>
                  <OrCheck> [P:HigherProcOrChan][(C:Chan => D)][ListHead:List (ListItem(D:Chan) => .) ListTail:List] </OrCheck> ... </thread>
                  requires TF ==String ("TF" +String S)

// If the match succeeds, substitute
rule <thread> ... <k> #(ORPAUSE)[S:String] => match[S](P ; C) ... </k>
                  (<OrCheck> [P:HigherProcOrChan][C:Chan][L:List] </OrCheck> => .) ... </thread>

// If nothing succeeds, then these do not match
rule <thread> ... <k> ... #(MATCHFAIL)[ TF:String ] ~> #(ORPAUSE)[S:String] => #(MATCHFAIL)[ S ] ... </k>
                  (<OrCheck> [P:HigherProcOrChan][C:Chan][.List] </OrCheck> => .) ... </thread>
                  requires TF ==String ("TF" +String S)

// In order to be sure that the "or"s break up sufficiently, we need to tell "match["TFSTDMATCH"]" how to handle them.
rule <thread> ... <k> ... match[TF:String]( P:HigherProcOrChan ; (C:Chan \/ D:Chan => C) ) ... </k>
                  <OrCheck> [P:HigherProcOrChan][(C \/ D => C)][(.List => ListItem(D)) L:List] </OrCheck> ... </thread>

// Because of precedence rules, we may end up with "and"s in the match["TFSTDMATCH"]( ) function
// In that case, we have to check if both match. If we do, we'll send it to the match( ) function
rule <thread> ... <k> ... match[TF:String]( P:HigherProcOrChan ; C:Chan /\ D:Chan )
                  => match[TF]( P:HigherProcOrChan ; C ) ~> match[TF]( P:HigherProcOrChan ; D ) ... </k> ... </thread>

// (b) Put everything into a standard form that's easy to work with
rule intostdmatchform(@P:HigherProc) => "quote"[ P ]
rule intostdmatchform(*C:Chan) => "eval"[ C ]
rule intostdmatchform(for(C:Chan <- D:Chan){P:HigherProc}) => "listen"[C][D][P]
rule intostdmatchform(for(C:Chan <= D:Chan){P:HigherProc}) => "persistentlisten"[C][D][P]
rule intostdmatchform(contract D:Chan(C:Chan) = { P:HigherProc }) => "persistentlisten"[C][D][P]
rule intostdmatchform(new X:Ids in { P:HigherProc }) => "new"[X][P]
rule intostdmatchform(match P:HigherProcOrChan { H:HigherMatchCases} ) => "match"[P][H]
rule intostdmatchform(C:Chan!(P:HigherProc)) => "send"[C][P]
rule intostdmatchform(C:Chan!!(P:HigherProc)) => "persistentsend"[C][P]
rule intostdmatchform( Nil ) => "Nil"[ ]
// Expressions
rule intostdmatchform(S:String) => "string"[S]
rule intostdmatchform(B:Bool) => "bool"[B]
rule intostdmatchform(I:Int) => "int"[I]
// Misc
rule intostdmatchform( #(I:Int) ) => "hash"[I]
rule intostdmatchform( P1:HigherProc | P2:HigherProc ) => "par"[P1][P2]
rule intostdmatchform( X:Id ) => "var"[X]


// "matchstdform" goes through a couple steps; it will take two arguments, and then check to see
// if they are of the same syntactic sort. If they are not, we fail; if they are, we keep going.
// This is a recursive check for matching.
rule <thread> ... <k> ... matchstdform[S:String]( S1:String P1:StringlessMatchFormSyntax ; S2:String P2:StringlessMatchFormSyntax )
              => matchstdform[S]( S1 P1 ; S2 P2 ; S1 ==String S2 ) ... </k> ... </thread>

// To account for expressions
// Strings
rule <thread> ... <k> ... matchstdform[S:String]( "string"[S1:String] ; "string"[S2:String] ; true )
                    => matchstdform[S]( S1:String ; S2:String ; S1 ==String S2) ... </k> ... </thread>
rule <thread> ... <k> ... matchstdform[S:String]( S1:String ; S2:String ; true ) => . ... </k> ... </thread>
rule <thread> ... <k> ... matchstdform[S:String]( S1:String ; S2:String ; false ) => #(MATCHFAIL)[S] ... </k> ... </thread>

// Ints
rule <thread> ... <k> ... matchstdform[S:String]( "int"[I1:Int] ; "int"[I2:Int] ; true )
                    => matchstdform[S]( I1:Int ; I2:Int ; I1 ==Int I2) ... </k> ... </thread>
rule <thread> ... <k> ... matchstdform[S:String]( I1:Int ; I2:Int ; true ) => . ... </k> ... </thread>
rule <thread> ... <k> ... matchstdform[S:String]( I1:Int ; I2:Int ; false ) => #(MATCHFAIL)[S] ... </k> ... </thread>

// Bools
rule <thread> ... <k> ... matchstdform[S:String]( "bool"[B1:Bool] ; "bool"[B2:Bool] ; true )
                    => matchstdform[S]( B1:Bool ; B2:Bool ; B1 ==Bool B2) ... </k> ... </thread>
rule <thread> ... <k> ... matchstdform[S:String]( B1:Bool ; B2:Bool ; true ) => . ... </k> ... </thread>
rule <thread> ... <k> ... matchstdform[S:String]( B1:Bool ; B2:Bool ; false ) => #(MATCHFAIL)[S] ... </k> ... </thread>

// As more expressions become part of rholang this will have to be updated



// Terminate if the strings do not match (i.e., the two processes are not of the same syntactic sort)
rule <thread> ... <k> ... matchstdform[S:String](S1:String P1:StringlessMatchFormSyntax ; S2:String P2:StringlessMatchFormSyntax ; false)
              => #(MATCHFAIL)[S]  ... </k> ... </thread>

rule <thread> ... <k> ... matchstdform[S:String]( E1:Exp ; E2:Exp ; false)
              => #(MATCHFAIL)[S]  ... </k> ... </thread>

// If the patterns are ever the same, as expressions or processes, we're good.
rule <thread> ... <k> ... matchstdform[S:String]( S1:String P1:StringlessMatchFormSyntax ; S1:String P1:StringlessMatchFormSyntax ; true ) => . ... </k> ... </thread>
rule <thread> ... <k> ... matchstdform[S:String](  E:Exp ; E:Exp ; true ) => . ... </k> ... </thread>

// When the pattern is a quote "@"
rule <thread> ... <k> ... matchstdform[S:String]( "quote"[P:HigherProc] ; "quote"[R:HigherProc] ; true)
          => match[S]( P ; R ) ... </k> ... </thread>

// When the pattern is an eval "*"
rule <thread> ... <k> ... matchstdform[S:String]( "eval"[C:Chan] ; "eval"[D:Chan] ; true)
          => match[S]( C ; D ) ... </k> ... </thread>

// When the pattern is a "for"
// This is not quite correct, but it's on its way. It still needs alpha equivalence, among other things.
// If you responsibly code, it will still match most things correctly. In a future update it will match
// exactly the correct terms, while failing to match anything else.
// One of the things we will need is a function that determines if there are free variables in a term.
// We will use De Brujin indices for this.
rule <thread> ... <k> ... matchstdform[S:String]("listen"[C:Chan][D:Chan][P:HigherProc] ; "listen"[E:Chan][F:Chan][R:HigherProc] ; true )
          => match["CHANMATCH"](C ; E) ~> match[S](D ; F) ~> match[S](P ; R) ... </k> ... </thread>

// THIS is how it should be.
//rule <thread> ... <k> ... matchstdform("listen"[C:Chan][D:Chan][P:HigherProc] ; "listen"[E:Chan][F:Chan][R:HigherProc] ; true )
//          => AreTheseChansEqual(C ; E) ~> match(D ; F) ~> match(P ; R) ... </k> ... </thread>


// When the pattern is a persistent "for"
// *** This is also incorrect; we need things like in the previous listen construct
rule <thread> ... <k> ... matchstdform[S:String]("persistentlisten"[C:Chan][D:Chan][P:HigherProc] ; "persistentlisten"[E:Chan][F:Chan][R:HigherProc] ; true )
          => match["CHANMATCH"](C ; E) ~> match[S](D ; F) ~> match[S](P ; R) ... </k> ... </thread>

// The (logic of the) code we would use for a more correct version

// When the pattern is a persistent "for"
//rule <thread> ... <k> ... match("persistentlisten"(C:Chan)(D:Chan)(P:HigherProc),"persistentlisten"(E:Chan)(F:Chan)(R:HigherProc), true )
//          => match("persistentlisten"(D)(P),"persistentlisten"(F)(R),C == E) ... </k> ... </thread>
//
//rule <thread> ... <k> ... match("persistentlisten"(D:Chan)(P:HigherProc),"persistentlisten"(F:Chan)(R:HigherProc),true)
//          => match(intostdmatchform(D),intostdmatchform(F)) ~> match(intostdmatchform(P),intostdmatchform(R)) ... </k> ... </thread>


// When the pattern is "send"
rule <thread> ... <k> ... matchstdform[S:String]( "send"[C:Chan][P:HigherProc] ; "send"[D:Chan][R:HigherProc] ; true)
          => match[S]( C ; D ) ~> match[S]( P ; R ) ... </k> ... </thread>

// When the pattern is "persistentsend"
rule <thread> ... <k> ... matchstdform[S:String]( "persistentsend"[C:Chan][P:HigherProc] ; "persistentsend"[D:Chan][R:HigherProc] ; true)
          => match[S]( C ; D ) ~> match[S]( P ; R ) ... </k> ... </thread>

// When the pattern is "hash"
rule <thread> ... <k> ... matchstdform[S:String]( "hash"[I:Int] ; "hash"[J:Int] ; true)
          => match[S]( I ; J ) ... </k> ... </thread>

// When the pattern is a "new"
// (i)
rule <thread> ... <k> ... matchstdform[S:String]("new"[X1:Id,L1:Ids][P:HigherProc] ; "new"[X2:Id,L2:Ids][R:HigherProc] ; true)
          => matchstdform[S]("new"[L1][ P[@ #(I:Int) / X1] ] ; "new"[L2][ R[@ #(I:Int) / X2] ] ; true) ... </k> ... </thread>
          <NewIntGenerator> I => I +Int 1 </NewIntGenerator>
// (ii) If either one creates more new variables than the other we fail
rule <thread> ... <k> ... matchstdform[S:String]("new"[X1:Id,L1:Ids][P:HigherProc] ; "new"[ .Ids ][R:HigherProc] ; true)
          => #(MATCHFAIL)[S] ... </k> ... </thread>
rule <thread> ... <k> ... matchstdform[S:String]("new"[ .Ids ][P:HigherProc] ; "new"[ X2:Id,L2:Ids ][R:HigherProc] ; true)
          => #(MATCHFAIL)[S] ... </k> ... </thread>

// (iii) When you get down to no new channels
rule <thread> ... <k> ... matchstdform[S:String]("new"[ .Ids ][P:HigherProc] ; "new"[ .Ids ][R:HigherProc] ; true)
          => match[S]( P ; R ) ... </k> ... </thread>

// When the pattern is a "match"
// We want to do the appropriate matching operations on the elements of the HigherMatchCases list first
// NOTE: This is not quite correct. Just like in the case of the "for" construct, Q1 and Q2 should be
// checked slightly more rigorously; we cannot match in patterns like this--We need to check for pattern
// equality.
rule <thread> ... <k> ... matchstdform[S:String]("match"[P1:HigherProcOrChan][Q1:HigherProcOrChan => { R1:HigherProc } H1:HigherMatchCases];"match"[P2:HigherProcOrChan][Q2:HigherProcOrChan => { R2:HigherProc } H2:HigherMatchCases]; true)
          => matchstdform[S]("match"[P1:HigherProcOrChan][H1:HigherMatchCases];"match"[P2:HigherProcOrChan][H2:HigherMatchCases]; true) ~> match[S](Q1;Q2) ~> match[S](R1;R2) ... </k> ... </thread>

// When we are done matching off the HigherMatchCases, we match the original patterns
rule <thread> ... <k> ... matchstdform[S:String]("match"[P1:HigherProcOrChan][.HigherMatchCases];"match"[P2:HigherProcOrChan][.HigherMatchCases]; true)
          => match[S](P1;P2) ... </k> ... </thread>

// If the lists of patterns-to-match are of different length, matching fails.
rule <thread> ... <k> ... matchstdform[S:String]("match"[P1:HigherProcOrChan][Q1:HigherProcOrChan => { R1:HigherProc } H1:HigherMatchCases];"match"[P2:HigherProcOrChan][.HigherMatchCases]; true)
          => #(MATCHFAIL)[S] ... </k> ... </thread>
rule <thread> ... <k> ... matchstdform[S:String]("match"[P1:HigherProcOrChan][.HigherMatchCases];"match"[P2:HigherProcOrChan][Q2:HigherProcOrChan => { R2:HigherProc } H2:HigherMatchCases]; true)
          => #(MATCHFAIL)[S] ... </k> ... </thread>

endmodule
