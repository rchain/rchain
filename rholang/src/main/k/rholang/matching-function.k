module MATCHING-FUNCTION
imports MATCH-PROCESS

// *******************************************************************
//                      MATCHING-FUNCTION
// *******************************************************************
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //
// THE DEFINITION OF THE GENERAL "match" FUNCTION (NOT the match process)
// This is the main engine for pattern matching. We will need this same algorithm
// in slightly nuanced forms throughout the framework, so we attach the identifying
// string to each instance of the match function.
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //

// Now the semantics of the match function
// Take the tuples one at a time
rule <thread> ... <k> ... aritymatch[ S:String ](
                             P:HigherProcPat, Q:HigherProcPats ; D:HigherNamePat, E:HigherNamePats )
                          => aritymatch[ S ]( Q ; E ) ~> match[S]( @P ; D )  ... </k> ... </thread>

// When they're both done, get rid of the aritymatch bit and get going on matching
rule <thread> ... <k> ... aritymatch[ S:String ]( .HigherProcPats ; .HigherNamePats )
                      => .K ... </k> ... </thread>

// They fail if the tuples are not the same length
rule <thread> ... <k> ... aritymatch[ S:String ]( .HigherProcPats ;D:HigherNamePat,E:HigherNamePats)
                      => #(MATCHFAIL)[S] ... </k> ... </thread>
rule <thread> ... <k> ... aritymatch[ S:String ]( P:HigherProcPat,Q:HigherProcPats; .HigherNamePats)
                      => #(MATCHFAIL)[S] ... </k> ... </thread>

// We also split things up if there are logical connectives: "and's" or "or's"
// (i) The "and's" are straightforward, since we need to match with all of them
// For processes
rule <thread> ... <k> ... match[S:String]( P:HigherPat ; Q:HigherProcPat /\ R:HigherProcPat )
               => match[S](P ; Q) ~> match[S]( P ; R ) ... </k> ... </thread>

// (ii) The "or's" are a little more complicated, but not by much
syntax OrCheckSyntax ::= "["HigherPat"]["HigherPat"]["List"]"

// For processes
rule <thread> ... <k> match[S:String]( P:HigherPat ; Q:HigherProcPat \/ R:HigherProcPat )
               =>  #(ORPAUSE)[S] ... </k>
               (.Bag => <OrCheck> [P][.K][ListItem(Q) ListItem(R)]  </OrCheck>) ... </thread>

// Make the first choice nondetermistically
rule <thread> ... <k> #(ORPAUSE)[S:String] => match["TF" +String S](P ; C) ~> #(ORPAUSE)[S] ... </k>
               <OrCheck>
                [P][(.K => C)][(ListItem(C:HigherPat) => .List) ListTail:List]
               </OrCheck> ... </thread>

// If the match fails, keep checking
rule <thread> ...
        <k> ... #(MATCHFAIL)[ TF:String ]
                ~> #(ORPAUSE)[S:String] => match[TF](P ; D)
                ~> #(ORPAUSE)[S] ... </k>
        <OrCheck>
          [P:HigherPat][(C:HigherPat => D)][
          (ListItem(D:HigherPat) => .List) ListTail:List]
        </OrCheck> ... </thread>
        requires TF ==String ("TF" +String S)

// If the match succeeds, substitute
rule <thread> ... <k> #(ORPAUSE)[S:String] => match[S](P ; C) ... </k>
                  (<OrCheck> [P:HigherPat][C:HigherPat][L:List] </OrCheck> => .Bag)
     ... </thread>

// If nothing succeeds, then these do not match
rule <thread> ...
        <k> ... #(MATCHFAIL)[ TF:String ] ~> #(ORPAUSE)[S:String] => #(MATCHFAIL)[ S ] ... </k>
        (<OrCheck> [P:HigherPat][C:HigherPat][.List] </OrCheck> => .Bag)
     ... </thread>
        requires TF ==String ("TF" +String S)

// In order to be sure that the "or"s break up sufficiently, we need to tell "match["TFSTDMATCH"]"
// how to handle them.
// For processes
rule <thread> ...
      <k> ... match[TF:String]( P:HigherPat ; (R:HigherProcPat \/ Q:HigherProcPat => R) ) ... </k>
      <OrCheck> [P:HigherPat][(R \/ Q => R)][(.List => ListItem(Q)) L:List] </OrCheck>
 ... </thread>

// Because of precedence rules, we may end up with "and"s in the match["TFSTDMATCH"]( ) function
// In that case, we have to check if both match. If we do, we'll send it to the match( ) function
// For processes
rule <thread> ... <k> ...match[TF:String]( P:HigherPat ; Q:HigherProcPat /\ R:HigherProcPat )
                  => match[TF]( P:HigherPat ; Q )
                  ~> match[TF]( P:HigherPat ; R ) ... </k> ... </thread>

// (b) Put everything into a standard form that's easy to work with
rule intostdmatchform(@P:HigherProcPat) => "quote"[ P ]
rule intostdmatchform(*C:HigherNamePat) => "eval"[ C ]
rule intostdmatchform(for(C:HigherNamePat <- D:HigherNamePat){P:HigherProcPat})
                                                                          => "listen"[C][D][P]
rule intostdmatchform(for(C:HigherNamePat <= D:HigherNamePat){P:HigherProcPat})
                                                                => "persistentlisten"[C][D][P]
rule intostdmatchform(contract D:HigherNamePat(C:HigherNamePat) = { P:HigherProcPat })
                                                                => "persistentlisten"[C][D][P]
rule intostdmatchform(new X:Ids in { P:HigherProcPat }) => "new"[X][P]
rule intostdmatchform(match P:HigherProcPat { H:HigherMatchCasesPat} ) => "match"[P][H]
rule intostdmatchform(C:HigherNamePat!(P:HigherProcPat)) => "send"[C][P]
rule intostdmatchform(C:HigherNamePat!!(P:HigherProcPat)) => "persistentsend"[C][P]
rule intostdmatchform( Nil ) => "Nil"[ ]
// Expressions
rule intostdmatchform(S:String) => "string"[S]
rule intostdmatchform(B:Bool) => "bool"[B]
rule intostdmatchform(I:Int) => "int"[I]
// Misc
rule intostdmatchform( unforgeable(I:Int) ) => "unforgeable"[I]
rule intostdmatchform( P1:HigherProcPat | P2:HigherProcPat ) => "par"[P1][P2]
rule intostdmatchform( X:Id ) => "variable"[X]
rule intostdmatchform( X:Wildcard ) => "wildcard"[ X ]


// "matchstdform" goes through a couple steps; it will take two arguments, and then check to see
// if they are of the same syntactic sort. If they are not, we fail; if they are, we keep going.
// This is a recursive check for matching.
rule <thread> ...
        <k> ...
        matchstdform[S:String](
                    S1:String P1:StringlessMatchFormSyntax ;
                    S2:String P2:StringlessMatchFormSyntax
                    )
        => matchstdform[S]( S1 P1 ; S2 P2 ; S1 ==String S2 ) ... </k> ... </thread>

// To account for expressions
// Strings
rule <thread> ... <k> ... matchstdform[S:String]( "string"[S1:String] ; "string"[S2:String] ; true )
               => matchstdform[S]( S1:String ; S2:String ; S1 ==String S2) ... </k> ... </thread>
rule <thread> ...
        <k> ... matchstdform[S:String]( S1:String ; S2:String ; true ) => .K ... </k>
     ... </thread>
rule <thread> ...
        <k> ... matchstdform[S:String]( S1:String ; S2:String ; false ) => #(MATCHFAIL)[S] ... </k>
     ... </thread>

// Ints
rule <thread> ... <k> ... matchstdform[S:String]( "int"[I1:Int] ; "int"[I2:Int] ; true )
                    => matchstdform[S]( I1:Int ; I2:Int ; I1 ==Int I2) ... </k> ... </thread>
rule <thread> ...
        <k> ... matchstdform[S:String]( I1:Int ; I2:Int ; true ) => .K ... </k>
     ... </thread>
rule <thread> ...
        <k> ... matchstdform[S:String]( I1:Int ; I2:Int ; false ) => #(MATCHFAIL)[S] ... </k>
     ... </thread>

// Bools
rule <thread> ... <k> ... matchstdform[S:String]( "bool"[B1:Bool] ; "bool"[B2:Bool] ; true )
                    => matchstdform[S]( B1:Bool ; B2:Bool ; B1 ==Bool B2) ... </k> ... </thread>
rule <thread> ...
        <k> ... matchstdform[S:String]( B1:Bool ; B2:Bool ; true ) => .K ... </k>
     ... </thread>
rule <thread> ...
        <k> ... matchstdform[S:String]( B1:Bool ; B2:Bool ; false ) => #(MATCHFAIL)[S] ... </k>
     ... </thread>

// As more expressions become part of rholang this will have to be updated



// Terminate if the strings do not match (ie, the two processes are not of the same syntactic sort)
rule <thread> ... <k> ... matchstdform[S:String](
          S1:String P1:StringlessMatchFormSyntax ; S2:String P2:StringlessMatchFormSyntax ; false )
              => #(MATCHFAIL)[S]  ... </k> ... </thread>

rule <thread> ... <k> ... matchstdform[S:String]( E1:Exp ; E2:Exp ; false)
              => #(MATCHFAIL)[S]  ... </k> ... </thread>

// If the patterns are ever the same, as expressions or processes, we're good.
rule <thread> ... <k> ... matchstdform[S:String](
          S1:String P1:StringlessMatchFormSyntax ; S1:String P1:StringlessMatchFormSyntax ; true )
          => .K ... </k> ... </thread>
rule <thread> ...
        <k> ... matchstdform[S:String](  E:Exp ; E:Exp ; true ) => .K ... </k>
     ... </thread>

// When the pattern is a quote "@"
rule <thread> ... <k> ... matchstdform[S:String](
                        "quote"[P:HigherProcPat] ; "quote"[R:HigherProcPat] ; true)
          => match[S]( P ; R ) ... </k> ... </thread>

// When the pattern is an eval "*"
rule <thread> ... <k> ...
             matchstdform[S:String]( "eval"[C:HigherNamePat] ; "eval"[D:HigherNamePat] ; true)
          => match[S]( C ; D ) ... </k> ... </thread>

// When the pattern is a "for"
// The channels in the "for" pattern must match exactly -- not even up to alpha equivalence
rule <thread> ...
        <k> ... matchstdform[S:String](
                            "listen"[C:HigherNamePat][D:HigherNamePat][P:HigherProcPat] ;
                            "listen"[E:HigherNamePat][F:HigherNamePat][R:HigherProcPat] ;
                            true
                          )
          => match["TEXTMATCH"](C ; E)
          ~> match[S](D ; F)
          ~> match[S](P ; R) ... </k> ... </thread>

// When the pattern is a persistent "for"
// The channels in the persistent "for" pattern must match exactly--not even up to alpha equivalence
rule <thread> ... <k> ... matchstdform[S:String](
                              "persistentlisten"[C:HigherNamePat][D:HigherNamePat][P:HigherProcPat];
                              "persistentlisten"[E:HigherNamePat][F:HigherNamePat][R:HigherProcPat];
                              true
                            )
          => match["TEXTMATCH"](C ; E)
          ~> match[S](D ; F)
          ~> match[S](P ; R) ... </k> ... </thread>


// When the pattern is "send"
rule <thread> ... <k> ... matchstdform[S:String](
                            "send"[C:HigherNamePat][P:HigherProcPat] ;
                            "send"[D:HigherNamePat][R:HigherProcPat] ;
                            true)
          => match[S]( C ; D ) ~> match[S]( P ; R ) ... </k> ... </thread>

// When the pattern is "persistentsend"
rule <thread> ... <k> ... matchstdform[S:String](
                            "persistentsend"[C:HigherNamePat][P:HigherProcPat] ;
                            "persistentsend"[D:HigherNamePat][R:HigherProcPat] ;
                            true)
          => match[S]( C ; D )
          ~> match[S]( P ; R ) ... </k> ... </thread>

// When the pattern is "unforgeable"
rule <thread> ... <k>... matchstdform[S:String]( "unforgeable"[I:Int] ; "unforgeable"[J:Int] ; true)
          => match[S]( I ; J ) ... </k> ... </thread>

// When the pattern is a "new"
// (i)
rule <thread> ... <k> ... matchstdform[S:String](
                                "new"[X1:Id,L1:Ids][P:HigherProcPat] ;
                                "new"[X2:Id,L2:Ids][R:HigherProcPat] ;
                                true)
          => matchstdform[S](
                    "new"[L1][ P[@ unforgeable(!I:Int) / X1] ] ;
                    "new"[L2][ R[@ unforgeable(!I:Int) / X2] ] ;
                    true) ... </k> ... </thread>

// (ii) If either one creates more new variables than the other we fail
rule <thread> ... <k> ... matchstdform[S:String](
                    "new"[X1:Id,L1:Ids][P:HigherProcPat] ;
                    "new"[ .Ids ][R:HigherProcPat] ;
                    true)
          => #(MATCHFAIL)[S] ... </k> ... </thread>
rule <thread> ... <k> ... matchstdform[S:String](
                    "new"[ .Ids ][P:HigherProcPat] ;
                    "new"[ X2:Id,L2:Ids ][R:HigherProcPat] ;
                    true)
          => #(MATCHFAIL)[S] ... </k> ... </thread>

// (iii) When you get down to no new HigherNames
rule <thread> ... <k> ... matchstdform[S:String](
                    "new"[ .Ids ][P:HigherProcPat] ;
                    "new"[ .Ids ][R:HigherProcPat] ;
                    true)
          => match[S]( P ; R ) ... </k> ... </thread>

// When the pattern is a "match"
// We want to do the appropriate matching operations on the elements of the HigherMatchCases
// list first
// NOTE: This is not quite correct. Just like in the case of the "for" construct, Q1 and Q2 should
// be checked slightly more rigorously; we cannot match in patterns like this--We need to check for
// pattern equality.
rule <thread> ... <k> ... matchstdform[S:String](
      "match"[P1:HigherProcPat][Q1:HigherProcPat => { R1:HigherProcPat } H1:HigherMatchCasesPat];
      "match"[P2:HigherProcPat][Q2:HigherProcPat => { R2:HigherProcPat } H2:HigherMatchCasesPat];
      true)
   => matchstdform[S](
      "match"[P1:HigherProcPat][H1:HigherMatchCasesPat];
      "match"[P2:HigherProcPat][H2:HigherMatchCasesPat];
      true)
      ~> match[S](Q1;Q2)
      ~> match[S](R1;R2) ... </k> ... </thread>

// When we are done matching off the HigherMatchCases, we match the original patterns
rule <thread> ... <k> ... matchstdform[S:String](
                              "match"[P1:HigherProcPat][.HigherMatchCasesPat];
                              "match"[P2:HigherProcPat][.HigherMatchCasesPat];
                              true)
          => match[S](P1;P2) ... </k> ... </thread>

// If the lists of patterns-to-match are of different length, matching fails.
rule <thread> ... <k> ... matchstdform[S:String](
                            "match"[P1:HigherProcPat][Q1:HigherProcPat =>
                            { R1:HigherProcPat } H1:HigherMatchCasesPat];
                            "match"[P2:HigherProcPat][.HigherMatchCasesPat];
                            true)
          => #(MATCHFAIL)[S] ... </k> ... </thread>
rule <thread> ... <k> ... matchstdform[S:String](
                            "match"[P1:HigherProcPat][.HigherMatchCasesPat];
                            "match"[P2:HigherProcPat][Q2:HigherProcPat
                            => { R2:HigherProcPat } H2:HigherMatchCasesPat];
                            true)
          => #(MATCHFAIL)[S] ... </k> ... </thread>

endmodule
