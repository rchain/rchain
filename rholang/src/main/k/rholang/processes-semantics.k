module PROCESSES-SEMANTICS
imports CONFIGURATION


// *******************************************************************
//                        PROCESSES-SEMANTICS
// *******************************************************************
// PROCESSES
// Nil process
rule Nil => .

// Evaluation and Quotation: Inverses
rule <k> ... * @ P:HigherProcPat  => P ... </k>
rule <k> ... @ * C:HigherNamePat  => C ... </k>

// Parallel processes
// This is fully nondeterministic, as desired
rule <thread> ... <k> ... P1:HigherProcPat | P2:HigherProcPat => P1 ... </k> ... </thread>
     (.Bag => <thread> ... <k> P2 </k> ... </thread>) [ndbParallelProcesses]

// New construct which supports lists of new channels, and not just one.
// Since Z:Ids could be an empty list, we need this recursive rule to have two
// nontrivial channels, or more. If we only include one the recursion will go on
// infinitely.
rule <thread>  ... <k> new X:Id, Y:Id, Z:Ids in { P:HigherProcPat }
              => new Y:Id, Z:Ids in { P[ @ #(!I:Int) / X] } </k>  ... </thread> [newchannelcreation]

// New construct
rule <thread> ...
        <k> new X:Id in { P:HigherProcPat } => P[ @ #(!I:Int) / X] </k>
     ... </thread> [newchannelcreation]

// Syntactic sugar for contracs as persistent sends/receives listening only on one channel.
rule <k> contract D:HigherNamePat(C:HigherNamePats) = { P:HigherProcPat } => for(C <= D){ P } </k>

// Delete empty cells and threads
rule <thread> ... <k> .K </k> => .Bag ... </thread>
rule <thread> ... <subst> .K </subst> => .Bag ... </thread>
rule <thread> ... <In> .K </In>  => .Bag ... </thread>
rule <thread> ... <Out> .K </Out> => .Bag ... </thread>
rule <thread> ... <InData> .K </InData> => .Bag ... </thread>
rule <thread> ... <OutData> .K </OutData> => .Bag ... </thread>
rule <T> ... <thread> .Bag </thread> => .Bag ... </T>

// Delete k-cells which have a ground term in them
rule <thread> ... <k> G:Ground => .K </k> ... </thread>

endmodule
