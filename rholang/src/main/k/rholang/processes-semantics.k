module PROCESSES-SEMANTICS
imports RHO-SYNTAX 
// PROCESSES
// Nil process
rule Nil => .

// Evaluation and Quotation: Inverses
rule <k> ... * @ P:HigherProc  => P ... </k>
rule <k> ... @ * C:Chan  => C ... </k>

// Parallel processes
// This is fully nondeterministic, as desired
rule <thread> ... <k> ... P1:HigherProc | P2:HigherProc => P1 ... </k> ... </thread>
     (.Bag => <thread> ... <k> P2 </k> ... </thread>)

// New construct
rule <thread>  ... <k> new X:Id in { P:HigherProc } => P[ @ #(I:Int) / X] </k>  ... </thread>
     <NewIntGenerator> I => I +Int 1 </NewIntGenerator> [newchannel]

// New construct which supports lists of new channels, and not just one.
// Since Z:Ids could be an empty list, we need this recursive rule to have two
// nontrivial channels, or more. If we only include one the recursion will go on
// infinitely.
rule <thread>  ... <k> new X:Id, Y:Id, Z:Ids in { P:HigherProc }
                       => new X:Id in { new Y:Id, Z:Ids in { P:HigherProc } }</k>  ... </thread>

// Syntactic sugar for contracs as persistent sends/receives listening only on one channel.
rule <k> contract D:Chan(C:Chans) = { P:HigherProc } => for(C <= D){ P } </k>

// Delete empty cells and threads
rule <thread> ...  <k> .K </k> =>. ... </thread>
rule <thread> ... <subst> .K </subst> => . ... </thread>
rule <thread> ...  <in> .K </in>  => . ... </thread>
rule <thread> ... <out> .K </out> => . ... </thread>
rule <thread> ... <InData> .K </InData> => . ... </thread>
rule <thread> ... <OutData> .K </OutData> => . ... </thread>
rule <thread> . </thread> => .

// Delete k-cells which have a ground term in them
rule <thread> ... <k> G:Ground => . </k> ... </thread>
endmodule
