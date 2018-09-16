module PERSISTENT-SENDING-RECEIVING
imports SENDING-RECEIVING

// *******************************************************************
//                 PERSISTENT-SENDING-RECEIVING
// *******************************************************************

// 1. Persistent sending
// We create an "out" cell just as we would have previously.
rule <thread>
        (<k> C:HigherNamePat!!(P:HigherProcPats) </k> => <Out> C!!(P) </Out>)
        (.Bag => <OutData> !I:Int ; L:Set </OutData> )
     </thread>
     <GlobalSetofOutIds> ... .Set => SetItem(!I:Int) </GlobalSetofOutIds>
     <GlobalSetofInIds> L </GlobalSetofInIds> [outcreation]

// Verify the ID of one is in the list of the other
// If they are, begin checking if there is a match.
// Do this without absorbing the "in" cell.

// (a) the "in"'s ID is in the "send"'s set of receives
rule
    <thread>
        <Out> C:HigherNamePat!!(P:HigherProcPats) </Out>
        <OutData> I:Int ; L:Set </OutData>
     </thread>
     // Combine the two threads
     <thread>
        <In> for(D:HigherNamePats <- C){ Q:HigherProcPat } </In>
        <InData> J ; (SetItem(I) => .Set) SetTail:Set </InData>
        // Import all the information from the out-cell
        (.Bag => <Out> "persistent" </Out>)
        // Begin checking the match in the k-cell
        (.Bag => <k> aritymatch["STDMATCH"]( P ; D ) </k>)
        // Keep the body of the listen statement to rewrite as we go along
        (.Bag => <subst> Q </subst>)
      </thread>


// (b) the (persistent) send's ID is in the receive's list of sends
rule <thread>
       <Out> C:HigherNamePat!!(P:HigherProcPats) </Out>
       <OutData> J:Int ; (SetItem(I) => .Set) SetTail:Set </OutData>
    </thread>
    <thread> ...
       <In> for(D:HigherNamePats <- C){ Q:HigherProcPat } </In>
       <InData> I:Int ; L:Set </InData>
       // Import all the information from the out-cell
       (.Bag => <Out> "persistent" </Out>)
       // Begin checking the match in the k-cell
       (.Bag => <k> aritymatch["STDMATCH"]( P ; D ) </k>)
       // Keep the body of the listen statement to rewrite as we go along
       (.Bag => <subst> Q </subst>)
    </thread>

// If there is ever a $(MATCHFAIL), we have not matched and we separate the thread
// into their original in- and out-threads.
rule <thread> ...
        // The trigger
        <k> #(MATCHFAIL)[ "STDMATCH" ] ~> REST:K => .K </k>
        // Check to make sure the receive is not persistent
        <In> for(D:HigherNamePats <- C){ Q:HigherProcPat } </In>
        // Delete the rest
        <Out> "persistent" => .K </Out>
        <subst> B => .K </subst>
    ... </thread>

// When we've finished matching, we're ready to execute Q after having replaced all the
// variables
rule <thread>
        <In> for(D:HigherNamePats <- C:HigherNamePat){ P:HigherProcPat } => .K </In>
        <InData> I1:Int ; L1:Set => .K </InData>
        <Out> "persistent" => .K </Out>
        <subst> Q:HigherProcPat  => .K </subst>
     </thread>
     (.Bag => <thread> <k> Q </k> </thread>)
     <GlobalSetofInIds> SetItem(I1) => .Set ... </GlobalSetofInIds>


// 2. Persistent receiving

// The same idea as before, but for persistent listens. This means that
// We don't absorb the "in" cell, and when there's a match we don't delete
// the "in" cell.
// Other than that, the structure is essentially the same.

// Put a persistent send into its <In> cell, as expected.
rule <thread> <k> for(D:HigherNamePats <= C){ Q:HigherProcPat } => .K </k>
       (.Bag => <In> for(D:HigherNamePats <= C){ Q:HigherProcPat } </In> )
       (.Bag => <InData> !I:Int ; L:Set </InData> )
    </thread>
    <GlobalSetofOutIds> L </GlobalSetofOutIds>
    <GlobalSetofInIds> ... .Set => SetItem(!I:Int) </GlobalSetofInIds> [increation]

// Verify the ID of one is in the list of the other
// If they are, begin checking if there is a match.
// Do this without absorbing the "in" cell.

// (a) the (persistent) in's ID is in the send's list of receives
rule <thread>
       <Out> C:HigherNamePat!(P:HigherProcPats) </Out>
       <OutData> J:Int ; (SetItem(I) => .Set) SetTail:Set </OutData>
       (.Bag => <k> aritymatch["STDMATCH"]( P ; D ) </k>)
       // Tag this ineraction as persistent
       (.Bag => <In> "persistent" </In>)
       // Keep the body of the listen statement to rewrite as we go along
       (.Bag => <subst> Q </subst>)
    </thread>
    // Since this receive is persistent, instead of deleting it like we do
    // in the normal receive case, we duplicate it so that it stays around.
    <thread>
       <In> for(D:HigherNamePats <= C){ Q:HigherProcPat } </In>
       <InData> I:Int ; L:Set </InData>
    </thread>

// (b) the send's ID is in the (persistent) receive's list of sends
rule <thread> ...
       <Out> C:HigherNamePat!(P:HigherProcPats) </Out>
       <OutData> J:Int ; L:Set </OutData>
       // Tag this ineraction as persistent
       (.Bag => <In> "persistent" </In>)
       // Begin checking the match in the k-cell
       (.Bag => <k> aritymatch["STDMATCH"]( P ; D ) </k>)
       // Keep the body of the listen statement to rewrite as we go along
       (.Bag => <subst> Q </subst>) ...
    </thread>
    // Delete this thread, as we abosrbed it into the other thread
    // (we do this by emptying all the cells, which rewrites to . )
    <thread>
       <In> for(D:HigherNamePats <= C){ Q:HigherProcPat } </In>
       <InData> I:Int ; (SetItem(J) => .Set) SetTail:Set </InData>
    </thread>


// If there is ever a $(MATCHFAIL), we have not matched and we separate the thread
// into their original in- and out-threads.
rule <thread>
        // Keep the out-thread intact, deleting everything else
        <Out> C:HigherNamePat!(P:HigherProcPats) </Out>
        <OutData> S:Int ; L:Set </OutData>
        // The trigger
        <k> #(MATCHFAIL)[ "STDMATCH" ] ~> REST:K => .K </k>
        // Check to make sure the receive was persistent
        <In> "persistent" => .K </In>
        <subst> B => .K </subst>
    </thread>


// When we've finished matching, we're ready to execute Q after having replaced all the
// variables
// Note that we don't delete the "in" cell.
rule <thread>
        <In> "persistent" => .K </In>
        <Out> C:HigherNamePat!(P:HigherProcPats) => .K </Out>
        <OutData> I:Int ; L:Set => .K </OutData>
        <subst> Q:HigherProcPat  => .K </subst>
     </thread>
     (.Bag => <thread> <k> Q </k> </thread>)
     <GlobalSetofOutIds> SetItem(I) => .Set ... </GlobalSetofOutIds>


// 3. Finally, we need semantics for how a persistent send and receive can interact.
//    We note that allowing them to interact can introduce infinite loops.
//    Previously, with matching, we have absorbed the data from either an <In> or
//    <Out> cell into (resp.) an <Out> or <In> cell, barring the case that one is
//    persistent. Since both are persistent, we create a new cell which will go through
//    the matching calculations.

// Create the new cell
rule <thread> ...
        <Out> C:HigherNamePat!!(P:HigherProcPats) </Out>
        <OutData> I:Int ; (SetItem(J) => .Set) SetTail:Set </OutData>
     ... </thread>
     <thread> ...
        <In> for(D:HigherNamePats <= C){ Q:HigherProcPat } </In>
        <InData> J:Int ; M:Set </InData>
     ... </thread>
     (.Bag => <thread> ...
        <k> aritymatch["STDMATCH"](P ; D) </k>
        <subst> Q </subst>
        <In> "persistent" </In>
        <InData> J ; .Set </InData>
        <Out> "persistent" </Out>
        <OutData> I ; .Set </OutData>
      ... </thread>)

rule <thread> ...
        <Out> C:HigherNamePat!!(P:HigherProcPats) </Out>
        <OutData> I:Int ; L:Set </OutData>
     ... </thread>
     <thread> ...
        <In> for(D:HigherNamePats <= C){ Q:HigherProcPat } </In>
        <InData> J:Int ; (SetItem(I) => .Set) SetTail:Set </InData>
     ... </thread>
     (.Bag => <thread> ...
        <k> aritymatch["STDMATCH"](P ; D) </k>
        <subst> Q </subst>
        <In> "persistent" </In>
        <InData> J ; .Set </InData>
        <Out> "persistent" </Out>
        <OutData> I ; .Set </OutData>
      ... </thread>)

// If there's been a match, we need to replace the Id number we deleted from the last step
// This will initiate an infinite loop.
rule <thread> ...
        <subst> Q => .K </subst>
        <In> "persistent" => .K </In>
        <InData> J ; .Set => .K </InData>
        <Out> "persistent" => .K </Out>
        <OutData> I ; .Set => .K </OutData> ...
     </thread>
     <thread> ... <InData> J ; L:Set </InData> ... </thread>
     <thread> ... <OutData> I ; (.Set => SetItem(J)) M:Set </OutData> ... </thread>
     (.Bag => <thread> ... <k> Q </k> ... </thread>)

rule <thread> ...
        <subst> Q => .K </subst>
        <In> "persistent" => .K </In>
        <InData> J ; .Set => .K </InData>
        <Out> "persistent" => .K </Out>
        <OutData> I ; .Set => .K </OutData> ...
     </thread>
     <thread> ... <InData> J ; (.Set => SetItem(I)) L:Set  </InData> ... </thread>
     <thread> ... <OutData> I ; M:Set </OutData> ... </thread>
     (.Bag => <thread> ... <k> Q </k> ... </thread>)

// If there's been no match, we just delete the cell and they won't try to match again.
rule <thread> ...
        <k> #(MATCHFAIL)[ "STDMATCH" ] ~> REST:K => .K </k>
        <subst> Q => .K </subst>
        <In> "persistent" => .K </In>
        <InData> J ; .Set => .K </InData>
        <Out> "persistent" => .K </Out>
        <OutData> I ; .Set => .K </OutData>
     ... </thread>

endmodule
