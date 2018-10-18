module SENDING-RECEIVING
imports PROCESSES-SEMANTICS

// *******************************************************************
//                        SENDING-RECEIVING
// *******************************************************************





// Put "sends" in out-cells, and "receives" in in-cells
// Gives each in- and out- cell an ID for matching purposes
// The "InData" and "OutData" are of the form: Id ; Set of Outs/Ins (respectively) that
// are possible matches
// There is nontrivial nondeterminism in this rewrite rule, in the order
// in which the parallel processes are spawned. This doesn't affect the
// results of our program, but it does change the structure of where the ID
// numbers land, etc.

rule <thread>
        (<k> C:HigherNamePat!(P:HigherProcPats) </k> => .Bag)
        (.Bag => <Out> C!(P) </Out>)
        (.Bag => <OutData> !I:Int ; L </OutData> )
     </thread>
     <GlobalSetofOutIds> ... .Set => SetItem(!I:Int) </GlobalSetofOutIds>
     <GlobalSetofInIds> L:Set </GlobalSetofInIds> [outcreation]

rule <thread> <k> for(D:HigherNamePats <- C:HigherNamePat){ Q:HigherProcPat } => .K </k>
       (.Bag => <In> for(D <- C){ Q:HigherProcPat } </In> )
       (.Bag => <InData> !I:Int ; L </InData> )
    </thread>
    <GlobalSetofOutIds> L:Set </GlobalSetofOutIds>
    <GlobalSetofInIds> ... .Set => SetItem(!I:Int) </GlobalSetofInIds> [increation]

// Verify the ID of one is in the Set of the other
// If they are, combine the two threads and begin
// checking if there is a match.
// (a) the receive's ID is in the Set of the send
rule
    // Delete this thread, to abosrbed it into the other thread
    // (we do this by emptying all the cells, which rewrites to . )
    <thread>
        <Out> C:HigherNamePat!(P:HigherProcPats) => .K </Out>
        <OutData> I:Int ; L:Set => .K </OutData>
     </thread>
     // Combine the two threads
     <thread>
        <In> for(D:HigherNamePats <- C){ Q:HigherProcPat } </In>
        <InData> J ; (SetItem(I) => .Set) SetTail:Set </InData>
        // Import all the information from the out-cell
        (.Bag => <Out> C:HigherNamePat!(P:HigherProcPats) </Out>)
        (.Bag => <OutData> I ; L </OutData>)
        // Begin checking the match in the k-cell
        (.Bag => <k> aritymatch["STDMATCH"]( P ; D ) </k>)
        // Keep the body of the listen statement to rewrite as we go along
        (.Bag => <subst> Q </subst>)
      </thread> [sendreceivematch]

// (b) the send's ID is in the list of the receive
rule <thread>
       <Out> C:HigherNamePat!(P:HigherProcPats) </Out>
       <OutData> J ; (SetItem(I) => .Set) SetTail:Set </OutData>
       // Absorb all the information from the in-thread
       (.Bag => <In> for(D:HigherNamePats <- C){ Q:HigherProcPat } </In>)
       (.Bag => <InData> I:Int ; L:Set </InData>)
       // Begin checking the match in the k-cell
       (.Bag => <k> aritymatch["STDMATCH"]( P ; D ) </k>)
       // Keep the body of the listen statement to rewrite as we go along
       (.Bag => <subst> Q </subst>)
    </thread>
    // Delete this thread, as we abosrbed it into the other thread
    // (we do this by emptying all the cells, which rewrites to . )
    <thread> ...
       <In> for(D:HigherNamePats <- C){ Q:HigherProcPat } => .K </In>
       <InData> I ; L => .K </InData>
    </thread> [sendreceivematch]


// If there is ever a #(MATCHFAIL), we have not matched and we separate the thread
// into their original in- and out-threads.
rule <thread> ...
        // The trigger
        <k> #(MATCHFAIL)[ "STDMATCH" ] ~> REST:K => .K </k>
        // Check to make sure the receive is not persistent
        <In> for(D:HigherNamePats <- C){ Q:HigherProcPat } </In>
        // Keep the in-thread intact, deleting everything else
        <Out> O => .K </Out>
        <OutData> S:Int ; L:Set => .K </OutData>
        <subst> B => .K </subst>
    ... </thread>
    // Recreate the out-thread; these will not try to match each other again
    (.Bag => <thread>
        <Out> O </Out>
        <OutData> S ; L </OutData>
    </thread>)

// The same for a fail in matching a HigherName
rule <thread> ...
        // The trigger
        <k> #(MATCHFAIL)[ "HIGHERNAMEMATCH" ] ~> REST:K => .K </k>
        // Check to make sure the receive is not persistent
        <In> for(D:HigherNamePats <- C){ Q:HigherProcPat } </In>
        // Keep the in-thread intact, deleting everything else
        <Out> O => .K </Out>
        <OutData> S:Int ; L:Set => .K </OutData>
        <subst> B => .K </subst>
    ... </thread>
    // Recreate the out-thread; these will not try to match each other again
    (.Bag => <thread>
        <Out> O </Out>
        <OutData> S ; L </OutData>
    </thread>)


// When we've finished matching, we're ready to execute Q after having replaced all the
// variables
rule <thread>
        <In> for(D:HigherNamePats <- C){ P:HigherProcPat } => .K </In>
        <InData> I1:Int ; L1:Set => .K </InData>
        <Out> O => .K </Out>
        <OutData> I2:Int ; L2:Set => .K </OutData>
        <subst> Q:HigherProcPat  => .K </subst>
     </thread>
     (.Bag => <thread> <k> Q </k> </thread>)
     <GlobalSetofOutIds> SetItem(I2) => .Set ... </GlobalSetofOutIds>
     <GlobalSetofInIds> SetItem(I1) => .Set ... </GlobalSetofInIds>

// For good housekeeping, delete HigherNames from <InData> and <OutData> cells
// that aren't on the same HigherName
// *** We will have to wait until we can check for HigherName equality before
// we can do this ***

endmodule
