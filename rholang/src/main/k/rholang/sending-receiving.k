module SENDING-RECEIVING
imports PROCESSES-SEMANTICS

// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //
// SENDING AND RECEIVING
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //

// Put "sends" in out-cells, and "receives" in in-cells
// Gives each in- and out- cell an ID for matching purposes
// The "InData" and "OutData" are of the form: Id ; List of Outs/Ins (respectively) that are possible matches
// There is nontrivial nondeterminism in this rewrite rule, in the order
// in which the parallel processes are spawned. This doesn't affect the
// results of our program, but it does change the structure of where the ID
// numbers land, etc.

rule <thread>
        (<k> C:Chan!(P:HigherProcs) </k> => <out> C!(P) </out>)
        (.Bag => <OutData> I ; L </OutData> )
     </thread>
     <NewIntGenerator> I => I +Int 1 </NewIntGenerator>
     <GlobalListofOutIds> ... .List => ListItem(I) ... </GlobalListofOutIds>
     <GlobalListofInIds> L </GlobalListofInIds> [outcreation]

rule <thread> <k> for(D:Chans <- C){ Q:HigherProc } => . </k>
       (.Bag => <in> for(D:Chans <- C){ Q:HigherProc } </in> )
       (.Bag => <InData> I ; L </InData> )
    </thread>
    <NewIntGenerator> I => I +Int 1 </NewIntGenerator>
    <GlobalListofOutIds> L </GlobalListofOutIds>
    <GlobalListofInIds> ... .List => ListItem(I) ... </GlobalListofInIds> [increation]

// Verify the ID of one is in the list of the other
// If they are, combine the two threads and begin
// checking if there is a match.
// (a) the receive's ID is in the list of the send
rule
    // Delete this thread, to abosrbed it into the other thread
    // (we do this by emptying all the cells, which rewrites to . )
    <thread>
        <out> C:Chan!(P:HigherProcs) => . </out>
        <OutData> I:Int ; L:List => . </OutData>
     </thread>
     // Combine the two threads
     <thread>
        <in> for(D:Chans <- C){ Q:HigherProc } </in>
        <InData> J ; ListHead:List (ListItem(I) => .) ListTail:List </InData>
        // Import all the information from the out-cell
        (.Bag => <out> C:Chan!(P:HigherProcs) </out>)
        (.Bag => <OutData> I ; L </OutData>)
        // Begin checking the match in the k-cell
        (.Bag => <k> listmatch["STDMATCH"]( P ; D ) </k>)
        // Keep the body of the listen statement to rewrite as we go along
        (.Bag => <subst> Q </subst>)
      </thread>

// (b) the send's ID is in the list of the receive
rule <thread>
       <out> C:Chan!(P:HigherProcs) </out>
       <OutData> J ; ListHead:List (ListItem(I) => .) ListTail:List </OutData>
       // Absorb all the information from the in-thread
       (.Bag => <in> for(D:Chans <- C){ Q:HigherProc } </in>)
       (.Bag => <InData> I:Int ; L:List </InData>)
       // Begin checking the match in the k-cell
       (.Bag => <k> listmatch["STDMATCH"]( P ; D ) </k>)
       // Keep the body of the listen statement to rewrite as we go along
       (.Bag => <subst> Q </subst>)
    </thread>
    // Delete this thread, as we abosrbed it into the other thread
    // (we do this by emptying all the cells, which rewrites to . )
    <thread> ...
       <in> for(D:Chans <- C){ Q:HigherProc } => . </in>
       <InData> I ; L => . </InData>
    </thread>


// If there is ever a $(MATCHFAIL), we have not matched and we separate the thread
// into their original in- and out-threads.
rule <thread> ...
        // The trigger
        <k> #(MATCHFAIL)[ "STDMATCH" ] ~> _ => . </k>
        // Check to make sure the receive is not persistent
        <in> for(D:Chans <- C){ Q:HigherProc } </in>
        // Keep the in-thread intact, deleting everything else
        <out> O => . </out>
        <OutData> S:Int ; L:List => . </OutData>
        <subst> B => . </subst>
    ... </thread>
    // Recreate the out-thread; these will not try to match each other again
    (.Bag => <thread>
        <out> O </out>
        <OutData> S ; L </OutData>
    </thread>)

// The same for a fail in matching a channel
rule <thread> ...
        // The trigger
        <k> #(MATCHFAIL)[ "CHANMATCH" ] ~> _ => . </k>
        // Check to make sure the receive is not persistent
        <in> for(D:Chans <- C){ Q:HigherProc } </in>
        // Keep the in-thread intact, deleting everything else
        <out> O => . </out>
        <OutData> S:Int ; L:List => . </OutData>
        <subst> B => . </subst>
    ... </thread>
    // Recreate the out-thread; these will not try to match each other again
    (.Bag => <thread>
        <out> O </out>
        <OutData> S ; L </OutData>
    </thread>)


// When we've finished matching, we're ready to execute Q after having replaced all the variables
rule <thread>
        <in> for(D:Chans <- C){ P:HigherProc } => . </in>
        <InData> I1:Int ; L1:List => . </InData>
        <out> O => . </out>
        <OutData> I2:Int ; L2:List => . </OutData>
        <subst> Q:HigherProc  => . </subst>
     </thread>
     (.Bag => <thread> <k> Q </k> </thread>)
     <GlobalListofOutIds> ... ListItem(I2) => . ... </GlobalListofOutIds>
     <GlobalListofInIds> ... ListItem(I1) => . ... </GlobalListofInIds>


// For good housekeeping, delete channels from <InData> and <OutData> cells
// that aren't on the same channel
// *** We will have to wait until we can check for channel equality before
// we can do this ***
endmodule
