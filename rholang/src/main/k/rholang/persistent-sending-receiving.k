module PERSISTENT-SENDING-RECEIVING
imports SENDING-RECEIVING 
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //
// PERSISTENT SENDING AND RECEIVING
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //

// 1. Persistent sending
// We create an "out" cell just as we would have previously.
rule <thread>
        (<k> C:Chan!!(P:HigherProcs) </k> => <out> C!!(P) </out>)
        (.Bag => <OutData> I ; L </OutData> )
     </thread>
     <NewIntGenerator> I => I +Int 1 </NewIntGenerator>
     <GlobalListofOutIds> ... .List => ListItem(I) ... </GlobalListofOutIds>
     <GlobalListofInIds> L </GlobalListofInIds> [outcreation]

// Verify the ID of one is in the list of the other
// If they are, begin checking if there is a match.
// Do this without absorbing the "in" cell.

// (a) the "in"'s ID is in the "send"'s list of receives
rule
    <thread>
        <out> C:Chan!!(P:HigherProcs) </out>
        <OutData> I:Int ; L:List </OutData>
     </thread>
     // Combine the two threads
     <thread>
        <in> for(D:Chans <- C){ Q:HigherProc } </in>
        <InData> J ; ListHead:List (ListItem(I) => .) ListTail:List </InData>
        // Import all the information from the out-cell
        (.Bag => <out> "persistent" </out>)
        // Begin checking the match in the k-cell
        (.Bag => <k> listmatch["STDMATCH"]( P ; D ) </k>)
        // Keep the body of the listen statement to rewrite as we go along
        (.Bag => <subst> Q </subst>)
      </thread>


// (b) the (persistent) send's ID is in the receive's list of sends
rule <thread>
       <out> C:Chan!!(P:HigherProcs) </out>
       <OutData> J:Int ; ListHead:List (ListItem(I) => .) ListTail:List </OutData>
    </thread>
    <thread> ...
       <in> for(D:Chans <- C){ Q:HigherProc } </in>
       <InData> I:Int ; L:List </InData>
       // Import all the information from the out-cell
       (.Bag => <out> "persistent" </out>)
       // Begin checking the match in the k-cell
       (.Bag => <k> listmatch["STDMATCH"]( P ; D ) </k>)
       // Keep the body of the listen statement to rewrite as we go along
       (.Bag => <subst> Q </subst>)
    </thread>

// If there is ever a $(MATCHFAIL), we have not matched and we separate the thread
// into their original in- and out-threads.
rule <thread> ...
        // The trigger
        <k> #(MATCHFAIL)[ "STDMATCH" ] ~> _ => . </k>
        // Check to make sure the receive is not persistent
        <in> for(D:Chans <- C){ Q:HigherProc } </in>
        // Delete the rest
        <out> "persistent" => . </out>
        <subst> B => . </subst>
    ... </thread>

// When we've finished matching, we're ready to execute Q after having replaced all the variables
rule <thread>
        <in> for(D:Chans <- C){ P:HigherProc } => . </in>
        <InData> I1:Int ; L1:List => . </InData>
        <out> "persistent" => . </out>
        <subst> Q:HigherProc  => . </subst>
     </thread>
     (.Bag => <thread> <k> Q </k> </thread>)
     <GlobalListofInIds> ... ListItem(I1) => . ... </GlobalListofInIds>


// 2. Persistent receiving

// The same idea as before, but for persistent listens. This means that
// We don't absorb the "in" cell, and when there's a match we don't delete
// the "in" cell.
// Other than that, the structure is essentially the same.

// Put a persistent send into its <in> cell, as expected.
rule <thread> <k> for(D:Chans <= C){ Q:HigherProc } => . </k>
       (.Bag => <in> for(D:Chans <= C){ Q:HigherProc } </in> )
       (.Bag => <InData> I:Int ; L:List </InData> )
    </thread>
    <NewIntGenerator> I => I +Int 1 </NewIntGenerator>
    <GlobalListofOutIds> L </GlobalListofOutIds>
    <GlobalListofInIds> ... .List => ListItem(I) ... </GlobalListofInIds> [increation]

// Verify the ID of one is in the list of the other
// If they are, begin checking if there is a match.
// Do this without absorbing the "in" cell.

// (a) the (persistent) in's ID is in the send's list of receives
rule <thread>
       <out> C:Chan!(P:HigherProcs) </out>
       <OutData> J:Int ; ListHead:List (ListItem(I) => .) ListTail:List </OutData>
       (.Bag => <k> listmatch["STDMATCH"]( P ; D ) </k>)
       // Tag this ineraction as persistent
       (.Bag => <in> "persistent" </in>)
       // Keep the body of the listen statement to rewrite as we go along
       (.Bag => <subst> Q </subst>)
    </thread>
    // Since this receive is persistent, instead of deleting it like we do
    // in the normal receive case, we duplicate it so that it stays around.
    <thread>
       <in> for(D:Chans <= C){ Q:HigherProc } </in>
       <InData> I:Int ; L:List </InData>
    </thread>

// (b) the send's ID is in the (persistent) receive's list of sends
rule <thread> ...
       <out> C:Chan!(P:HigherProcs) </out>
       <OutData> J:Int ; L:List </OutData>
       // Tag this ineraction as persistent
       (.Bag => <in> "persistent" </in>)
       // Begin checking the match in the k-cell
       (.Bag => <k> listmatch["STDMATCH"]( P ; D ) </k>)
       // Keep the body of the listen statement to rewrite as we go along
       (.Bag => <subst> Q </subst>) ...
    </thread>
    // Delete this thread, as we abosrbed it into the other thread
    // (we do this by emptying all the cells, which rewrites to . )
    <thread>
       <in> for(D:Chans <= C){ Q:HigherProc } </in>
       <InData> I:Int ; ListHead:List (ListItem(J) => .) ListTail:List </InData>
    </thread>


// If there is ever a $(MATCHFAIL), we have not matched and we separate the thread
// into their original in- and out-threads.
rule <thread>
        // Keep the out-thread intact, deleting everything else
        <out> C:Chan!(P:HigherProcs) </out>
        <OutData> S:Int ; L:List </OutData>
        // The trigger
        <k> #(MATCHFAIL)[ "STDMATCH" ] ~> _ => . </k>
        // Check to make sure the receive was persistent
        <in> "persistent" => . </in>
        <subst> B => . </subst>
    </thread>


// When we've finished matching, we're ready to execute Q after having replaced all the variables
// Note that we don't delete the "in" cell.
rule <thread>
        <in> "persistent" => . </in>
        <out> C:Chan!(P:HigherProcs) => . </out>
        <OutData> I:Int ; L:List => . </OutData>
        <subst> Q:HigherProc  => . </subst>
     </thread>
     (.Bag => <thread> <k> Q </k> </thread>)
     <GlobalListofOutIds> ... ListItem(I) => . ... </GlobalListofOutIds>


// 3. Finally, we need semantics for how a persistent send and receive can interact.
//    We note that allowing them to interact can introduce infinite loops.
//    Previously, with matching, we have absorbed the data from either an <in> or
//    <out> cell into (resp.) an <out> or <in> cell, barring the case that one is
//    persistent. Since both are persistent, we create a new cell which will go through
//    the matching calculations.

// Create the new cell
rule <thread> ...
        <out> C:Chan!!(P:HigherProcs) </out>
        <OutData> I:Int ; ListHead:List (ListItem(J) => .) ListTail:List </OutData>
     ... </thread>
     <thread> ...
        <in> for(D:Chans <= C){ Q:HigherProc } </in>
        <InData> J:Int ; M:List </InData>
     ... </thread>
     (.Bag => <thread> ...
        <k> listmatch["STDMATCH"](P ; D) </k>
        <subst> Q </subst>
        <in> "persistent" </in>
        <InData> J ; .List </InData>
        <out> "persistent" </out>
        <OutData> I ; .List </OutData>
      ... </thread>)

rule <thread> ...
        <out> C:Chan!!(P:HigherProcs) </out>
        <OutData> I:Int ; L:List </OutData>
     ... </thread>
     <thread> ...
        <in> for(D:Chans <= C){ Q:HigherProc } </in>
        <InData> J:Int ; ListHead:List (ListItem(I) => .) ListTail:List </InData>
     ... </thread>
     (.Bag => <thread> ...
        <k> listmatch["STDMATCH"](P ; D) </k>
        <subst> Q </subst>
        <in> "persistent" </in>
        <InData> J ; .List </InData>
        <out> "persistent" </out>
        <OutData> I ; .List </OutData>
      ... </thread>)

// If there's been a match, we need to replace the Id number we deleted from the last step
// This will initiate an infinite loop.
rule <thread> ...
        <subst> Q => . </subst>
        <in> "persistent" => . </in>
        <InData> J ; .List => . </InData>
        <out> "persistent" => . </out>
        <OutData> I ; .List => . </OutData> ...
     </thread>
     <thread> ... <InData> J ; L:List </InData> ... </thread>
     <thread> ... <OutData> I ; (.List => ListItem(J)) M:List </OutData> ... </thread>
     (.Bag => <thread> ... <k> Q </k> ... </thread>)

rule <thread> ...
        <subst> Q => . </subst>
        <in> "persistent" => . </in>
        <InData> J ; .List => . </InData>
        <out> "persistent" => . </out>
        <OutData> I ; .List => . </OutData> ...
     </thread>
     <thread> ... <InData> J ; (.List => ListItem(I)) L:List  </InData> ... </thread>
     <thread> ... <OutData> I ; M:List </OutData> ... </thread>
     (.Bag => <thread> ... <k> Q </k> ... </thread>)

// If there's been no match, we just delete the cell and they won't try to match again.
rule <thread> ...
        <k> #(MATCHFAIL)[ "STDMATCH" ] ~> _ => . </k>
        <subst> Q => . </subst>
        <in> "persistent" => . </in>
        <InData> J ; .List => . </InData>
        <out> "persistent" => . </out>
        <OutData> I ; .List => . </OutData>
     ... </thread>

endmodule
