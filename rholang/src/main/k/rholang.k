module RHO-SYNTAX
imports SUBSTITUTION

// We need processes, including variables
syntax Proc ::= ProcNoVars
              | Id

// We need processes, excluding variables
syntax ProcNoVars ::=
              // Ground terms, including the empty process and expressions
                Ground
              // Listen
              | "for" "(" Names "<-" Name ")" "{" Proc "}" [binder(1 -> 3)]
              // Persistent listen
              | "for" "(" Names "<=" Name ")" "{" Proc "}" [binder(1 -> 3)]
              | "contract" Name "(" Names ")" "=" "{" Proc "}"
              // Send
              | Name "!" "(" Procs ")"
              // Persistent send
              | Name "!!" "(" Procs ")"
              // Evaluate
              | "*" Name
              // New
              | "new" Ids "in" "{" Proc "}" [binder]
              // Match
              | "match" ProcOrName "{" MatchCases "}"
              // Parallel
              | Proc "|" Proc [left]
              // Bracket
              | "{" Proc "}" [bracket]

// Grouping "Nil" and Exp is useful, as they behave the same on the top-level
// but we should note here that they are NOT structurally equivalent, nor are
// they semantically equivalent in a general sense.
syntax Ground ::= "Nil"
              | Exp

// We need names with no variables or logical connectives
syntax NameNoVars ::= "@" Proc

// The general idea of a name can be a (bound) variable. It can also
// contain logical connectives. We need to distinguish between non-variable
// terms, with no logical connectives, etc and those that are or have them.
syntax Name ::= NameNoVars
              | Id
              // logical "and"
              | Name "/\\" Name [left]
              // logical "or"
              > Name "\\/" Name [left]

// Names and Processes, used specifically for -arity matching
syntax Names ::= List{ Name, "," }
syntax Procs ::= List{ Proc, "," }
syntax Ids   ::= List{Id, ","}

// A list for matching parallel processes
syntax ParMatchList ::= List{ HigherProc, ";"}

// Syntax needed for the "match" process
syntax MatchCase  ::= ProcOrName "=>" "{" Proc "}" [binder]
syntax MatchCases ::= List{MatchCase, " "}
// The same but for Higher Processes
syntax HigherMatchCase  ::= HigherProcOrChan "=>" "{" HigherProc "}" [binder]
syntax HigherMatchCases ::= List{HigherMatchCase, " "}


// Syntax that will show up b/c of the new construct,
// but that cannot be written (unforgeable things)

// For matching
syntax ProcOrName ::= Id
              | ProcOrNameNoVars
              | Name
              | Proc

syntax HigherProcOrChan ::= Id
              | HigherProcOrChanNoVars
              | Chan
              | HigherProc

// For matching, we need to make a distinction between variables and no variables
syntax ProcOrNameNoVars ::= NameNoVars
              | ProcNoVars

syntax HigherProcOrChanNoVars ::= ChanNoVars
              | HigherProcNoVars

// We need higher processes, including variables
syntax HigherProc ::= HigherProcNoVars
              | Id

// Channels and Hiher Processes, specifically for sending and listening for tuples
syntax Chans ::= List{ Chan, "," }
syntax HigherProcs ::= List{ HigherProc, "," }

// We need higher processes, excluding variables
syntax HigherProcNoVars ::=
                "for" "(" Chans "<-" Chan ")" "{" HigherProc "}" [binder(1 -> 3)]
              | "for" "(" Chans "<=" Chan ")" "{" HigherProc "}" [binder(1 -> 3)]
              | "contract" Chan "(" Chans ")" "=" "{" HigherProc "}"
              | Chan "!" "(" HigherProcs ")"
              | Chan "!!" "(" HigherProcs ")"
              | "*" Chan
              | "new" Ids "in" "{" HigherProc "}" [binder]
              | "match" HigherProcOrChan "{" HigherMatchCases "}"
              | ProcNoVars
              // Syntax for new channels--the whole reason for "Higher" anything
              | "#(" Int ")"
              | HigherProc "|" HigherProc [left]
              | "{" HigherProc "}" [bracket]

// The general idea of a channel can be a (bound) variable. It can also
// contain logical connectives. We need to distinguish between non-variable
// terms, with no logical connectives, etc and those that are or have them.
syntax Chan ::= ChanNoVars
              | Id
              // logical "and"
              | Chan "/\\" Chan [left]
              // logical "or"
              > Chan "\\/" Chan [left]

// We need channels, with no variables
syntax ChanNoVars ::= "@" HigherProc
              | NameNoVars

syntax Exp  ::= Int
              | Bool
              | String

syntax KVariable ::= Id
syntax KResult ::= Int
              | String StringlessMatchFormSyntax
              | ChanNoVars
              | Id
              | Bool

// Syntax which is needed for various matching operations below
syntax CanBeStdMatched ::= Id
              | "intostdmatchform(" HigherProcOrChan ")"
              | "intostdtfmatchform(" HigherProcOrChan ")"
              | Ground
              | ChanNoVars

syntax Fun  ::=
              // General matching functions which use the "matching engine" below
                "match" "[" String "]" "(" HigherProcOrChan ";" HigherProcOrChan ")"
              | "listmatch" "[" String "]" "(" HigherProcs ";" Chans ")"
              | "matchstdform" "[" String "]" "(" CanBeStdMatched ";" CanBeStdMatched ")" [strict(2,3)]
              | "matchstdform" "[" String "]" "(" CanBeStdMatched ";" CanBeStdMatched ";" Bool ")" [strict(3)]
              // Functions for matching parallel processes
              | "toparmatchstdform" "(" HigherProcOrChan ")"
              | "fromparmatchstdform" "(" HigherProcOrChan ")"
              // Errors and pauses, dependent on the match function we're using
              | "#(MATCHFAIL)[" String "]"
              | "#(MATCHPAUSE)[" String "]"
              | "#(ORPAUSE)[" String "]"
              | "#(MATCHFAIL)[" String "," Int "]"
              | "#(PARPAUSE)[" String "," Int "]"
              | "#(MATCHPAUSE)"

syntax StringlessMatchFormSyntax ::=
              // Quote, Eval and Var
                "[" HigherProcOrChan "]"
              // Listen and Persistent Listen, as well as Contracts
              | "[" Chan "][" Chan "][" HigherProc "]"
              // New
              | "[" Ids "][" HigherProc "]"
              // Send
              | "[" Chan "][" HigherProc "]"
              // Match
              | "[" HigherProcOrChan "][" HigherMatchCases "]"
              | "[" HigherProcOrChan "][" HigherProc "]"
              // Logical And's and Or's
              | "[" String "][" Chan "][" Chan "]"
              // Nil
              | "[" "]"
              // Hashed new channels
              | "[" Int "]"
              | "[" HigherProc "][" HigherProc "]"

// This is the syntax for the data we need to keep on our in and out cells
syntax InOutData ::= Int ";" List

endmodule
module RHO
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

// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //
// Specific rules for each different match function
// (1) How they interact with variables
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //

// The Standard match function
// This checks to see if two patterns match, and then substitutes free variables
// from the second argument with terms from the first
// 1. Production rules : what we're putting into the function
//    In the standard case we do not allow variables in the second component; if we encounter them
//    we simply substitute
rule <thread> ... <k> ... match["STDMATCH"]( P:HigherProcOrChan ; D:HigherProcOrChanNoVars )
              => matchstdform["STDMATCH"]( intostdmatchform(P) ; intostdmatchform(D)) ... </k> ... </thread>

// 1. Rules for encountering variables in the second component
rule <thread> ... <k> ... match["STDMATCH"]( P:HigherProcOrChan ; X:Id )
              => . ... </k>
              <subst> Q => Q[ P / X] </subst> ... </thread>

              // If a variable matches with something, check if that thing has free variables
              // If it does, then rewrite as #(MATCHFAIL). If it does not, then substitute S for X
              // in the original statement. Checking for free variables will come soon.
              // For now, we just substitute. The code we would use for matching is below, once
              // semantics for the "freevars( )" function is done.


              //rule <thread> ... <k> ... match["STDMATCH"](S:Stmt, X:Id) => freevars(S,X) ... </k> ... </thread>



// The "DoTheseMatch" function
// This only checks to see if two patterns will match **in the sense of the standard
// match function**, but does not do any substituting. If it encounters a variable,
// it just rewrites to nothing. Rewriting to nothing is a sign of success
// This still has the same needs as the standard match function given above
// 1. Production rules : what we're putting into the function
//    Like in the standard case, we do not allow variables in the second component; if we encounter them
//    we simply substitute
rule <thread> ... <k> ... match["TFSTDMATCH"]( P:HigherProcOrChan ; D:HigherProcOrChanNoVars )
              => matchstdform["TFSTDMATCH"]( intostdmatchform(P) ; intostdmatchform(D)) ... </k> ... </thread>

rule <thread> ... <k> ... match["TFSTDMATCH"]( P:HigherProcOrChan ; X:Id )
              => . ... </k> ... </thread>
              // If a variable matches with something, check if that thing has free variables
              // If it does, then rewrite as #(MATCHFAIL)[ "TFSTDMATCH" ]. If it does not, then substitute S for X
              // in the original statement.

              //rule <thread> ... <k> ... tfmatch(S:Stmt, X:Id) => freevars(S,X) ... </k> ... </thread>


// The "DoTheseChansMatch" function
// This function will see
// 1. We allow variables in the second component
rule <thread> ... <k> ... match["CHANMATCH"]( P:HigherProcOrChan ; D:HigherProcOrChan )
              => matchstdform["CHANMATCH"]( intostdmatchform(P) ; intostdmatchform(D)) ... </k> ... </thread>

// This isn't quite right. It won't be necessary once we have alpha equivalence, (via De Brujin indices?).
rule <thread> ... <k> ... matchstdform["CHANMATCH"]( "var"[X:Id] ; "var"[Y:Id] ; true) => . ... </k> ... </thread>


// The T/F version of the "DoTheseChansMatch" function
// This function will see
// 1. We allow variables in the second component
rule <thread> ... <k> ... match["TFCHANMATCH"]( P:HigherProcOrChan ; D:HigherProcOrChan )
              => matchstdform["TFCHANMATCH"]( intostdmatchform(P) ; intostdmatchform(D)) ... </k> ... </thread>

// This isn't quite right. It won't be necessary once we have alpha equivalence, (via De Brujin indices?).
rule <thread> ... <k> ... matchstdform["TFCHANMATCH"]( "var"[X:Id] ; "var"[Y:Id] ; true) => . ... </k> ... </thread>


// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //
/// PAR MATCHING ALGORITHM
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //

// When the pattern is "par" this process becomes a little more complicated
syntax PatternMatchingCells ::= "[" String "][" HigherProc "][" HigherProc "][" HigherProc "][" Int "][" HigherProc "]{" List "}"
          | "[" HigherProc "][" HigherProc "][" HigherProc "][" HigherProc "][" HigherProc "]"
          // For variable matching at the end
          | "[" HigherProc "][" HigherProc "][" HigherProc "][" HigherProc "][" HigherProc "]{" HigherProc "}"

// If we get a pattern or a process sent that has a "|" in it, we pause computation and invoke the par match.
// The processes sent go in the "from" cells; the patterns go in the "to" cells.
// The intuition for this is that we match from the process sent to the pattern.
// Only the "to" cells can contain free variables.
rule <thread> ...
         <k> ... matchstdform[S:String]( "par"[P1:HigherProc][P2:HigherProc] ; "par"[Q1:HigherProc][Q2:HigherProc] ; true)
                 => #(PARPAUSE)[S,I +Int 1] ... </k>
         (.Bag =>
         <ParMatch> ...
           // We need each ParMatch cell to have an ID, since it's possible to nest ParMatches inside of ParMatches
           // with correct syntax. We assign the ID in such a way that the ParMatchCounter is the same as the ParMatch ID,
           // and not one off.
           <id> I +Int 1 </id>
           <from> fromparmatchstdform(P1 | P2) </from>
           <to> toparmatchstdform(Q1 | Q2) </to>
         ... </ParMatch>)
   ... </thread>
   <ParMatchCounter> I:Int => I +Int 1 </ParMatchCounter>

// In order to systematically check for a match we need these in some kind of a standard
// form. We first need to do this. We give semantics for a "par", but the whole point of
// this is to break up the "par". Right after this we'll give rules for breaking up the "par"

// The "true" "false" bracketed part of this standard form is just to make sure that we separate
// all the "par"s before checking everything. This ensures we don't miss anything.

// First the "from" cells
// The standard form is [ process sort ][ process ][ id of the process to which it matches ][ id ][ active/inactive ]{ list of "to" cells }
// We start with negative values in the matching ID, as well as a nonsensical list. These will
// get updated as the matching procedes.
// We include process sort (the string in the first bracketed part) there for both a natural sorting to speed up computation (as rholang does)
// as well as to watch for any remaining "par"s that might mess things up.
rule <thread> ... <ParMatch> ... <from> fromparmatchstdform(P1:HigherProc | P2:HigherProc)
                  => ["par"][true][P1 | P2][ -1 ][ -1 ]["inactive"]{ListItem(-1)}  </from> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <from> fromparmatchstdform(for(C:Chan <- D:Chan){P:HigherProc})
                  => ["listen"][false][for(C <- D){P}][ -1 ][ I ]["inactive"]{ListItem(-1)}  </from>
                  <MatchFromCounter> I => I +Int 1 </MatchFromCounter> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <from> fromparmatchstdform(for(C:Chan <= D:Chan){P:HigherProc})
                  => ["persistentlisten"][false][for(C <= D){P}][ -1 ][ I ]["inactive"]{ListItem(-1)}  </from>
                  <MatchFromCounter> I => I +Int 1 </MatchFromCounter> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <from> fromparmatchstdform(contract D:Chan(C:Chan) = { P:HigherProc })
                  => ["persistentlisten"][false][for(C <= D){P}][ -1 ][ I ]["inactive"]{ListItem(-1)}  </from>
                  <MatchFromCounter> I => I +Int 1 </MatchFromCounter> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <from> fromparmatchstdform(new X:Ids in { P:HigherProc })
                  => ["new"][false][new X in { P }][ -1 ][ I ]["inactive"]{ListItem(-1)}  </from>
                  <MatchFromCounter> I => I +Int 1 </MatchFromCounter> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <from> fromparmatchstdform(C:Chan!(P:HigherProc))
                  => ["send"][false][C!(P)][ -1 ][ I ]["inactive"]{ListItem(-1)}  </from>
                  <MatchFromCounter> I => I +Int 1 </MatchFromCounter> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <from> fromparmatchstdform(C:Chan!!(P:HigherProc))
                  => ["persistentsend"][false][C!!(P)][ -1 ][ I ]["inactive"]{ListItem(-1)}  </from>
                  <MatchFromCounter> I => I +Int 1 </MatchFromCounter> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <from> fromparmatchstdform(match P:HigherProcOrChan { H:HigherMatchCases })
                  => ["match"][false][match P { H }][ -1 ][ I ]["inactive"]{ListItem(-1)}  </from>
                  <MatchFromCounter> I => I +Int 1 </MatchFromCounter> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <from> fromparmatchstdform( Nil )
                  => ["Nil"][false][ Nil ][ -1 ][ I ]["inactive"]{ListItem(-1)}  </from>
                  <MatchFromCounter> I => I +Int 1 </MatchFromCounter> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <from> fromparmatchstdform(E:Exp)
                  => ["Exp"][false][E][ -1 ][ I ]["inactive"]{ListItem(-1)}  </from>
                  <MatchFromCounter> I => I +Int 1 </MatchFromCounter> ... </ParMatch> ... </thread>

// Then the "to" cells
// The standard form is [ process sort ][ process ][ id number ][ taken/available ]
// During matching, only the taken/available gets updated
rule <thread> ... <ParMatch> ... <to> toparmatchstdform(P1:HigherProc | P2:HigherProc)
                  => ["par"][true][P1 | P2][ -1 ]["available"]  </to> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <to> toparmatchstdform(for(C:Chan <- D:Chan){P:HigherProc})
                  => ["listen"][false][for(C <- D){P}][ I ]["available"]  </to>
                  <MatchToCounter> I => I +Int 1 </MatchToCounter>
                  <ListofAllToCells> ... .List => ListItem(I) ... </ListofAllToCells> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <to> toparmatchstdform(for(C:Chan <= D:Chan){P:HigherProc})
                  => ["persistentlisten"][false][for(C <= D){P}][ I ]["available"]  </to>
                  <MatchToCounter> I => I +Int 1 </MatchToCounter>
                  <ListofAllToCells> ... .List => ListItem(I) ... </ListofAllToCells> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <to> toparmatchstdform(contract D:Chan(C:Chan) = { P:HigherProc })
                  => ["persistentlisten"][false][for(C <= D){P}][ I ]["available"]  </to>
                  <MatchToCounter> I => I +Int 1 </MatchToCounter>
                  <ListofAllToCells> ... .List => ListItem(I) ... </ListofAllToCells> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <to> toparmatchstdform(new X:Id in { P:HigherProc })
                  => ["new"][false][new X in { P }][ I ]["available"]  </to>
                  <MatchToCounter> I => I +Int 1 </MatchToCounter>
                  <ListofAllToCells> ... .List => ListItem(I) ... </ListofAllToCells> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <to> toparmatchstdform(C:Chan!(P:HigherProc))
                  => ["send"][false][C!(P)][ I ]["available"]  </to>
                  <MatchToCounter> I => I +Int 1 </MatchToCounter>
                  <ListofAllToCells> ... .List => ListItem(I) ... </ListofAllToCells> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <to> toparmatchstdform(C:Chan!(P:HigherProc))
                  => ["persistentsend"][false][C!!(P)][ I ]["available"]  </to>
                  <MatchToCounter> I => I +Int 1 </MatchToCounter>
                  <ListofAllToCells> ... .List => ListItem(I) ... </ListofAllToCells> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <to> toparmatchstdform(match P:HigherProcOrChan { H:HigherMatchCases })
                  => ["match"][false][ match P { H } ][ I ]["available"]  </to>
                  <MatchToCounter> I => I +Int 1 </MatchToCounter>
                  <ListofAllToCells> ... .List => ListItem(I) ... </ListofAllToCells> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <to> toparmatchstdform( Nil )
                  => ["Nil"][false][ Nil ][ I ]["available"]  </to>
                  <MatchToCounter> I => I +Int 1 </MatchToCounter>
                  <ListofAllToCells> ... .List => ListItem(I) ... </ListofAllToCells> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <to> toparmatchstdform(E:Exp)
                  => ["Exp"][false][E][ I ]["available"]  </to>
                  <MatchToCounter> I => I +Int 1 </MatchToCounter>
                  <ListofAllToCells> ... .List => ListItem(I) ... </ListofAllToCells> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <to> toparmatchstdform(X:Id)
                  => ["Id"][false][X][ I ]["variable"]  </to>
                  <MatchToCounter> I </MatchToCounter>
                  <ListofAllToCells> ... .List => ListItem("var") ... </ListofAllToCells> ... </ParMatch> ... </thread>


// Delete empty "to" and "from" cells
rule <thread> ... <ParMatch> ... <to> .K </to> => . ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <from> .K </from> => . ... </ParMatch> ... </thread>

// Now to break up the "par"s
// first the "from"
rule <thread> ... <ParMatch> ... <from> ["par"][true][P1:HigherProc | P2:HigherProc][ -1 ][ -1 ]["inactive"]{ListItem(-1)}
                  => fromparmatchstdform(P1) </from>
                  (.Bag => <from> fromparmatchstdform(P2) </from> ) ... </ParMatch> ... </thread>

// now the "to"
rule <thread> ... <ParMatch> ... <to> ["par"][true][P1:HigherProc | P2:HigherProc][ -1 ]["available"]
                  => toparmatchstdform(P1) </to>
                  (.Bag => <to> toparmatchstdform(P2) </to> ) ... </ParMatch> ... </thread>

// Now that the pars are separated, we can start systematically trying to find a match.
// Note: because of possible nondeterministic behavior, we have to make sure we don't
// accidentally match before all the "par"s are done separating. This is taken care of by the
// tags on each std form statement: true of it's still a "par", false otherwise. We only proceed if
// it's false.

// Let the par matching games begin
// We change this "from" cell's id to -1 so that we don't start the game over and over again
rule <thread> ... <ParMatch> ... <from> [S:String][false][ P:HigherProc ][ -1 ][ (0 => -1) ][ ("inactive" => "active") ]{ (_ => L) } </from>
                                 <ListofAllToCells> L:List </ListofAllToCells> ... </ParMatch> ... </thread>

// General step (for matching with non-variables)
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ (-1 => I) ][ J:Int ][ "active" ]{ ListHead:List (ListItem(I) => .) ListTail:List } </from>
                                 <to> [S2:String][false][P2:HigherProc][I:Int]["available"] </to>
                                 <id> ID:Int </id> ... </ParMatch>
                  // ERROR!! YOU NEED TO MAKE A "TFSTDMATCH" FOR EACH KIND OF MATCHING YOU WANT!!!
                  <k> #(PARPAUSE)[S:String,ID] => match["TF" +String S]( P1 ; P2 ) ~> #(PARPAUSE)[S,ID] ... </k> ... </thread>

// You can't try to match with a process that's already taken
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ J1:Int ][ J2:Int ][ "active" ]{ ListHead:List (ListItem(I) => .) ListTail:List } </from>
                                 <to> [S2:String][false][P2:HigherProcNoVars][I]["taken"] </to> ... </ParMatch> ... </thread>

// There's been a match!
// We change the current from cell from active to inactive, reserve the to cell it matched with, and move up to the next from cell
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ I ][ J:Int ][ ("active" => "inactive") ]{ L1 } </from>
                                 <to> [S2:String][false][P2:HigherProc][I][("available" => "taken")] </to>
                                 <id> ID </id>
                                 // We have just taken one of the "to" cells, so we keep track of that in the counter
                                 <MatchToCounter> I2:Int => I2 +Int -1 </MatchToCounter>
                                 // We grab the next "from" cell up
                                 <from> [S3:String][false][ P3:HigherProc ][ M:Int ][ J2:Int ][ ("inactive" => "active") ]{ _ => L2 } </from>
                                 <ListofAllToCells> L2:List </ListofAllToCells> ... </ParMatch>
                  // There's been a match because there is nothing before the #(PARPAUSE) in the k-cell
                  // This means that the "match["TFSTDMATCH"]" function was successful, and left nothing.
                  <k> #(PARPAUSE)[S:String,ID:Int] ... </k> ... </thread>
                  requires (J2 =Int J +Int 1)

// We have to do the same rule, but in the case we matched on level 0 (since we changed 0 to -1)
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ I ][ -1 ][ ("active" => "inactive") ]{ L1 } </from>
                                 <to> [S2:String][false][P2:HigherProc][I][("available" => "taken")] </to>
                                 <id> ID </id>
                                 // We have just taken one of the "to" cells, so we keep track of that in the counter
                                 <MatchToCounter> I2:Int => I2 +Int -1 </MatchToCounter>
                                 // We grab the next "from" cell up
                                 <from> [S3:String][false][ P3:HigherProc ][ M:Int ][ 1 ][ ("inactive" => "active") ]{ _ => L2 } </from>
                                 <ListofAllToCells> L2:List </ListofAllToCells> ... </ParMatch>
                  // There's been a match because there is nothing before the #(PARPAUSE) in the k-cell
                  // This means that the "match["TFSTDMATCH"]" function was successful, and left nothing.
                  <k> #(PARPAUSE)[S:String,ID:Int] ... </k> ... </thread>

// Finally, we do the same rule, but in the case that we've matched on the last "from" cell
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ I ][ J1:Int ][ ("active" => "done") ]{ L1 } </from>
                                 <to> [S2:String][false][P2:HigherProc][I][("available" => "taken")] </to>
                                 <id> ID </id>
                                 // We have just taken one of the "to" cells, so we keep track of that in the counter
                                 <MatchToCounter> I2:Int => I2 +Int -1 </MatchToCounter>
                                 // We're at the top
                                 <MatchFromCounter> J2:Int </MatchFromCounter> ... </ParMatch>
                  // There's been a match because there is nothing before the #(PARPAUSE) in the k-cell
                  // This means that the "match["TFSTDMATCH"]" function was successful, and left nothing.
                  <k> #(PARPAUSE)[S:String,ID:Int] ... </k> ... </thread>
                  requires (J2 =Int J1 +Int 1)

// There's been a match! (with a variable)
// General step for binding to variables
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ (-1 => "var") ][ J:Int ][ ("active" => "inactive") ]{ ListHead:List (ListItem("var") => .) ListTail:List } </from>
                    <from> [S2:String][false][ P2:HigherProc ][ I2:Int ][ J2:Int ][ ("inactive" => "active") ]{ _ => L2:List } </from>
                    <ListofAllToCells> L2:List </ListofAllToCells>
                  ... </ParMatch> ... </thread>
                  requires (J2 =Int J +Int 1)

// Same step, but if we are at the 0 level
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ (-1 => "var") ][ -1 ][ ("active" => "inactive") ]{ ListHead:List (ListItem("var") => .) ListTail:List } </from>
                    <from> [S2:String][false][ P2:HigherProc ][ I2:Int ][ 1 ][ ("inactive" => "active") ]{ _ => L2:List } </from>
                    <ListofAllToCells> L2:List </ListofAllToCells>
                  ... </ParMatch> ... </thread>

// Same step, but if we're at the last "from" cell
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ (-1 => "var") ][ J:Int ][ ("active" => "done") ]{ ListHead:List (ListItem("var") => .) ListTail:List } </from>
                    <MatchFromCounter> J2:Int </MatchFromCounter>
                  ... </ParMatch> ... </thread>
                  requires (J2 =Int J +Int 1)

// The match failed, so we try another one (we redo the general step)
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ (I => -1) ][ J:Int ][ "active" ]{ L1 } </from>
                                 <to> [S2:String][false][P2:HigherProc][I:Int]["available"] </to>
                                 <id> ID:Int </id> ... </ParMatch>
                  <k> ... (#(MATCHFAIL)[ TF:String ] ~> M:K => .) ~> #(PARPAUSE)[S:String,ID] ... </k> ... </thread>
                  <ParMatchCounter> ID </ParMatchCounter>
                  // WATCH OUT FOR ME. This could cause future problems
                  requires (TF ==String "TFSTDMATCH") orBool (TF ==String "TFCHANMATCH")


// There are no more options for possible matches (go back down)
// This is for a non-variable
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ -1 ][ J:Int ][ ("active" => "inactive") ]{ .List } </from>
                                 // Go down one; you can't use what you had before
                                 <from> [R1:String][false][ Q1:HigherProc ][ (I2 => -1) ][ J2:Int ][ ("inactive" => "active") ]{ L } </from>
                                 <to> [R2:String][false][Q2:HigherProc][I2:Int][("taken" => "available")] </to>
                                 // We have to return one of the "to" cells to move forward
                                 <MatchToCounter> C => C +Int 1 </MatchToCounter> ... </ParMatch> ... </thread>
                                 requires (J2 =Int J +Int -1)

// We have to do the same rule, but in the case that we were on the first level and we have to go back down to 0
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ -1 ][ 1 ][ ("active" => "inactive") ]{ .List } </from>
                                 // Go down one; you can't use what you had before
                                 <from> [R1:String][false][ Q1:HigherProc ][ (I2 => -1) ][ -1 ][ ("inactive" => "active") ]{ L } </from>
                                 <to> [R2:String][false][Q2:HigherProc][I2:Int][("taken" => "available")] </to>
                                 // We have to return one of the "to" cells to move forward
                                 <MatchToCounter> C => C +Int 1 </MatchToCounter> ... </ParMatch> ... </thread>

// Going back down where the thing below matched to a variable (essential duplicate of rules above)
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ (I1 => -1) ][ J:Int ][ ("active" => "inactive") ]{ .List } </from>
                                 // Go down one; you can't use what you had before
                                 <from> [R1:String][false][ Q1:HigherProc ][ ("var" => -1) ][ J2:Int ][ ("inactive" => "active") ]{ L } </from>
                                 ... </ParMatch> ... </thread>
                                 requires (J2 =Int J +Int -1)

rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ (I1 => -1) ][ 1 ][ ("active" => "inactive") ]{ .List } </from>
                                 // Go down one; you can't use what you had before
                                 <from> [R1:String][false][ Q1:HigherProc ][ ("var" => -1) ][ -1 ][ ("inactive" => "active") ]{ L } </from>
                                 ... </ParMatch> ... </thread>

// There are no more options on the 0 level, so the match fails and we rewrite #(PARPAUSE) as #(MATCHFAIL)
// (a) We first write these rules for nested ParMatches.
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ -1 ][ -2 ][ "active" ]{ .List } => #(MATCHFAIL)[ "TFSTDMATCH" ] </from>
                    <id> ID:Int </id> ... </ParMatch>
                   <k> #(PARPAUSE)[S:String,ID] => #(MATCHFAIL)["TF" +String S] ... </k> ... </thread>
                   <ParMatchCounter> ID => ID +Int -1 </ParMatchCounter>
                   requires ID >Int 1

rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ -1 ][ -1 ][ "active" ]{ .List } => #(MATCHFAIL)[ "TFSTDMATCH" ] </from>
                    <id> ID:Int </id> ... </ParMatch>
                   <k> #(PARPAUSE)[S:String,ID] => #(MATCHFAIL)["TF" +String S] ... </k> ... </thread>
                   <ParMatchCounter> ID => ID +Int -1 </ParMatchCounter>
                   requires ID >Int 1

// Cleanup
rule <thread> ... <ParMatch> ... <from> #(MATCHFAIL)[ "TFSTDMATCH" ] </from> <id> ID </id> ... </ParMatch> => . ... </thread>
     requires ID >Int 1

// (b) Now the rules for the case that we're at the base ParMatch. The difference is that we throw an
//     error matching the original request, instead of a "TFSTDMATCH" matchfail.
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ -1 ][ -2 ][ "active" ]{ .List } => #(MATCHFAIL)[ "TFSTDMATCH" ] </from>
                    <id> ID:Int </id> ... </ParMatch>
                   <k> #(PARPAUSE)[S:String,ID] => #(MATCHFAIL)[S] ... </k> ... </thread>
                   <ParMatchCounter> ID => ID +Int -1 </ParMatchCounter>
                   requires ID =Int 1

rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ -1 ][ -1 ][ "active" ]{ .List } => #(MATCHFAIL)[ "TFSTDMATCH" ] </from>
                    <id> ID:Int </id> ... </ParMatch>
                   <k> #(PARPAUSE)[S:String,ID] => #(MATCHFAIL)[S] ... </k> ... </thread>
                   <ParMatchCounter> ID => ID +Int -1 </ParMatchCounter>
                   requires ID =Int 1

// Cleanup
rule <thread> ... <ParMatch> ... <from> #(MATCHFAIL)[ "TFSTDMATCH" ] </from> <id> ID </id> ... </ParMatch> => . ... </thread>
     requires ID =Int 1


// We've gotten to the end but we need to make sure there
// aren't any "to" cells which have been neglected.
// This can happen by matching too many processes to variables
// If this happens, we have to go back down and try again,
// potentially failing the whole match.

// First we see if the MatchToCounter is 0. If it is, that means we
// didn't neglect any "to" cells. If not, then we need to try again
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ E:Exp ][ J:Int ][ "done" ]{ L1:List } </from>
                    <MatchFromCounter> J1:Int </MatchFromCounter>
                    <MatchToCounter> J2:Int </MatchToCounter>
                    <Matched> .K => (J2 =Int 0) </Matched>
                    <id> ID:Int </id>
                  ... </ParMatch> ... </thread>
                  <ParMatchCounter> ID </ParMatchCounter>
                  requires (J +Int 1 =Int J1)

// If there are leftover "to" cells, we need to go back down
// If the last thing we matched to was a variable, then we just go back down and try again
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ ("var" => -1) ][ J:Int ][ ("done" => "active") ]{ L1:List } </from>
                    <MatchFromCounter> J2:Int </MatchFromCounter>
                    <Matched> false => . </Matched>
                    <id> ID:Int </id>
                  ... </ParMatch> ... </thread>
                  <ParMatchCounter> ID </ParMatchCounter>
                  requires (J2 =Int J +Int 1)

// If the last thing we matched to was not a variable, we need to go back down
// and update the thing we were matched to to say "available"
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ (I:Int => -1) ][ J:Int ][ ("done" => "active") ]{ L1:List } </from>
                    <to> [S2:String][false][P2:HigherProc][I][("taken" => "available")] </to>
                    <MatchFromCounter> J2:Int </MatchFromCounter>
                    // We need to put back the "to" cell
                    <MatchToCounter> I2:Int => I2 +Int -1 </MatchToCounter>
                    <Matched> false => . </Matched>
                  ... </ParMatch> ... </thread>
                  requires (J2 =Int J +Int 1)

// We've matched everything, this match as a whole is a success!
// (We may have extra variables left over, which we bind to "Nil")
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ I ][ J:Int ][ ("done" => "inactive") ]{ L1 } </from>
                                 // So we matched
                                 <Matched> true </Matched>
                                 // Work on the furthest ParMatch invoked
                                 <id> ID </id> ... </ParMatch>
                   <k> #(PARPAUSE)[S:String,ID:Int] ... </k> ... </thread>
                   <ParMatchCounter> ID </ParMatchCounter>

// What happens when "Matched" is true
rule <thread> ... <ParMatch> ...
                            <from> [S1:String][false][ P1:HigherProc ][ I ][ J:Int ][ "inactive" ]{ L1 } => . </from>
                            <to> [S2:String][false][P2:HigherProc][I:Int]["taken"] => . </to>
                            <Matched> true </Matched>
                            <MatchToCounter> I1 => I1 +Int 1 </MatchToCounter>
                            <MatchFromCounter> I2 => I2 +Int -1 </MatchFromCounter>
                            <id> ID </id>
                            ... </ParMatch>
                  <k> #(PARPAUSE)[S:String,ID:Int] => #(PARPAUSE)[S,ID] ~> match[S](P1 ; P2) ... </k>  ... </thread>
                  <ParMatchCounter> ID </ParMatchCounter>

rule <thread> ... <ParMatch> ...
                    <to> [S:String][false][P:HigherProc][I]["variable"]
                     =>  [S:String][false][P:HigherProc][I]["variable"]{ Nil } </to>
                    <Matched> true </Matched>
                    <id> ID:Int </id> ... </ParMatch> ... </thread>
                    <ParMatchCounter> ID </ParMatchCounter>

// Now we (nondetermistically) assign processes which are to be bound to vars
// by grouping them.
rule <thread> ... <ParMatch> ...
                            <from> [S1:String][false][ P1:HigherProc ]["var"][ J:Int ][ "inactive" ]{ L1 } => . </from>
                            <to> [S2:String][false][P2:HigherProc][I]["variable"]{ (P:HigherProc => P | P1) } </to>
                            <Matched> true </Matched>
                            <MatchToCounter> I1 => I1 +Int 1 </MatchToCounter>
                            <MatchFromCounter> I2 => I2 +Int -1 </MatchFromCounter>
                            <id> ID:Int </id>
                            ... </ParMatch>  ... </thread>
                            <ParMatchCounter> ID </ParMatchCounter>

// Once that is done, we send all the vars to be matched; if they got no processes
// they get the "Nil" process
rule <thread> ... <ParMatch> ...
                            <to> [S1:String][false][P1:Id][I]["variable"]{ P2:HigherProc } => . </to>
                            <Matched> true </Matched>
                            <MatchFromCounter> 0 </MatchFromCounter>
                            <id> ID </id>
                            ... </ParMatch>
                  <k> #(PARPAUSE)[S:String,ID:Int] => #(PARPAUSE)[S,ID] ~> match["STDMATCH"](P2 ; P1) ... </k>  ... </thread>
                  <ParMatchCounter> ID </ParMatchCounter>

// Once this has exhausted, clear the contents of the the "ParMatch" cell,
// unpause the calculation and move forward
rule <thread> ... <ParMatch>
                      <id> ID:Int => . </id>
                      <MatchFromCounter> I2:Int => . </MatchFromCounter>
                      <MatchToCounter> I1:Int => . </MatchToCounter>
                      <ListofAllToCells> L1:List => . </ListofAllToCells>
                      <Wildcards> B2:Bool => . </Wildcards>
                      <Matched> true => . </Matched>
                      <BindToVars> Nil => . </BindToVars>
                  </ParMatch>
                  // Continue forward on the calculation! Hooray!!
                  <k> #(PARPAUSE)[S:String,ID] => . ... </k>
                  ... </thread>
                  <ParMatchCounter> ID => ID +Int -1 </ParMatchCounter>


// Empty cells get deleted
rule <thread> ... <ParMatch> ... <MatchFromCounter> .K </MatchFromCounter> => . ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <MatchToCounter> .K </MatchToCounter> => . ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <ListofAllToCells> .List </ListofAllToCells> => . ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <Wildcards> .K </Wildcards> => . ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <BindToVars> .K </BindToVars> => . ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <id> .K </id> => . ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> <Matched> .K </Matched> </ParMatch> => . ... </thread>


// Configuration
configuration <T color="yellow">
                <thread multiplicity="*" color="purple">
                  <k color="red"> $PGM:Proc </k>
                  <in multiplicity="*" color="blue"> .K </in>
                  <out multiplicity="*" color="yellow"> .K </out>
                  <InData multiplicity="*"> .K </InData>
                  <OutData multiplicity="*"> .K </OutData>
                  <subst multiplicity="*" color="orange"> .K </subst>
                  // to match patterns with parallel processes in them
                  <ParMatch multiplicity="*">
                    <id> .K </id>
                    <to multiplicity="*"> .K </to>
                    <from multiplicity="*"> .K </from>
                    <MatchFromCounter> 0 </MatchFromCounter>
                    <MatchToCounter> 0 </MatchToCounter>
                    <ListofAllToCells> .List </ListofAllToCells>
                    <Wildcards> false </Wildcards>
                    <BindToVars> Nil </BindToVars>
                    <Matched> .K </Matched>
                  </ParMatch>
                  // to check patterns with a logical "or" in them
                  <OrCheck multiplicity="*"> .K </OrCheck>
                </thread>
                <GlobalListofOutIds color="orange"> .List </GlobalListofOutIds>
                <GlobalListofInIds color="purple"> .List </GlobalListofInIds>
                <NewIntGenerator color="purple"> 0 </NewIntGenerator>
                <ParMatchCounter> 0 </ParMatchCounter>
              </T>


endmodule
