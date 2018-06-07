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
              | "for" "(" Names "<-" Name ")" "{" Proc "}" [binder]
              // Persistent listen
              | "for" "(" Names "<=" Name ")" "{" Proc "}" [binder]
              | "contract" Name "(" Name ")" "=" "{" Proc "}" [binder]
              // Send
              | Name "!(" Procs ")"
              // Evaluate
              | "*" Name
              // New
              | "new" Ids "in" "{" Proc "}" [binder]
              // Parallel
              | Proc "|" Proc [left]

// We put "Nil" and Exp in the same syntactic category, since they are
// structurally equivalent as ground properties
syntax Ground ::= "Nil"
              | Exp

// We need names with no variables
// (We need put the Proc through some algorithm which puts it into its normal form; will come later)
syntax NameNoVars ::= "@" Proc

// The general idea of a name can be a (bound) variable
syntax Name ::= NameNoVars
              | Id

// Names and Processes, used specifically for sending and listening for tuples
syntax Names ::= List{ Name, "," }
syntax Procs ::= List{ Proc, "," }
syntax Ids   ::= List{Id, ","}

// A list for matching parallel processes
syntax ParMatchList ::= List{ HigherProc, ";"}

// Syntax that will show up b/c of the new construct,
// but that cannot be written (unforgeable things)

// For matching
syntax HigherProcOrChan ::= Id
              | HigherProcOrChanNoVars
              | Chan
              | HigherProc

// For matching, we need to make a distinction between variables and no variables
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
                "for(" Chans "<-" Chan "){" HigherProc "}" [binder]
              | "for(" Chans "<=" Chan "){" HigherProc "}" [binder]
              | "contract" Chan "(" Chan ")" "=" "{" HigherProc "}" [binder] // binder? How does this work?
              | Chan "!(" HigherProcs ")"
              | "*" Chan
              | "new" Ids "in" "{" HigherProc "}" [binder]
              | ProcNoVars
              // Syntax for new channels—the whole reason for "Higher" anything
              | "#(" Int ")"
              | HigherProc "|" HigherProc [left] // should this be changed to a different category? Procs?
              | "{" HigherProc "}" [bracket]

// The general idea of a channel can be a (bound) variable
syntax Chan ::= ChanNoVars
              | Id

// We need channels, with no variables
syntax ChanNoVars ::= "@" HigherProc
              | NameNoVars

syntax Exp  ::= Int
              | Bool
              | String

syntax KVariable ::= Id
syntax KResult ::= Int
              | String StringlessMatchFormSyntax
              | Id
              | Bool

// Syntax which is needed for various matching operations below
syntax CanBeStdMatched ::= Id
              | "intostdmatchform(" HigherProcOrChan ")"
              | "intostdtfmatchform(" HigherProcOrChan ")"
              | Ground

syntax Fun  ::= "match" "(" HigherProcOrChan ";" HigherProcOrChan ")"
              | "listmatch" "(" HigherProcs ";" Chans ")"
              | "DoTheseMatch" "(" HigherProcOrChan ";" HigherProcOrChan ")"
              | "matchstdform" "(" CanBeStdMatched ";" CanBeStdMatched ")" [strict]
              | "tfmatchstdform" "(" CanBeStdMatched ";" CanBeStdMatched ")" [strict]
              | "matchstdform" "(" CanBeStdMatched ";" CanBeStdMatched ";" Bool ")" [strict(3)]
              | "tfmatchstdform" "(" CanBeStdMatched ";" CanBeStdMatched ";" Bool ")" [strict(3)]
              | "toparmatchstdform" "(" HigherProcOrChan ")"
              | "fromparmatchstdform" "(" HigherProcOrChan ")"
              | "#(MATCHFAIL)"
              | "#(TFMATCHFAIL)"
              | "#(PARPAUSE)"

syntax StringlessMatchFormSyntax ::=
              // Quote, Eval and Var
                "[" HigherProcOrChan "]"
              // Listen and Persistent Listen, as well as Contracts
              | "[" Chan "][" Chan "][" HigherProc "]"
              // New
              | "[" Ids "][" HigherProc "]"
              // Send
              | "[" Chan "][" HigherProc "]"
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

// When the list runs out

// Syntactic sugar for contracs as persistent sends/receives listening only on one channel.
rule <k> contract D:Chan(C:Chan) = { P:HigherProc } => for(C <= D){ P } </k>

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
        (.Bag => <k> listmatch( P ; D ) </k>)
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
       (.Bag => <k> listmatch( P ; D ) </k>)
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
        <k> #(MATCHFAIL) ~> _ => . </k>
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
// PERSISTENT SENDING (AND RECEIVING)
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //

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
       (.Bag => <k> listmatch( P ; D ) </k>)
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
       (.Bag => <k> listmatch( P ; D ) </k>)
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
        <out> O </out>
        <OutData> S:Int ; L:List </OutData>
        // The trigger
        <k> #(MATCHFAIL) ~> _ => . </k>
        // Check to make sure the receive was persistent
        <in> "persistent" => . </in>
        <subst> B => . </subst>
    </thread>


// When we've finished matching, we're ready to execute Q after having replaced all the variables
// Note that we don't delete the "in" cell.
rule <thread>
        <in> "persistent" => . </in>
        <out> O => . </out>
        <OutData> I:Int ; L:List => . </OutData>
        <subst> Q:HigherProc  => . </subst>
     </thread>
     (.Bag => <thread> <k> Q </k> </thread>)
     <GlobalListofOutIds> ... ListItem(I) => . ... </GlobalListofOutIds>

// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //
// THE DEFINITION OF THE "match" ACTION
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //

// Take the tuples one at a time
rule <thread> ... <k> ... listmatch( P:HigherProc, Q:HigherProcs ; D:Chan, E:Chans )
                      => listmatch( Q ; E ) ~> match( @P ; D )  ... </k> ... </thread>

// When they're both done, get rid of the listmatch bit and get going on matching
rule <thread> ... <k> ... listmatch( .HigherProcs ; .Chans )
                      => . ... </k> ... </thread>

// They fail if the tuples are not the same length
rule <thread> ... <k> ... listmatch( .HigherProcs ; D:Chan, E:Chans )
                      => #(MATCHFAIL) ... </k> ... </thread>
rule <thread> ... <k> ... listmatch( P:HigherProc, Q:HigherProcs ; .Chans )
                      => #(MATCHFAIL) ... </k> ... </thread>

// (a) Give the rules for the match action which receives two arguments
rule <thread> ... <k> ... match( P:HigherProcOrChan ; D:HigherProcOrChanNoVars )
              => matchstdform( intostdmatchform(P) ; intostdmatchform(D)) ... </k> ... </thread>


// If a variable matches with something, check if that thing has free variables
// If it does, then rewrite as #(MATCHFAIL). If it does not, then substitute S for X
// in the original statement. Checking for free variables will come soon.
// For now, we just substitute. The code we would use for matching is below, once
// semantics for the "freevars( )" function is done.
rule <thread> ... <k> ... match( P:HigherProcOrChan ; X:Id )
              => . ... </k>
              <subst> Q => Q[ P / X] </subst> ... </thread>

//rule <thread> ... <k> ... match(S:Stmt, X:Id) => freevars(S,X) ... </k> ... </thread>



// (b) Put everything into a standard form that's easy to work with
rule intostdmatchform(@P:HigherProc) => "quote"[ P ]
rule intostdmatchform(*C:Chan) => "eval"[ C ]
rule intostdmatchform(for(C:Chan <- D:Chan){P:HigherProc}) => "listen"[C][D][P]
rule intostdmatchform(for(C:Chan <= D:Chan){P:HigherProc}) => "persistentlisten"[C][D][P]
rule intostdmatchform(contract D:Chan(C:Chan) = { P:HigherProc }) => "persistentlisten"[C][D][P]
rule intostdmatchform(new X:Ids in { P:HigherProc }) => "new"[X][P]
rule intostdmatchform(C:Chan!(P:HigherProc)) => "send"[C][P]
rule intostdmatchform( Nil ) => "Nil"[ ]
//rule intostdmatchform(E:Exp) => E
rule intostdmatchform(S:String) => "string"[S]
rule intostdmatchform(B:Bool) => "bool"[B]
rule intostdmatchform(I:Int) => "int"[I]
rule intostdmatchform( #(I:Int) ) => "hash"[I]
rule intostdmatchform( P1:HigherProc | P2:HigherProc ) => "par"[P1][P2]
// This should not be necessary; will be removed later,
// when channel equality can be checked.
rule intostdmatchform( X:Id ) => "var"[X]


// "matchstdform" goes through a couple steps; it will take two arguments, and then check to see
// if they are of the same syntactic sort. If they are not, we fail; if they are, we keep going.
// This is a recursive check for matching.
rule <thread> ... <k> ... matchstdform( S1:String P1:StringlessMatchFormSyntax ; S2:String P2:StringlessMatchFormSyntax )
              => matchstdform( S1 P1 ; S2 P2 ; S1 ==String S2 ) ... </k> ... </thread>

// To account for expressions
// Strings
rule <thread> ... <k> ... matchstdform( "string"[S1:String] ; "string"[S2:String] ; true )
                    => matchstdform( S1:String ; S2:String ; S1 ==String S2) ... </k> ... </thread>
rule <thread> ... <k> ... matchstdform( S1:String ; S2:String ; true ) => . ... </k> ... </thread>
rule <thread> ... <k> ... matchstdform( S1:String ; S2:String ; false ) => #(MATCHFAIL) ... </k> ... </thread>

// Ints
rule <thread> ... <k> ... matchstdform( "int"[I1:Int] ; "int"[I2:Int] ; true )
                    => matchstdform( I1:Int ; I2:Int ; I1 ==Int I2) ... </k> ... </thread>
rule <thread> ... <k> ... matchstdform( I1:Int ; I2:Int ; true ) => . ... </k> ... </thread>
rule <thread> ... <k> ... matchstdform( I1:Int ; I2:Int ; false ) => #(MATCHFAIL) ... </k> ... </thread>

// Bools
rule <thread> ... <k> ... matchstdform( "bool"[B1:Bool] ; "bool"[B2:Bool] ; true )
                    => matchstdform( B1:Bool ; B2:Bool ; B1 ==Bool B2) ... </k> ... </thread>
rule <thread> ... <k> ... matchstdform( B1:Bool ; B2:Bool ; true ) => . ... </k> ... </thread>
rule <thread> ... <k> ... matchstdform( B1:Bool ; B2:Bool ; false ) => #(MATCHFAIL) ... </k> ... </thread>

// As more expressions become part of rholang this will have to be updated



// Terminate if the strings do not match (i.e., the two processes are not of the same syntactic sort)
rule <thread> ... <k> ... matchstdform(S1:String P1:StringlessMatchFormSyntax ; S2:String P2:StringlessMatchFormSyntax ; false)
              => #(MATCHFAIL)  ... </k> ... </thread>

rule <thread> ... <k> ... matchstdform( E1:Exp ; E2:Exp ; false)
              => #(MATCHFAIL)  ... </k> ... </thread>

// If the patterns are ever the same, as expressions or processes, we're good.
rule <thread> ... <k> ... matchstdform( S1:String P1:StringlessMatchFormSyntax ; S1:String P1:StringlessMatchFormSyntax ; true ) => . ... </k> ... </thread>
rule <thread> ... <k> ... matchstdform(  E:Exp ; E:Exp ; true ) => . ... </k> ... </thread>

// When the pattern is a quote "@"
rule <thread> ... <k> ... matchstdform( "quote"[P:HigherProc] ; "quote"[R:HigherProc] ; true)
          => match( P ; R ) ... </k> ... </thread>

// When the pattern is an eval "*"
rule <thread> ... <k> ... matchstdform( "eval"[C:Chan] ; "eval"[D:Chan] ; true)
          => match( C ; D ) ... </k> ... </thread>

// When the pattern is a "for"
// This is not quite correct, but it's on its way. It still needs alpha equivalence, among other things.
// If you responsibly code, it will still match most things correctly. In a future update it will match
// exactly the correct terms, while failing to match anything else.
// One of the things we will need is a function that determines if there are free variables in a term.
// We will use De Brujin indices for this.
rule <thread> ... <k> ... matchstdform("listen"[C:Chan][D:Chan][P:HigherProc] ; "listen"[E:Chan][F:Chan][R:HigherProc] ; true )
          => match(C ; E) ~> match(D ; F) ~> match(P ; R) ... </k> ... </thread>

// The (logic of the) code we would use for a more correct version
/*
rule <thread> ... <k> ... matchstdform("listen"[C:Chan][D:Chan][P:HigherProc],"listen"[E:Chan][F:Chan][R:HigherProc], true )
          => matchstdform("listen"[D][P],"listen"[F][R],D ==Chan F) ... </k> ... </thread>

rule <thread> ... <k> ... matchstdform("listen"[C:Chan][P:HigherProc],"listen"[D:Chan][R:HigherProc], true )
          => match( C, D ) ~> match( P, R ) ... </k> ... </thread>
*/

// When the pattern is a persistent "for"
// *** This is also incorrect; we need things like in the previous listen construct
rule <thread> ... <k> ... matchstdform("persistentlisten"[C:Chan][D:Chan][P:HigherProc] ; "persistentlisten"[E:Chan][F:Chan][R:HigherProc] ; true )
          => match(C ; E) ~> match(D ; F) ~> match(P ; R) ... </k> ... </thread>

// The (logic of the) code we would use for a more correct version
/*
// When the pattern is a persistent "for"
rule <thread> ... <k> ... match("persistentlisten"(C:Chan)(D:Chan)(P:HigherProc),"persistentlisten"(E:Chan)(F:Chan)(R:HigherProc), true )
          => match("persistentlisten"(D)(P),"persistentlisten"(F)(R),C == E) ... </k> ... </thread>

rule <thread> ... <k> ... match("persistentlisten"(D:Chan)(P:HigherProc),"persistentlisten"(F:Chan)(R:HigherProc),true)
          => match(intostdmatchform(D),intostdmatchform(F)) ~> match(intostdmatchform(P),intostdmatchform(R)) ... </k> ... </thread>
*/

// When the pattern is "send"
rule <thread> ... <k> ... matchstdform( "send"[C:Chan][P:HigherProc] ; "send"[D:Chan][R:HigherProc] ; true)
          => match( C ; D ) ~> match( P ; R ) ... </k> ... </thread>

// When the pattern is "hash"
rule <thread> ... <k> ... matchstdform( "hash"[I:Int] ; "hash"[J:Int] ; true)
          => match( I ; J ) ... </k> ... </thread>

// When the pattern is a "new"
rule <thread> ... <k> ... matchstdform("new"[X1:Id,L1:Ids][P:HigherProc] ; "new"[X2:Id,L2:Ids][R:HigherProc] ; true)
          => matchstdform("new"[L1][ P[@ #(I:Int) / X1] ] ; "new"[L2][ R[@ #(I:Int) / X2] ] ; true) ... </k> ... </thread>
          <NewIntGenerator> I => I +Int 1 </NewIntGenerator>
// If either one creates more new variables than the other we fail
rule <thread> ... <k> ... matchstdform("new"[X1:Id,L1:Ids][P:HigherProc] ; "new"[ .Ids ][R:HigherProc] ; true)
          => #(MATCHFAIL) ... </k> ... </thread>
rule <thread> ... <k> ... matchstdform("new"[ .Ids ][P:HigherProc] ; "new"[ X2:Id,L2:Ids ][R:HigherProc] ; true)
          => #(MATCHFAIL) ... </k> ... </thread>

// When you get down to no new channels
rule <thread> ... <k> ... matchstdform("new"[ .Ids ][P:HigherProc] ; "new"[ .Ids ][R:HigherProc] ; true)
          => match( P ; R ) ... </k> ... </thread>


// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //
// T/F Matching, i.e. the "DoTheseMatch( )" function
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //

// Match Function that rewrites to true if there is a match, and false otherwise
// INITIALLY WILL NOT SUPPORT PAR'S BUT SHOULD IN THE FUTURE
// THIS WILL BE VITAL FOR THE MORE GENERAL MATCH FUNCTION; YOU NEEED TO KNOW
// WHICH ONES MATCH BEFORE SUBSTITUTING.
// *** All calculations come before a #(PARPAUSE) tag to pause computation ****

// This is essentially the same as the match functin before; mostly just rewritten.

// (A) Give the rules for the match action which receives two arguments
rule <thread> ... <k> ... DoTheseMatch( P:HigherProcOrChan ; D:HigherProcOrChanNoVars )
              => tfmatchstdform( intostdtfmatchform(P) ; intostdtfmatchform(D)) ... </k> ... </thread>


// If a variable matches with something, check if that thing has free variables
// If it does, then rewrite as #(TFMATCHFAIL). If it does not, then substitute S for X
// in the original statement.
rule <thread> ... <k> ... DoTheseMatch( P:HigherProcOrChan ; X:Id )
              => . ... </k> ... </thread>

//rule <thread> ... <k> ... tfmatch(S:Stmt, X:Id) => freevars(S,X) ... </k> ... </thread>


// (B) Put everything into a standard form that's easy to work with
rule intostdtfmatchform(@P:HigherProc) => "quote"[ P ]
rule intostdtfmatchform(*C:Chan) => "eval"[ C ]
rule intostdtfmatchform(for(C:Chan <- D:Chan){P:HigherProc}) => "listen"[C][D][P]
rule intostdtfmatchform(for(C:Chan <= D:Chan){P:HigherProc}) => "persistentlisten"[C][D][P]
rule intostdtfmatchform(contract D:Chan(C:Chan) = { P:HigherProc }) => "persistentlisten"[C][D][P]
rule intostdtfmatchform(new X:Ids in { P:HigherProc }) => "new"[X][P]
rule intostdtfmatchform(C:Chan!(P:HigherProc)) => "send"[C][P]
rule intostdtfmatchform( Nil ) => "Nil"[ ]
// Expressions
rule intostdtfmatchform(S:String) => "string"[S]
rule intostdtfmatchform(B:Bool) => "bool"[B]
rule intostdtfmatchform(I:Int) => "int"[I]
// Misc
rule intostdtfmatchform( #(I:Int) ) => "hash"[I]
rule intostdtfmatchform( P1:HigherProc | P2:HigherProc ) => "par"[P1][P2]
// This should not be necessary; will be removed later,
// when channel equality can be checked.
rule intostdtfmatchform( X:Id ) => "var"[X]


// The "tfmatchstdform" function works like the "matchstdform" from before.
rule <thread> ... <k> ... tfmatchstdform( S1:String P1:StringlessMatchFormSyntax ; S2:String P2:StringlessMatchFormSyntax )
              => tfmatchstdform( S1 P1 ; S2 P2 ; S1 ==String S2 ) ... </k> ... </thread>

// To account for expressions
// Strings
rule <thread> ... <k> ... tfmatchstdform( "string"[S1:String] ; "string"[S2:String] ; true )
                    => tfmatchstdform( S1:String ; S2:String ; S1 ==String S2) ... </k> ... </thread>
rule <thread> ... <k> ... tfmatchstdform( S1:String ; S2:String ; true ) => . ... </k> ... </thread>
rule <thread> ... <k> ... tfmatchstdform( S1:String ; S2:String ; false ) => #(TFMATCHFAIL) ... </k> ... </thread>

// Ints
rule <thread> ... <k> ... tfmatchstdform( "int"[I1:Int] ; "int"[I2:Int] ; true )
                    => tfmatchstdform( I1:Int ; I2:Int ; I1 ==Int I2) ... </k> ... </thread>
rule <thread> ... <k> ... tfmatchstdform( I1:Int ; I2:Int ; true ) => . ... </k> ... </thread>
rule <thread> ... <k> ... tfmatchstdform( I1:Int ; I2:Int ; false ) => #(TFMATCHFAIL) ... </k> ... </thread>

// Bools
rule <thread> ... <k> ... tfmatchstdform( "bool"[B1:Bool] ; "bool"[B2:Bool] ; true )
                    => tfmatchstdform( B1:Bool ; B2:Bool ; B1 ==Bool B2) ... </k> ... </thread>
rule <thread> ... <k> ... tfmatchstdform( B1:Bool ; B2:Bool ; true ) => . ... </k> ... </thread>
rule <thread> ... <k> ... tfmatchstdform( B1:Bool ; B2:Bool ; false ) => #(TFMATCHFAIL) ... </k> ... </thread>

// As more expressions become part of rholang this will have to be updated


// Terminate if the strings do not match (they do not match on the outer shell)
rule <thread> ... <k> ... tfmatchstdform(S1:String P1:StringlessMatchFormSyntax ; S2:String P2:StringlessMatchFormSyntax ; false)
              => #(TFMATCHFAIL)  ... </k> ... </thread>

rule <thread> ... <k> ... tfmatchstdform( E1:Exp ; E2:Exp ; false)
              => #(TFMATCHFAIL)  ... </k> ... </thread>


// If the patterns are ever the same, as expressions or processes
rule <thread> ... <k> ... tfmatchstdform( S1:String P1:StringlessMatchFormSyntax ; S1:String P1:StringlessMatchFormSyntax ; true ) => . ... </k> ... </thread>
rule <thread> ... <k> ... tfmatchstdform(  E:Exp ; E:Exp ; true ) => . ... </k> ... </thread>

// When the pattern is a quote "@"
rule <thread> ... <k> ... tfmatchstdform( "quote"[P:HigherProc] ; "quote"[R:HigherProc] ; true)
          => DoTheseMatch( P ; R ) ... </k> ... </thread>

// When the pattern is an eval "*"
rule <thread> ... <k> ... tfmatchstdform( "eval"[C:Chan] ; "eval"[D:Chan] ; true)
          => DoTheseMatch( C ; D ) ... </k> ... </thread>

// When the pattern is a "for"
// Incorrect, as before; needs the same things as before.
rule <thread> ... <k> ... tfmatchstdform("listen"[C:Chan][D:Chan][P:HigherProc] ; "listen"[E:Chan][F:Chan][R:HigherProc] ; true )
          => DoTheseMatch(C ; E) ~> DoTheseMatch(D ; F) ~> DoTheseMatch(P ; R) ... </k> ... </thread>

// The (logic) of what we'd write if things worked.
/*
rule <thread> ... <k> ... tfmatchstdform("listen"[C:Chan][D:Chan][P:HigherProc],"listen"[E:Chan][F:Chan][R:HigherProc], true )
          => tfmatchstdform("listen"[D][P],"listen"[F][R],D ==Chan F) ... </k> ... </thread>

rule <thread> ... <k> ... tfmatchstdform("listen"[C:Chan][P:HigherProc],"listen"[D:Chan][R:HigherProc], true )
          => DoTheseMatch( C, D ) ~> DoTheseMatch( P, R ) ... </k> ... </thread>
*/

// When the pattern is a persistent "for"
// Also incorrect; we need things like in the previous listen construct
rule <thread> ... <k> ... tfmatchstdform("persistentlisten"[C:Chan][D:Chan][P:HigherProc] ; "persistentlisten"[E:Chan][F:Chan][R:HigherProc] ; true )
          => DoTheseMatch(C ; E) ~> DoTheseMatch(D ; F) ~> DoTheseMatch(P ; R) ... </k> ... </thread>

/*
// When the pattern is a persistent "for"
rule <thread> ... <k> ... DoTheseMatch("persistentlisten"(C:Chan)(D:Chan)(P:HigherProc),"persistentlisten"(E:Chan)(F:Chan)(R:HigherProc), true )
          => DoTheseMatch("persistentlisten"(D)(P),"persistentlisten"(F)(R),C == E) ... </k> ... </thread>

rule <thread> ... <k> ... DoTheseMatch("persistentlisten"(D:Chan)(P:HigherProc),"persistentlisten"(F:Chan)(R:HigherProc),true)
          => DoTheseMatch(intostdtfmatchform(D),intostdtfmatchform(F)) ~> DoTheseMatch(intostdtfmatchform(P),intostdtfmatchform(R)) ... </k> ... </thread>
*/

// When the pattern is "send"
rule <thread> ... <k> ... tfmatchstdform( "send"[C:Chan][P:HigherProc] ; "send"[D:Chan][R:HigherProc] ; true)
          => DoTheseMatch( C ; D ) ~> DoTheseMatch( P ; R ) ... </k> ... </thread>

// When the pattern is "hash"
rule <thread> ... <k> ... tfmatchstdform( "hash"[I:Int] ; "hash"[J:Int] ; true)
          => DoTheseMatch( I ; J ) ... </k> ... </thread>

// When the pattern is a "new"
rule <thread> ... <k> ... tfmatchstdform("new"[X1:Id,L1:Ids][P:HigherProc] ; "new"[X2:Id,L2:Ids][R:HigherProc] ; true)
          => tfmatchstdform("new"[L1][ P[@ #(I:Int) / X1] ] ; "new"[L2][ R[@ #(I:Int) / X2] ] ; true) ... </k> ... </thread>
          <NewIntGenerator> I => I +Int 1 </NewIntGenerator>
// If either one creates more new variables than the other we fail
rule <thread> ... <k> ... tfmatchstdform("new"[X1:Id,L1:Ids][P:HigherProc] ; "new"[ .Ids ][R:HigherProc] ; true)
          => #(MATCHFAIL) ... </k> ... </thread>
rule <thread> ... <k> ... tfmatchstdform("new"[ .Ids ][P:HigherProc] ; "new"[ X2:Id,L2:Ids ][R:HigherProc] ; true)
          => #(MATCHFAIL) ... </k> ... </thread>

// When you get down to no new channels
rule <thread> ... <k> ... tfmatchstdform("new"[ .Ids ][P:HigherProc] ; "new"[ .Ids ][R:HigherProc] ; true)
          => DoTheseMatch( P ; R ) ... </k> ... </thread>

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
         <k> ... matchstdform( "par"[P1:HigherProc][P2:HigherProc] ; "par"[Q1:HigherProc][Q2:HigherProc] ; true)
                 => #(PARPAUSE) ... </k>
         (.Bag =>
         <ParMatch> ...
           <from> fromparmatchstdform(P1 | P2) </from>
           <to> toparmatchstdform(Q1 | Q2) </to>
         ... </ParMatch>)
   ... </thread>

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
                                 <to> [S2:String][false][P2:HigherProc][I:Int]["available"] </to> ... </ParMatch>
                  <k> #(PARPAUSE) => DoTheseMatch( P1 ; P2 ) ~> #(PARPAUSE) ... </k> ... </thread>

// You can't try to match with a process that's already taken
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ J1:Int ][ J2:Int ][ "active" ]{ ListHead:List (ListItem(I) => .) ListTail:List } </from>
                                 <to> [S2:String][false][P2:HigherProcNoVars][I]["taken"] </to> ... </ParMatch> ... </thread>

// There's been a match!
// We need to do this in two steps, in order to deal with indices
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ I ][ (J:Int => J +Int 1) ][ "active" ]{ L1 } </from>
                                 <to> [S2:String][false][P2:HigherProc][I][("available" => "taken")] </to>
                                 // We snag one of the "to" cells to move forward
                                 <MatchToCounter> I2:Int => I2 +Int -1 </MatchToCounter> ... </ParMatch>
                  // There's been a match because there is nothing before the #(PARPAUSE) in the k-cell
                  // This means that the "DoTheseMatch" function was successful, and left nothing.
                  <k> #(PARPAUSE) ... </k> ... </thread>

// Now we put the index in our successful from cell back, using it to grab the next one
// We mark P2 as taken, mark P1 as inactive and then move up one, marking P as active and grabbing a list of all the "to" cells to work on.
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ I ][ (J:Int => J +Int -1) ][ ("active" => "inactive") ]{ L1 } </from>
                                 <to> [S2:String][false][P2:HigherProc][I]["taken"] </to>
                                 <from> [S:String][false][ P:HigherProc ][ M:Int ][ J ][ ("inactive" => "active") ]{ _ => L2 } </from>
                                 <ListofAllToCells> L2:List </ListofAllToCells> ... </ParMatch>
                  // There's been a match because there is nothing before the #(PARPAUSE) in the k-cell
                  // This means that the "DoTheseMatch" function was successful, and left nothing.
                  <k> #(PARPAUSE) ... </k> ... </thread>

// We have to do the same rule, but in the case we matched on level 0 (since we changed 0 to -1)
// We can do it in one rule because there is no problem with indices
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ I ][ (0 => -1) ][ ("active" => "inactive") ]{ L1 } </from>
                                 <to> [S2:String][false][P2:HigherProc][I]["taken"] </to>
                                 <from> [S:String][false][ P:HigherProc ][ M:Int ][ 1 ][ ("inactive" => "active") ]{ _ => L2 } </from>
                                 <ListofAllToCells> L2:List </ListofAllToCells> ... </ParMatch>
                  // There's been a match because there is nothing before the #(PARPAUSE) in the k-cell
                  // This means that the "DoTheseMatch" function was successful, and left nothing.
                  <k> #(PARPAUSE) ... </k> ... </thread>


// There's been a match! (with a variable)
// General step for binding to variables
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ (-1 => "var") ][ (J:Int => J +Int 1) ][ "active" ]{ ListHead:List (ListItem("var") => .) ListTail:List } </from>
                  ... </ParMatch> ... </thread>

rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ "var" ][ (J:Int => J +Int -1) ][ ("active" => "inactive") ]{ L1:List } </from>
                    <from> [S2:String][false][ P2:HigherProc ][ I2:Int ][ J ][ ("inactive" => "active") ]{ _ => L2:List } </from>
                    <ListofAllToCells> L2:List </ListofAllToCells>
                  ... </ParMatch> ... </thread>

// Same step, but if we are at the 0 level
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ "var" ][ (0 => 1) ][ "active" ]{ L:List } </from>
                  ... </ParMatch> ... </thread>

// The match failed, so we try another one (we redo the general step)
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ (I => -1) ][ J:Int ][ "active" ]{ L1 } </from>
                                 <to> [S2:String][false][P2:HigherProc][I:Int]["available"] </to> ... </ParMatch>
                  <k> ... (#(TFMATCHFAIL) ~> M:K => .) ~> #(PARPAUSE) ... </k> ... </thread>


// There are no more options for possible matches (go back down)
// This is for a non-variable
rule <thread> ... <ParMatch> ...
                      <from> [S1:String][false][ P1:HigherProc ][ -1 ][ (J:Int => J +Int -1) ][ ("active" => "transition") ]{ .List } </from>
                  ... </ParMatch> ... </thread>


rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ (I1 => -1) ][ (J:Int => J:Int +Int 1) ][ ("transition" => "inactive") ]{ .List } </from>
                                 // Go down one; you can't use what you had before
                                 <from> [R1:String][false][ Q1:HigherProc ][ (I2 => -1) ][ J ][ ("inactive" => "active") ]{ L } </from>
                                 <to> [R2:String][false][Q2:HigherProc][I2:Int][("taken" => "available")] </to>
                                 // We have to return one of the "to" cells to move forward
                                 <MatchToCounter> C => C +Int 1 </MatchToCounter> ... </ParMatch> ... </thread>


// We have to do the same rule, but in the case that we were on the first level and we have to go back down to 0
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ (I1 => -1) ][ (0 => 1) ][ ("transition" => "inactive") ]{ .List } </from>
                                 // Go down one; you can't use what you had before
                                 <from> [R1:String][false][ Q1:HigherProc ][ (I2 => -1) ][ -1 ][ ("inactive" => "active") ]{ L } </from>
                                 <to> [R2:String][false][Q2:HigherProc][I2:Int][("taken" => "available")] </to>
                                 // We have to return one of the "to" cells to move forward
                                 <MatchToCounter> C => C +Int 1 </MatchToCounter>  ... </ParMatch> ... </thread>

// Going back down where the thing below matched to a variable (essential duplicate of rules above)
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ (I1 => -1) ][ (J:Int => J:Int +Int 1) ][ ("transition" => "inactive") ]{ .List } </from>
                                 // Go down one; you can't use what you had before
                                 <from> [R1:String][false][ Q1:HigherProc ][ ("var" => -1) ][ J ][ ("inactive" => "active") ]{ L } </from>
                  ... </ParMatch> ... </thread>

rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ (I1 => -1) ][ (0 => 1) ][ ("transition" => "inactive") ]{ .List } </from>
                                 // Go down one; you can't use what you had before
                                 <from> [R1:String][false][ Q1:HigherProc ][ ("var" => -1) ][ -1 ][ ("inactive" => "active") ]{ L } </from>
              ... </ParMatch> ... </thread>


// There are no more options on the 0 level, so the match failes and we rewrite #(PARPAUSE) as #(MATCHFAIL)
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ -1 ][ -2 ][ "transition" ]{ .List } => #(TFMATCHFAIL) </from> ... </ParMatch>
                   <k> #(PARPAUSE) => #(MATCHFAIL) ... </k> ... </thread>

rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ -1 ][ -1 ][ "transition" ]{ .List } => #(TFMATCHFAIL) </from> ... </ParMatch>
                   <k> #(PARPAUSE) => #(MATCHFAIL) ... </k> ... </thread>

rule <thread> ... <ParMatch> ... <from> #(TFMATCHFAIL) </from> ... </ParMatch> => . ... </thread>


// We've gotten to the end but we need to make sure there
// aren't any "to" cells which have been neglected.
// This can happen by matching too many processes to variables
// If this happens, we have to go back down and try again,
// potentially failing the whole match.

// First we see if the MatchToCounter is 0. If it is, that means we
// didn't neglect any "to" cells. If not, then we need to try again
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ E:Exp ][ J:Int ][ "active" ]{ L1:List } </from>
                    <MatchFromCounter> J </MatchFromCounter>
                    <MatchToCounter> J2:Int </MatchToCounter>
                    <Matched> .K => (J2 =Int 0) </Matched>
                  ... </ParMatch> ... </thread>

// If there are leftover "to" cells, we need to go back down
// If the last thing we matched to was a variable, then we just go back down and try again
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ ("var" => -1) ][ (J:Int => J +Int -1) ][ "active" ]{ L1:List } </from>
                    <MatchFromCounter> J </MatchFromCounter>
                    <Matched> false => . </Matched>
                  ... </ParMatch> ... </thread>

// If the last thing we matched to was not a variable, we need to go back down
// and update the thing we were matched to to say "available"
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ (I:Int => -1) ][ (J:Int => J +Int -1) ][ "active" ]{ L1:List } </from>
                    <to> [S2:String][false][P2:HigherProc][I][("taken" => "available")] </to>
                    <MatchFromCounter> J </MatchFromCounter>
                    <Matched> false => . </Matched>
                  ... </ParMatch> ... </thread>


// We've matched everything, this match as a whole is a success!
// (We may have extra variables left over, which we bind to "Nil")

rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ I ][ J:Int ][ ("active" => "inactive") ]{ L1 } </from>
                                 // So we matched
                                 <Matched> true </Matched> ... </ParMatch>
                   <k> #(PARPAUSE) ... </k> ... </thread>

// What happens when "Matched" is true
rule <thread> ... <ParMatch> ...
                            <from> [S1:String][false][ P1:HigherProc ][ I ][ J:Int ][ "inactive" ]{ L1 } => . </from>
                            <to> [S2:String][false][P2:HigherProc][I:Int]["taken"] => . </to>
                            <Matched> true </Matched>
                            <MatchToCounter> I1 => I1 +Int 1 </MatchToCounter>
                            <MatchFromCounter> I2 => I2 +Int -1 </MatchFromCounter>
                            ... </ParMatch>
                  <k> #(PARPAUSE) => #(PARPAUSE) ~> match(P1 ; P2) ... </k>  ... </thread>

rule <thread> ... <ParMatch> ...
                    <to> [S:String][false][P:HigherProc][I]["variable"]
                     =>  [S:String][false][P:HigherProc][I]["variable"]{ Nil } </to>
                    <Matched> true </Matched> ... </ParMatch> ... </thread>

// Now we (nondetermistically) assign processes which are to be bound to vars
// by grouping them.
rule <thread> ... <ParMatch> ...
                            <from> [S1:String][false][ P1:HigherProc ]["var"][ J:Int ][ "inactive" ]{ L1 } => . </from>
                            <to> [S2:String][false][P2:HigherProc][I]["variable"]{ (P:HigherProc => P | P1) } </to>
                            <Matched> true </Matched>
                            <MatchToCounter> I1 => I1 +Int 1 </MatchToCounter>
                            <MatchFromCounter> I2 => I2 +Int -1 </MatchFromCounter>
                            ... </ParMatch>  ... </thread>

// Once that is done, we send all the vars to be matched; if they got no processes
// they get the "Nil" process
rule <thread> ... <ParMatch> ...
                            <to> [S:String][false][P1:HigherProc][I]["variable"]{ P2:HigherProc } => . </to>
                            <Matched> true </Matched>
                            <MatchFromCounter> 0 </MatchFromCounter>
                            ... </ParMatch>
                  <k> #(PARPAUSE) => #(PARPAUSE) ~> match(P2 ; P1) ... </k>  ... </thread>


// Once this has exhausted, clear the contents of the the "ParMatch" cell,
// unpause the calculation and move forward
rule <thread> ... <ParMatch>
                      <MatchFromCounter> I2:Int => . </MatchFromCounter>
                      <MatchToCounter> I1:Int => . </MatchToCounter>
                      <ListofAllToCells> L1:List => . </ListofAllToCells>
                      <Wildcards> B2:Bool => . </Wildcards>
                      <Matched> true => . </Matched>
                      <BindToVars> Nil => . </BindToVars>
                  </ParMatch>
                  // Continue forward on the calculation! Hooray!!
                  <k> #(PARPAUSE) => . ... </k>
                  ... </thread>

// Empty cells get deleted
rule <thread> ... <ParMatch> ... <MatchFromCounter> .K </MatchFromCounter> => . ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <MatchToCounter> .K </MatchToCounter> => . ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <ListofAllToCells> .List </ListofAllToCells> => . ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <Wildcards> .K </Wildcards> => . ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <BindToVars> .K </BindToVars> => . ... </ParMatch> ... </thread>
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
                  <ParMatch multiplicity="*">
                    <to multiplicity="*"> .K </to>
                    <from multiplicity="*"> .K </from>
                    <MatchFromCounter> 0 </MatchFromCounter>
                    <MatchToCounter> 0 </MatchToCounter>
                    <ListofAllToCells> .List </ListofAllToCells>
                    <Wildcards> false </Wildcards>
                    <BindToVars> Nil </BindToVars>
                    <Matched> .K </Matched>
                  </ParMatch>
                </thread>
                <GlobalListofOutIds color="orange"> .List </GlobalListofOutIds>
                <GlobalListofInIds color="purple"> .List </GlobalListofInIds>
                <NewIntGenerator color="purple"> 0 </NewIntGenerator>
              </T>


endmodule
