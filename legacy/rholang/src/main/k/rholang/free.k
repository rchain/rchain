module FREE
imports JOINS

// *******************************************************************
//                          FREE
// *******************************************************************

// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //
/// The "fv( )" function: collects free variables of any statement
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //
// These are the semantics for checking if a certain statement has free variables. This is
// useful (a) to check to make sure that any written program doesn't have free variables,
// and (b) to make sure that a pattern does not match with something that has a free variable in it.

// We're going to do this by constructing a tree, hence the cells we call will be called "nodes".
// By using the tree we will construct the set of free variables in a statement. We collect the
// free variables using the free vars rules from the RHO calculus.

// Initiating the check
rule <thread> ... <k> fv(MATCHTYPE:String , TOBECHECKED:HigherPat , VARTOBINDTO:Id)
                                      => #(VARPAUSE)[MATCHTYPE , TOBECHECKED , VARTOBINDTO] ... </k>
              (.Bag => <FreeVars>
                  <node> [0][ TOBECHECKED ]["base"][ .List ]{ .Set }[ false ] </node>
                  <NodeCounter> 1 </NodeCounter>
              </FreeVars>) ... </thread>



// Rules for CONSTRUCTING the tree
// *** Send
// If at the base node, just abstract it up a node. It's easier to work with it this way.
rule <thread> ... <FreeVars> ...
                   <node> [ID:Int][ C:HigherNamePat!(P:HigherProcPats) ][ "base" ][ (.List
                                   => ListItem(NEWID)) ]{ FREEVARS:Set }[ (false
                                   => true) ] </node>
          (.Bag => <node>
                    [NEWID][ C:HigherNamePat!(P:HigherProcPats) ][ ID ][ .List ]{ .Set }[false]
                  </node>)
                   <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
              ... </FreeVars> ... </thread>

// First go through the list, making a new node for each process in the HigherProcPats
rule <thread> ...
        <FreeVars> ...
         <node>
          [ID:Int][
              (C:HigherNamePat!(PHead1:HigherProcPat , PHead2:HigherProcPat , PTail:HigherProcPats)
            => C:HigherNamePat!(PHead2:HigherProcPat, PTail:HigherProcPats)) ][ PARENT:Int ][ (.List
            => ListItem(NEWID)) ListOfChildren:List ]{ FREEVARS:Set }[ false ]
         </node>
         (.Bag => <node> [NEWID][ PHead1 ][ ID ][ .List ]{ .Set }[false]  </node>)
         <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
    ... </FreeVars> ... </thread>

// When we only have one process left
rule <thread> ...
        <FreeVars> ...
          <node>
           [ID:Int][ C:HigherNamePat!(LastProc:HigherProcPat) ][ PARENT:Int ][
           ListOfChildren:List (.List => ListItem(NEWID) ListItem(NEWID +Int 1)) ]{ FREEVARS:Set }[
           (false => true) ]
          </node>
 (.Bag => <node> [NEWID][ C ][ ID ][ .List ]{ .Set }[false]  </node>
          <node> [NEWID +Int 1][ LastProc ][ ID ][ .List ]{ .Set }[false]  </node>)
             <NodeCounter> (NEWID:Int => NEWID +Int 2) </NodeCounter>
        ... </FreeVars> ... </thread>

// *** Persistent send: all the same rules as the send, but with the persistent syntax
// If at the base node, just abstract it up a node. It's easier to work with it this way.
rule <thread> ...
        <FreeVars> ...
          <node>[ID:Int][ C:HigherNamePat!!(P:HigherProcPats) ][ "base" ][ (.List
                                   => ListItem(NEWID)) ]{ FREEVARS:Set }[ (false => true) ] </node>
 (.Bag => <node>[NEWID][ C:HigherNamePat!(P:HigherProcPats) ][ ID ][ .List ]{ .Set }[false]</node>)
         <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
    ... </FreeVars> ... </thread>

// First go through the list, making a new node for each process in the HigherProcPats
rule <thread> ...
        <FreeVars> ...
          <node> [ID:Int][
             (C:HigherNamePat!!(PHead1:HigherProcPat , PHead2:HigherProcPat , PTail:HigherProcPats)
           => C:HigherNamePat!(PHead2:HigherProcPat, PTail:HigherProcPats)) ][ PARENT:Int ][ (.List
           => ListItem(NEWID)) ListOfChildren:List ]{ FREEVARS:Set }[ false ] </node>
 (.Bag => <node> [NEWID][ PHead1 ][ ID ][ .List ]{ .Set }[false]  </node>)
         <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
    ... </FreeVars> ... </thread>

// When we only have one process left
rule <thread> ...
        <FreeVars> ...
          <node>
           [ID:Int][ C:HigherNamePat!!(LastProc:HigherProcPat) ][ PARENT:Int ][
           ListOfChildren:List (.List => ListItem(NEWID) ListItem(NEWID +Int 1)) ]{FREEVARS:Set}[
           (false => true) ]
          </node>
         (.Bag => <node> [NEWID][ C ][ ID ][ .List ]{ .Set }[false]  </node>
                  <node> [NEWID +Int 1][ LastProc ][ ID ][ .List ]{ .Set }[false]  </node>)
         <NodeCounter> (NEWID:Int => NEWID +Int 2) </NodeCounter>
    ... </FreeVars> ... </thread>

// *** Quote
rule <thread> ...
        <FreeVars> ...
          <node> [ID:Int][ @ P:HigherProcPat ][ PARENT:Exp ][ (.List
                                  => ListItem(NEWID)) ]{ FREEVARS:Set }[ (false => true) ] </node>
 (.Bag => <node> [NEWID][ P ][ ID ][ .List ]{ .Set }[false]  </node>)
         <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
    ... </FreeVars> ... </thread>
// *** Eval
rule <thread> ...
        <FreeVars> ...
          <node> [ID:Int][ * C:HigherNamePat ][ PARENT:Exp ][ (.List
                                  => ListItem(NEWID)) ]{ FREEVARS:Set }[ (false
                                  => true) ] </node>
 (.Bag => <node> [NEWID][ C ][ ID ][ .List ]{ .Set }[false]  </node>)
         <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
    ... </FreeVars> ... </thread>
// *** Par
rule <thread> ...
        <FreeVars> ...
          <node> [ID:Int][ P1:HigherProcPat | P2:HigherProcPat ][ PARENT:Exp ][ (.List
            => ListItem(NEWID) ListItem(NEWID +Int 1)) ]{ FREEVARS:Set }[ (false
            => true) ] </node>
         (.Bag => <node> [NEWID][ P1 ][ ID ][ .List ]{ .Set }[false]  </node>
                  <node> [NEWID +Int 1][ P2 ][ ID ][ .List ]{ .Set }[false]  </node>)
         <NodeCounter> (NEWID:Int => NEWID +Int 2) </NodeCounter>
    ... </FreeVars> ... </thread>



// Logical "and"
// For a conjunction of HigherProcesses
rule <thread> ...
        <FreeVars> ...
          <node> [ID:Int][ P1:HigherProcPat /\ P2:HigherProcPat ][ PARENT:Exp ][ (.List
            => ListItem(NEWID) ListItem(NEWID +Int 1)) ]{ FREEVARS:Set }[ (false
            => true) ] </node>
         (.Bag => <node> [NEWID][ P1 ][ ID ][ .List ]{ .Set }[false]  </node>
                  <node> [NEWID +Int 1][ P2 ][ ID ][ .List ]{ .Set }[false]  </node>)
         <NodeCounter> (NEWID:Int => NEWID +Int 2) </NodeCounter>
    ... </FreeVars> ... </thread>


// Logical "or"
// For a statement P1 \/ P2, we do not bind any variables in either P1 or P2. Thus if we hit a
// logical "or", we check for any free variables at all. Any variables here are free variables
// in the entire statement.

// For HigherProcecesses
 // Make the 'ornode'
rule <thread> ...
       <FreeVars> ...
         <node>
           [ID:Int][ P1:HigherProcPat \/ P2:HigherProcPat ][ PARENT:Exp ][
           .List => ListItem(NEWID) ]{ FREEVARS:Set }[(false => true)]
         </node>
         (.Bag => <ornode>
           [NEWID][ P1:HigherProcPat \/ P2:HigherProcPat ][ ID ][ .List ]{ FREEVARS:Set }[false]
         </ornode>)
         <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
   ... </FreeVars> ... </thread>
 // Continue building the tree
rule <thread> ...
       <FreeVars> ...
         <ornode>
           [ID:Int][ P1:HigherProcPat \/ P2:HigherProcPat ][ PARENT:Exp ][ (.List
           => ListItem(NEWID) ListItem(NEWID +Int 1)) ]{ FREEVARS:Set }[(false => true)]
         </ornode>
         (.Bag => <node> [NEWID][ P1 ][ ID ][ .List ]{ .Set }[false]  </node>
                  <node> [NEWID +Int 1][ P2 ][ ID ][ .List ]{ .Set }[false]  </node>)
         <NodeCounter> (NEWID:Int => NEWID +Int 2) </NodeCounter>
   ... </FreeVars> ... </thread>

// Absorb children into ornodes
// (Now for both HigherNames and HigherProcesses)
rule <thread> ...
        <FreeVars> ...
          <ornode>
            [ID:Int][ P:HigherPat ][ PARENT:Exp ][
            (ListItem(ChildID:Int) => .List) ChildrenListTail:List
            ]{ (.Set => ChildFreeVars) FreeVars:Set }[ true ]
          </ornode>
          (<node> [ChildID][ Q:HigherPat ][ ID ][ .List ]{ ChildFreeVars:Set }[true] </node>
                => .Bag)
    ... </FreeVars> ... </thread>
// Once you've absorbed all the children into the ornode, transport its free
// variables to the base's free variable list
rule <thread> ...
        <FreeVars> ...
          <ornode>
            [ID:Int][ P:HigherPat ][ PARENT:Int ][ .List ]{
            (SetItem(X:Id) => .Set) FreeVars:Set }[ true ]
          </ornode>
          <node> [0][ Q:HigherPat ][ "base" ][ BaseChildren:List ]{
          (.Set => SetItem(X)) BaseFreeVars:Set }[true]  </node>
    ... </FreeVars> ... </thread>
// Then delete the ornode
rule <thread> ...
        <FreeVars> ...
          (<ornode>
            [ID:Int][ P:HigherPat ][ PARENT:Int ][ .List ]{ .Set }[ true ]
          </ornode> => .Bag)
          <node> [PARENT][ Q:HigherPat ][ GrandParentId:Exp ][

          (ListItem(ID) => .List) ChildrenListTail:List
          ]{ FreeVars:Set }[true] </node>
    ... </FreeVars> ... </thread>



// Rules for binding operations
// Listen
rule <thread> ...
        <FreeVars> ...
          <node>
            [ID:Int][ for( C:HigherNamePats <- D:HigherNamePat ){ P:HigherProcPat } ][ PARENT:Exp ][
            (.List => ListItem(NEWID) ListItem(NEWID +Int 1)) ]{ FREEVARS:Set }[ (false => true) ]
          </node>
 (.Bag => <node> [NEWID][ D ][ ID ][ .List ]{ .Set }[false]  </node>
          <bnode> [NEWID +Int 1][ ID ][ .K ; P ][ .List ; C ]{ .Set }[false]  </bnode>)
         <NodeCounter> (NEWID:Int => NEWID +Int 2) </NodeCounter>
    ... </FreeVars> ... </thread>
// Persistent Listen
rule <thread> ...
        <FreeVars> ...
          <node>
            [ID:Int][ for( C:HigherNamePats <= D:HigherNamePat ){ P:HigherProcPat } ][ PARENT:Exp ][
            (.List => ListItem(NEWID) ListItem(NEWID +Int 1)) ]{ FREEVARS:Set }[ (false => true) ]
          </node>
 (.Bag => <node> [NEWID][ D ][ ID ][ .List ]{ .Set }[false]  </node>
          <bnode> [NEWID +Int 1][ ID ][ .K ; P ][ .List ; C ]{ .Set }[false]  </bnode>)
         <NodeCounter> (NEWID:Int => NEWID +Int 2) </NodeCounter>
    ... </FreeVars> ... </thread>
// Contract
rule <thread> ...
        <FreeVars> ...
          <node>
            [ID:Int][ contract D:HigherNamePat (C:HigherNamePats) = {P:HigherProcPat} ][ PARENT:Exp
            ][(.List => ListItem(NEWID) ListItem(NEWID +Int 1)) ]{ FREEVARS:Set }[ (false => true)]
          </node>
       (.Bag => <node> [NEWID][ D ][ ID ][ .List ]{ .Set }[false]  </node>
                <bnode> [NEWID +Int 1][ ID ][ .K ; P ][ .List ; C ]{ .Set }[false]  </bnode>)
         <NodeCounter> (NEWID:Int => NEWID +Int 2) </NodeCounter>
    ... </FreeVars> ... </thread>
// New
rule <thread> ...
        <FreeVars> ...
            <node> [ID:Int][ new X:Ids in { P:HigherProcPat } ][ PARENT:Exp ][ (.List
                                   => ListItem(NEWID)) ]{ FREEVARS:Set }[ (false => true) ] </node>
   (.Bag => <bnode> [NEWID][ ID ][ .K ; P ][ .List ; X ]{ .Set }[false]  </bnode>)
         <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
    ... </FreeVars> ... </thread>

// Match
// If it's at the base, make another node so we're not tampering with the base process stored
rule <thread> ...
        <FreeVars> ...
          <node> [ID:Int][ match P:HigherProcPat {
                  Q:HigherProcPat => { R:HigherProcPat }
                  HigherMatchCases } ][ "base" ][ (.List
                  => ListItem(NEWID)) ]{ FREEVARS:Set }[ (false
                  => true) ] </node>
 (.Bag => <node> [NEWID][ match P:HigherProcPat { Q:HigherProcPat => { R:HigherProcPat }
                  HigherMatchCases } ][ ID ][ .List ]{ .Set }[ false ]
                  </node>)
         <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
    ... </FreeVars> ... </thread>

// First make all the bnodes. Leave the node's bool at false for now
rule <thread> ...
        <FreeVars> ...
          <node> [ID:Int][ match P:HigherProcPat {
                                    Q:HigherProcPat => { R:HigherProcPat }
                                    Q2:HigherProcPat => { R2:HigherProcPat }
                                    HigherMatchCases }
                          ][ PARENT:Int ][ ListOfChildren:List ]{ FREEVARS:Set }[ false ]
                          =>
                [ID:Int][ match P:HigherProcPat {
                                    Q2:HigherProcPat => { R2:HigherProcPat }
                                    HigherMatchCases }
                        ][ PARENT ][ ListOfChildren:List ListItem(NEWID) ]{ FREEVARS:Set }[ false ]
          </node>
         (.Bag => <bnode> [NEWID][ ID ][ .K ; R ][ .List ; Q ]{ .Set }[false]  </bnode>)
         <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
    ... </FreeVars> ... </thread>

// When there's just one of the HigherMatchCases left, make the regular node for the process we're
// matching, and change the node's bool to true
rule <thread> ...
        <FreeVars> ...
            <node> [ID:Int][ match P:HigherProcPat {
                                    Q:HigherProcPat => { R:HigherProcPat }
                                                  } ][ PARENT:Int ][
                         ListOfChildren:List (.List => ListItem(NEWID) ListItem(NEWID +Int 1)) ]{
                         FREEVARS:Set }[ (false => true) ] </node>
   (.Bag => <node> [NEWID][ P ][ ID ][ .List ]{ .Set }[false]  </node>
            <bnode> [NEWID +Int 1][ ID ][ .K ; R ][ .List ; Q ]{ .Set }[false]  </bnode>)
         <NodeCounter> (NEWID:Int => NEWID +Int 2) </NodeCounter>
    ... </FreeVars> ... </thread>


// Constructing with terminal nodes, which are expressions (including unforgeable(Int)'s), as well
// as Nil
rule <thread> ...
          <FreeVars> ...
              <node> [ID:Int][ E:Exp ][ PARENT:Exp ][ .List ]{ .Set }[ (false => true) ] </node>
      ... </FreeVars>
 ... </thread>

// unforgeable(Int)'s
rule <thread> ...
          <FreeVars> ...
              <node> [ID:Int][ unforgeable(I:Int) ][ PARENT:Exp ][ .List ]{ .Set }[ (false => true) ] </node>
      ... </FreeVars>
 ... </thread>

// Nil
rule <thread> ...
          <FreeVars> ...
              <node> [ID:Int][ Nil ][ PARENT:Exp ][ .List ]{ .Set }[ (false => true) ] </node>
      ... </FreeVars>
 ... </thread>

// Semantics for variables. When we have a variable at the end of a node, we delete that node and
// add the variable to the list of vars of its parent
rule <thread> ...
      <FreeVars> ...
          <node>
            [ID:Int][ X:Id ][ PARENT:Exp ][ .List ]{ (.Set => SetItem(X)) }[ (false => true) ]
          </node>
  ... </FreeVars>
 ... </thread>

// Semantics for wildcards. Wildcards can only occur in patterns, so they cannot contribute to the
// overall number of free variables. We just delete them.
rule <thread> ...
      <FreeVars> ...
          <node>
            [ID:Int][ X:Wildcard ][ PARENT:Exp ][ .List ]{ .Set }[ (false => true) ]
          </node>
  ... </FreeVars>
 ... </thread>


// Semantics for building the "bnode"s
// We need bnodes because we need to keep track of which variables certain constructs bind, and
// which of those are still free. Locally free variables do not contribute to the free variables
// we're looking for, as long as their entire scope is contained within the statement given to
// the ground node. As it is written, bnodes can have one "plus" side and any number of "minus"
// sides.
// Creates the "plus" branch
rule <thread> ...
      <FreeVars> ...
        <bnode>
        [ID:Int][PARENT:Exp][ .K ; PlusProc:HigherProcPat ][.List ; C:HigherPats]{.Set}[false]
          => [ID:Int][PARENT:Exp][ NEWID ; .K ][.List ; C:HigherPats]{.Set}[false] </bnode>
        (.Bag => <node> [NEWID][ PlusProc ][ ID ][ .List ]{ .Set }[false]  </node>)
        <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
  ... </FreeVars>
 ... </thread>

// The "minus" branch(es)
rule <thread> ...
      <FreeVars> ...
        <bnode>
           [ID:Int][PARENT:Exp][PlusID:Int ; .K ][ MinusIDs:List ;
           MinusChan1:HigherPat , MinusChan2:HigherPat , MinusChans:HigherPats
           ]{.Set}[false] =>
           [ID:Int][PARENT:Exp][PlusID:Int ; .K ][ MinusIDs:List ListItem(NEWID) ;
           MinusChan2:HigherPat , MinusChans:HigherPats ]{.Set}[false]</bnode>
         (.Bag => <node> [NEWID][ MinusChan1 ][ ID ][ .List ]{ .Set }[false]  </node>)
         <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
   ... </FreeVars>
 ... </thread>

rule <thread> ...
        <FreeVars> ...
          <bnode> [ID:Int][PARENT:Exp][PlusID:Int ; .K ][ MinusIDs:List ; MinusChan:HigherPat
                  ]{.Set}[false] =>
                  [ID:Int][PARENT:Exp][PlusID:Int ; .K ][ MinusIDs:List ListItem(NEWID) ; .K
                  ]{.Set}[false]</bnode>
          (.Bag => <node> [NEWID][ MinusChan ][ ID ][ .List ]{ .Set }[false]  </node>)
          <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
    ... </FreeVars>
 ... </thread>


// Once there's nothing left to make nodes off the bnode we change the bnode's bool to "true"
rule <thread> ...
      <FreeVars> ...
        <bnode>
          [ID:Int][PARENT:Exp][PlusID:Int ; .K ][ MinusIDs:List ; .K ]{.Set}[(false => true)]
        </bnode> ...
      </FreeVars> ...
    </thread>


// Semantics for DECONSTRUCTING the tree, starting from the ends of the branches, at the terminal
// nodes. When something no longer has any children listed, all of its children have been
// deconstructed. So we deconstruct that node, passing on its list of free vars to its parent
rule <thread> ... <FreeVars> ...
                     (<node> [ID:Int][ ProcOrName:HigherPat ][ PARENT:Int ][ .List ]{
                                                           ChildVars:Set }[ true ] </node> => .Bag)
                      <node> [PARENT][ParentProcOrName:HigherPat][GRANDPARENT:Exp][
                                  (ListItem(ID) => .List) ChildrenListTail:List ]{
                                  (.Set => ChildVars) ParentVars:Set }[true] </node>
              ... </FreeVars> ... </thread>

// Semantics for deconstructing the "bnode". We need to collect the free variables from the Plus
// branch and then subtract from them the Minus branch. It is precisely because of this step that
// we have to use a tree structure. If we break up a program into its parts, like we do in the
// matching algorithm, and simply go along collecting and erasing free variables from a set,
// we might end up erasing too much. Furthermore, because of variable shadowing, we need to start
// from the innermost scopes and move back outward. These two things make it so it's necessary to
// use a tree in the way that we do.

// bnodes
// Set minus the MinusVars from the PlusVars sets, giving the free variables
rule <thread> ... <FreeVars> ...
        <bnode>
          [BNodeID:Int][Parent:Exp][PlusID:Int ; .K ][
          (ListItem(MinusID) => .List) MinusIDs:List ; .K ]{.Set}[true]
        </bnode>
        // PlusVars
        <node> [PlusID:Int][ PlusProcOrName:HigherPat ][ BNodeID ][ .List ]{
        (PlusVars:Set => PlusVars -Set MinusVars) }[true] </node>
        // MinusVars
        (<node> [MinusID:Int][ MinusProcOrName:HigherPat ][
        BNodeID ][ .List ]{ MinusVars:Set }[true] </node> => .Bag)
    ... </FreeVars> ... </thread>

// Rewrite the <bnode> so it compresses like a normal node
rule <thread> ... <FreeVars> ...
        (<bnode>
          [BNodeID:Int][Parent:Exp][PlusID:Int ; .K ][ .List ; .K ]{.Set}[true]
        </bnode>
        // PlusVars
        <node> [PlusID:Int][ PlusProcOrName:HigherPat ][
        BNodeID ][ .List ]{ FreeVars:Set }[true] </node> => .Bag)
        (.Bag => <node> [BNodeID]["bnode"][Parent][.List]{ FreeVars }[true] </node>)
    ... </FreeVars> ... </thread>

// SOLUTION TO DOUBLE BINDING VARIABLE PROBLEM


// At the end, we rewrite to #(VARPAUSE){ Set of Free Vars }. Depending on the context, a nonempty
// list of free vars may return an error or be successful.
rule <thread> ...
      (<FreeVars> ...
           <node> [ID:Int][BaseProc:HigherPat]["base"][.List]{FreeVars:Set}[true] </node>
  ... </FreeVars> => .Bag)
           <k> #(VARPAUSE)[ MATCHTYPE:String , BaseProc , Z:Id ]
                          =>  #(VARPAUSE)[ MATCHTYPE:String , BaseProc , Z:Id ]{ FreeVars } ... </k>
 ... </thread>


endmodule
