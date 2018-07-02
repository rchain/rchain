module FREE
imports MATCHING-WITH-PAR

// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //
/// The "fv( )" function: collects free variables of any statement
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //
// These are the semantics for checking if a certain statement has free variables. This is
// useful (a) to check to make sure that any written program doesn't have free variables,
// and (b) to make sure that a pattern does not match with something that has a free variable in it.

// We're going to do this by constructing a tree, hence the cells we call will be called "nodes".
// By using the tree we will construct the set of free variables in a statement. We collect the
// free variables using the free vars rules from the RHO calculus.

// TO DO: INCORPORATE LISTS

// First we construct the syntax for our nodes
syntax Ints ::= List{ Int, ","}
syntax HigherProcsOrChans ::= List{HigherProcOrChan, ","}
syntax NodeSyntax ::=
            // For normal nodes, the syntax is
            //[ Id ][ Proc/Chan ][ Parent ][ Children ]{ Multiset of free vars }[ Node constructed ]
             "[" Int "][" HigherProcOrChan "][" Exp "][" List "]{" List "}[" Bool "]"
            // Nodes corresponding to binders (bnodes) are given the syntax
            // [ Id ][ Parent ][ Plus ID ; Plus Process/Channel ][ Minus ID ; Minus Process/Channel
            //                                        ]{ Multiset of free vars }[ Node constructed ]
            | "[" Int "][" Exp "][" Int ";" HigherProcsOrChans "][" List ";" HigherProcsOrChans "]{"
                                                                                  List "}[" Bool "]"
            // Syntax for the calculation of figuring out if (a) there are duplicate binding
            // variables (resulting in bad syntax; will return an error), and (b) which are
            // the free variables in a binding construct.
            // For the Plus side: [ Id ][ ParentsID ]{ plus vars }{ plus vars }
            //            The duplicated set is so that we can systematically go one-by-one on
            //            the necessary checks for (a) and (b)
            | "[" Int "][" Int "]{" List "}{" List "}"
            // For the minus side: [ Id ][ ParentsID ][ the variable we're checking ]{
            //                                                            minus vars }{ minus vars }
            | "[" Int "][" Int "][" Id "]{" List "}{" List "}"


// Initiating the check
rule <thread> ... <k> fv(MATCHTYPE:String , TOBECHECKED:HigherProcOrChan , VAR:Id)
                                              => #(VARPAUSE)[MATCHTYPE , TOBECHECKED , VAR] ... </k>
              (.Bag => <FreeVars>
                  <node> [0][ TOBECHECKED ]["base"][ .List ]{ .List }[ false ] </node>
                  <NodeCounter> 1 </NodeCounter>
              </FreeVars>) ... </thread>



// Rules for CONSTRUCTING the tree
// *** Send
// If at the base node, just abstract it up a node. It's easier to work with it this way.
rule <thread> ... <FreeVars> ...
                   <node> [ID:Int][ C:Chan!(P:HigherProcs) ][ "base" ][ (.List
                                   => ListItem(NEWID)) ]{ FREEVARS:List }[ (false
                                   => true) ] </node>
          (.Bag => <node> [NEWID][ C:Chan!(P:HigherProcs) ][ ID ][ .List ]{ .List }[false]  </node>)
                   <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
              ... </FreeVars> ... </thread>

// First go through the list, making a new node for each process in the HigherProcs
rule <thread> ...
        <FreeVars> ...
         <node> [ID:Int][ (C:Chan!(PHead1:HigherProc , PHead2:HigherProc , PTail:HigherProcs)
                      => C:Chan!(PHead2:HigherProc, PTail:HigherProcs)) ][ PARENT:Int ][ (.List
                      => ListItem(NEWID)) ListOfChildren:List ]{ FREEVARS:List }[ false ] </node>
         (.Bag => <node> [NEWID][ PHead1 ][ ID ][ .List ]{ .List }[false]  </node>)
         <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
    ... </FreeVars> ... </thread>

// When we only have one process left
rule <thread> ...
        <FreeVars> ...
          <node> [ID:Int][ C:Chan!(LastProc:HigherProc) ][ PARENT:Int ][ ListOfChildren:List (.List
            => ListItem(NEWID) ListItem(NEWID +Int 1)) ]{ FREEVARS:List }[ (false => true) ] </node>
 (.Bag => <node> [NEWID][ C ][ ID ][ .List ]{ .List }[false]  </node>
          <node> [NEWID +Int 1][ LastProc ][ ID ][ .List ]{ .List }[false]  </node>)
             <NodeCounter> (NEWID:Int => NEWID +Int 2) </NodeCounter>
        ... </FreeVars> ... </thread>

// *** Persistent send: all the same rules as the send, but with the persistent syntax
// If at the base node, just abstract it up a node. It's easier to work with it this way.
rule <thread> ...
        <FreeVars> ...
          <node> [ID:Int][ C:Chan!!(P:HigherProcs) ][ "base" ][ (.List
                                   => ListItem(NEWID)) ]{ FREEVARS:List }[ (false => true) ] </node>
 (.Bag => <node> [NEWID][ C:Chan!(P:HigherProcs) ][ ID ][ .List ]{ .List }[false]  </node>)
         <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
    ... </FreeVars> ... </thread>

// First go through the list, making a new node for each process in the HigherProcs
rule <thread> ...
        <FreeVars> ...
          <node> [ID:Int][ (C:Chan!!(PHead1:HigherProc , PHead2:HigherProc , PTail:HigherProcs)
                        => C:Chan!(PHead2:HigherProc, PTail:HigherProcs)) ][ PARENT:Int ][ (.List
                        => ListItem(NEWID)) ListOfChildren:List ]{ FREEVARS:List }[ false ] </node>
 (.Bag => <node> [NEWID][ PHead1 ][ ID ][ .List ]{ .List }[false]  </node>)
         <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
    ... </FreeVars> ... </thread>

// When we only have one process left
rule <thread> ...
        <FreeVars> ...
          <node> [ID:Int][ C:Chan!!(LastProc:HigherProc) ][ PARENT:Int ][ ListOfChildren:List (.List
            => ListItem(NEWID) ListItem(NEWID +Int 1)) ]{ FREEVARS:List }[ (false => true) ] </node>
         (.Bag => <node> [NEWID][ C ][ ID ][ .List ]{ .List }[false]  </node>
                  <node> [NEWID +Int 1][ LastProc ][ ID ][ .List ]{ .List }[false]  </node>)
         <NodeCounter> (NEWID:Int => NEWID +Int 2) </NodeCounter>
    ... </FreeVars> ... </thread>

// *** Quote
rule <thread> ...
        <FreeVars> ...
          <node> [ID:Int][ @ P:HigherProc ][ PARENT:Exp ][ (.List
                                  => ListItem(NEWID)) ]{ FREEVARS:List }[ (false => true) ] </node>
 (.Bag => <node> [NEWID][ P ][ ID ][ .List ]{ .List }[false]  </node>)
         <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
    ... </FreeVars> ... </thread>
// *** Eval
rule <thread> ...
        <FreeVars> ...
          <node> [ID:Int][ * C:Chan ][ PARENT:Exp ][ (.List
                                  => ListItem(NEWID)) ]{ FREEVARS:List }[ (false
                                  => true) ] </node>
 (.Bag => <node> [NEWID][ C ][ ID ][ .List ]{ .List }[false]  </node>)
         <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
    ... </FreeVars> ... </thread>
// *** Par
rule <thread> ...
        <FreeVars> ...
          <node> [ID:Int][ P1:HigherProc | P2:HigherProc ][ PARENT:Exp ][ (.List
            => ListItem(NEWID) ListItem(NEWID +Int 1)) ]{ FREEVARS:List }[ (false
            => true) ] </node>
         (.Bag => <node> [NEWID][ P1 ][ ID ][ .List ]{ .List }[false]  </node>
                  <node> [NEWID +Int 1][ P2 ][ ID ][ .List ]{ .List }[false]  </node>)
         <NodeCounter> (NEWID:Int => NEWID +Int 2) </NodeCounter>
    ... </FreeVars> ... </thread>

// Rules for binding operations
// Listen
rule <thread> ...
        <FreeVars> ...
                  <node> [ID:Int][ for( C:Chans <- D:Chan ){ P:HigherProc } ][ PARENT:Exp ][ (.List
            => ListItem(NEWID) ListItem(NEWID +Int 1)) ]{ FREEVARS:List }[ (false => true) ] </node>
         (.Bag => <node> [NEWID][ D ][ ID ][ .List ]{ .List }[false]  </node>
                  <bnode> [NEWID +Int 1][ ID ][ .K ; P ][ .List ; C ]{ .List }[false]  </bnode>)
         <NodeCounter> (NEWID:Int => NEWID +Int 2) </NodeCounter>
    ... </FreeVars> ... </thread>
// Persistent Listen
rule <thread> ...
        <FreeVars> ...
                  <node> [ID:Int][ for( C:Chans <= D:Chan ){ P:HigherProc } ][ PARENT:Exp ][ (.List
            => ListItem(NEWID) ListItem(NEWID +Int 1)) ]{ FREEVARS:List }[ (false => true) ] </node>
         (.Bag => <node> [NEWID][ D ][ ID ][ .List ]{ .List }[false]  </node>
                  <bnode> [NEWID +Int 1][ ID ][ .K ; P ][ .List ; C ]{ .List }[false]  </bnode>)
         <NodeCounter> (NEWID:Int => NEWID +Int 2) </NodeCounter>
    ... </FreeVars> ... </thread>
// Contract
rule <thread> ...
        <FreeVars> ...
                <node> [ID:Int][ contract D:Chan (C:Chans) = {P:HigherProc} ][ PARENT:Exp ][ (.List
            => ListItem(NEWID) ListItem(NEWID +Int 1)) ]{ FREEVARS:List }[ (false => true) ] </node>
       (.Bag => <node> [NEWID][ D ][ ID ][ .List ]{ .List }[false]  </node>
                <bnode> [NEWID +Int 1][ ID ][ .K ; P ][ .List ; C ]{ .List }[false]  </bnode>)
         <NodeCounter> (NEWID:Int => NEWID +Int 2) </NodeCounter>
    ... </FreeVars> ... </thread>
// New
rule <thread> ...
        <FreeVars> ...
            <node> [ID:Int][ new X:Ids in { P:HigherProc } ][ PARENT:Exp ][ (.List
                                   => ListItem(NEWID)) ]{ FREEVARS:List }[ (false => true) ] </node>
   (.Bag => <bnode> [NEWID][ ID ][ .K ; P ][ .List ; X ]{ .List }[false]  </bnode>)
         <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
    ... </FreeVars> ... </thread>

// Match
// If it's at the base, make another node so we're not tampering with the base process stored
rule <thread> ...
        <FreeVars> ...
          <node> [ID:Int][ match P:HigherProcOrChan {
                  Q:HigherProcOrChan => { R:HigherProc }
                  Q2:HigherProcOrChan => { R2:HigherProc }
                  HigherMatchCases } ][ "base" ][ (.List
                  => ListItem(NEWID)) ]{ FREEVARS:List }[ (false
                  => true) ] </node>
 (.Bag => <node> [NEWID][ match P:HigherProcOrChan { Q:HigherProcOrChan => { R:HigherProc }
                  Q2:HigherProcOrChan => { R2:HigherProc }
                  HigherMatchCases } ][ ID ][ .List ]{ .List }[ false ]
                  </node>)
         <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
    ... </FreeVars> ... </thread>

// First make all the bnodes. Leave the node's bool at false for now
rule <thread> ...
        <FreeVars> ...
          <node> [ID:Int][ match P:HigherProcOrChan {
                                    Q:HigherProcOrChan => { R:HigherProc }
                                    Q2:HigherProcOrChan => { R2:HigherProc }
                                    HigherMatchCases }
                          ][ PARENT:Int ][ ListOfChildren:List ]{ FREEVARS:List }[ false ]
                          =>
                [ID:Int][ match P:HigherProcOrChan {
                                    Q2:HigherProcOrChan => { R2:HigherProc }
                                    HigherMatchCases }
                        ][ PARENT ][ ListOfChildren:List ListItem(NEWID) ]{ FREEVARS:List }[ false ]
          </node>
         (.Bag => <bnode> [NEWID][ ID ][ .K ; R ][ .List ; Q ]{ .List }[false]  </bnode>)
         <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
    ... </FreeVars> ... </thread>

// When there's just one of the HigherMatchCases left, make the regular node for the process we're
// matching, and change the node's bool to true
rule <thread> ...
        <FreeVars> ...
            <node> [ID:Int][ match P:HigherProcOrChan {
                                    Q:HigherProcOrChan => { R:HigherProc }
                                                  } ][ PARENT:Int ][
                         ListOfChildren:List (.List => ListItem(NEWID) ListItem(NEWID +Int 1)) ]{
                         FREEVARS:List }[ (false => true) ] </node>
   (.Bag => <node> [NEWID][ P ][ ID ][ .List ]{ .List }[false]  </node>
            <bnode> [NEWID +Int 1][ ID ][ .K ; R ][ .List ; Q ]{ .List }[false]  </bnode>)
         <NodeCounter> (NEWID:Int => NEWID +Int 2) </NodeCounter>
    ... </FreeVars> ... </thread>


// Constructing with terminal nodes, which are expressions (including #(Int)'s), as well as Nil
rule <thread> ...
          <FreeVars> ...
              <node> [ID:Int][ E:Exp ][ PARENT:Exp ][ .List ]{ .List }[ (false => true) ] </node>
      ... </FreeVars>
 ... </thread>

// #(Int)'s
rule <thread> ...
          <FreeVars> ...
              <node> [ID:Int][ #(I:Int) ][ PARENT:Exp ][ .List ]{ .List }[ (false => true) ] </node>
      ... </FreeVars>
 ... </thread>

// Nil
rule <thread> ...
          <FreeVars> ...
              <node> [ID:Int][ Nil ][ PARENT:Exp ][ .List ]{ .List }[ (false => true) ] </node>
      ... </FreeVars>
 ... </thread>

// Semantics for variables. When we have a variable at the end of a node, we delete that node and
// add the variable to the list of vars of its parent
rule <thread> ...
      <FreeVars> ...
          <node>
            [ID:Int][ X:Id ][ PARENT:Exp ][ .List ]{ (.List => ListItem(X)) }[ (false => true) ]
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
        [ID:Int][PARENT:Exp][ .K ; PlusProc:HigherProc ][.List ; C:HigherProcsOrChans]{.List}[false]
          => [ID:Int][PARENT:Exp][ NEWID ; .K ][.List ; C:HigherProcsOrChans]{.List}[false] </bnode>
        (.Bag => <node> [NEWID][ PlusProc ][ ID ][ .List ]{ .List }[false]  </node>)
        <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
  ... </FreeVars>
 ... </thread>

// The "minus" branch(es)
rule <thread> ...
      <FreeVars> ...
        <bnode>
           [ID:Int][PARENT:Exp][PlusID:Int ; .K ][ MinusIDs:List ;
           MinusChan1:HigherProcOrChan , MinusChan2:HigherProcOrChan , MinusChans:HigherProcsOrChans
           ]{.List}[false] =>
           [ID:Int][PARENT:Exp][PlusID:Int ; .K ][ MinusIDs:List ListItem(NEWID) ;
           MinusChan2:HigherProcOrChan , MinusChans:HigherProcsOrChans ]{.List}[false]</bnode>
         (.Bag => <node> [NEWID][ MinusChan1 ][ ID ][ .List ]{ .List }[false]  </node>)
         <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
   ... </FreeVars>
 ... </thread>

rule <thread> ...
        <FreeVars> ...
          <bnode> [ID:Int][PARENT:Exp][PlusID:Int ; .K ][ MinusIDs:List ; MinusChan:HigherProcOrChan
                  ]{.List}[false] =>
                  [ID:Int][PARENT:Exp][PlusID:Int ; .K ][ MinusIDs:List ListItem(NEWID) ; .K
                  ]{.List}[false]</bnode>
          (.Bag => <node> [NEWID][ MinusChan ][ ID ][ .List ]{ .List }[false]  </node>)
          <NodeCounter> (NEWID:Int => NEWID +Int 1) </NodeCounter>
    ... </FreeVars>
 ... </thread>


// Once there's nothing left to make nodes off the bnode we change the bnode's bool to "true"
rule <thread> ...
      <FreeVars> ...
        <bnode>
          [ID:Int][PARENT:Exp][PlusID:Int ; .K ][ MinusIDs:List ; .K ]{.List}[(false => true)]
        </bnode> ...
      </FreeVars> ...
    </thread>


// Semantics for DECONSTRUCTING the tree, starting from the ends of the branches, at the terminal
// nodes. When something no longer has any children listed, all of its children have been
// deconstructed. So we deconstruct that node, passing on its list of free vars to its parent
rule <thread> ... <FreeVars> ...
                     (<node> [ID:Int][ ProcOrChan:HigherProcOrChan ][ PARENT:Int ][ .List ]{
                                                              ChildVars:List }[ true ] </node> => .)
                      <node> [PARENT][ParentProcOrChan:HigherProcOrChan][GRANDPARENT:Exp][
                                  ChildrenListHead:List (ListItem(ID) => .) ChildrenListTail:List ]{
                                  (.List => ChildVars) ParentVars:List }[true] </node>
              ... </FreeVars> ... </thread>

// Semantics for deconstructing the "bnode". We need to collect the free variables from the Plus
// branch and then subtract from them the Minus branch. It is precisely because of this step that
// we have to use a tree structure. If we break up a program into its parts, like we do in the
// matching algorithm, and simply go along collecting and erasing free variables from a set,
// we might end up erasing too much. Furthermore, because of variable shadowing, we need to start
// from the innermost scopes and move back outward. These two things make it so it's necessary to
// use a tree in the way that we do.

// bnodes
// Change syntax to simplify things
rule <thread> ... <FreeVars> ...
                      // Include this to ensure that we're only grabbing children of a bnode
                      <bnode> [BnodeID:Int][ParentID:Int][PlusID:Int ; .K ][MinusIDs:List ; .K ]{
                                                                            .List }[ true ] </bnode>
                      // This will initiate when each of the children nodes of bnode has no children
                      <node>  [PlusID][PlusProc][BnodeID:Int][ .List ]{PlusVars:List}[true]
                                                   => [PlusID][BnodeID]{ PlusVars }{ .List } </node>
              ... </FreeVars> ... </thread>

rule <thread> ... <FreeVars> ...
                      // Include this to ensure that we're only grabbing children of a bnode
                      <bnode> [BnodeID:Int][ParentID:Int][PlusID:Int ; .K ][
                              MinusIDHead:List ListItem(MinusID) MinusIDTail:List ; .K
                              ]{ .List }[ true ] </bnode>
                      // This will initiate when each of the children nodes of bnode has no children
                      <node>  [MinusID][MinusChan][BnodeID:Int][ .List ]{ MinusVars:List}[true]
                                           => [MinusID][BnodeID][ .K ]{ MinusVars }{ .List } </node>
              ... </FreeVars> ... </thread>

// Initiate the check
rule <thread> ... <FreeVars> ...
                      <node> [MinusID:Int][BnodeID:Int][ (.K => X) ]{
                                                (ListItem(X) => .) MinusVars:List }{ .List } </node>
              ... </FreeVars> ... </thread>

// Check for repeats
rule (<thread> ...
        <FreeVars> ...
           <node>  [MinusID:Int][BnodeID:Int][ X:Id ]{ ListItem(X) MinusVars }{ MinusVars2 } </node>
    ... </FreeVars> ... </thread> => .)
    <StdOut> (.List
         => ListItem("ERROR: Two instances of one variable in a binder.")) StdOutList:List </StdOut>

// Just in case we missed it
rule (<thread> ...
        <FreeVars> ...
           <node>  [MinusID:Int][BnodeID:Int][ X:Id ]{ MinusVars }{ ListItem(X) MinusVars2 } </node>
    ... </FreeVars> ... </thread> => .)
    <StdOut> (.List
         => ListItem("ERROR: Two instances of one variable in a binder.")) StdOutList:List </StdOut>

// Move over vars that are not equal to X
rule <thread> ...
        <FreeVars> ...
            <node>  [MinusID:Int][BnodeID:Int][ X:Id ]{ (ListItem(Y) => .) MinusVars
                                                      }{ (.List => ListItem(Y)) MinusVars2 } </node>
    ... </FreeVars>
 ... </thread>


// Once the left-hand side is empty, check and delete any of the Plus vars that coincide with
// the variable being checked.
rule <thread> ...
      <FreeVars> ...
        <node>  [MinusID:Int][BnodeID:Int][ X:Id ]{ .List }{ MinusVars2:List } </node>
        <node>  [PlusID:Int][BnodeID:Int]{
                                        (ListItem(X) => .) PlusVars:List }{ PlusVars2:List } </node>
  ... </FreeVars> ... </thread>

// Move the other ones over.
rule <thread> ...
        <FreeVars> ...
            <node>  [MinusID:Int][BnodeID:Int][ X:Id ]{ .List }{ MinusVars2:List } </node>
            <node>  [PlusID:Int][BnodeID:Int]{ (ListItem(Y) => .) PlusVars:List }{
                                                     (.List => ListItem(Y)) PlusVars2:List } </node>
    ... </FreeVars> ... </thread>

// Once the left-hand brackets are empty on both sides, get rid of the var being tested and loop
// back to reinitiate a check.
rule <thread> ...
        <FreeVars> ...
            <node>  [MinusID:Int][BnodeID:Int][ (X:Id => .) ]{
                                             (.List => MinusVars) }{ (MinusVars:List => .) } </node>
            <node>  [PlusID:Int][BnodeID:Int]{ (.List => PlusVars) }{ (PlusVars:List => .) } </node>
    ... </FreeVars> ... </thread>


// Delete the minus nodes when the MinusVars lists are empty
rule <thread> ...
        <FreeVars> ...
          (<node>  [MinusID:Int][BnodeID:Int][ .K ]{ .List }{ .List } </node> => .)
          <bnode> [BnodeID][GrandParentId:Int][ PlusID ; .K ][
                  MinusIDsHead:List (ListItem(MinusID) => .) MinusIDsTail:List ; .K ]{
                  .List }[true] </bnode>
    ... </FreeVars> ... </thread>

// Once the MinusVars list is empty, collect the remaining variables (in PlusVars) as a part of the
// list and keep going. This will collapse the tree entirely, down to the base.
rule <thread> ...
        <FreeVars> ...
          (<node>  [PlusID:Int][BnodeID:Int]{ PlusVars }{ .List } </node> => .)
          (<bnode> [BnodeID][GrandParentId:Int][ PlusID ; .K ][ .List ; .K ]{ .List }[true] </bnode>
        => <node> [BnodeID]["bnode"][GrandParentId][.List]{PlusVars}[true] </node>)
    ... </FreeVars> ... </thread>


// At the end, we rewrite to #(VARPAUSE)[ List of Free Vars ]. Depending on the context, a nonempty
// list of free vars may return an error or be successful.
rule <thread> ...
      (<FreeVars> ...
           <node> [ID:Int][BaseProc:HigherProcOrChan]["base"][.List]{FreeVars:List}[true] </node>
  ... </FreeVars> => .)
           <k> #(VARPAUSE)[ MATCHTYPE:String , BaseProc , Z:Id ]
                          =>  #(VARPAUSE)[ MATCHTYPE:String , BaseProc , Z:Id ][ FreeVars ] ... </k>
 ... </thread>


endmodule
