module PROGRAM-RESTRICTIONS
imports ALPHA-EQUIVALENCE

// *******************************************************************
//                          PROGRAM-RESTRICTIONS
// *******************************************************************

// We need to set forth rules about the global structure of a Rholang program.
// We need this both to check the initial program as well as to check the body of executable
// code which appear as other instances of higher processes.

// Syntax inherently takes care of most problems, but checking for globally free variables is
// inherently impossible to verify except at runtime.

//    The program cannot have any globally free variables
//    In the case of other instances of HigherProcesses, before finding a match and rewriting, we
//    check for free variables. We do this for the whole program in the rho.k file in the
//    configuration. The special rules for the fvInitial( ) function are given here.

// Initiating the check
rule <thread> ... <k> fvInitial( PROGRAM:Proc ) => #(INITIALCHECK)[PROGRAM] ... </k>
              (.Bag => <FreeVars>
                  <node> [0][ PROGRAM ]["base"][ .List ]{ .Set }[ false ] </node>
                  <NodeCounter> 1 </NodeCounter>
              </FreeVars>) ... </thread>

// Once the variables have been found, either fail or continue
// Failure: there is a globally free variable
rule <thread> ... <k> (#(INITIALCHECK)[PROGRAM] => Nil) </k>
                  (<FreeVars> ...
                   <node>[0][PROGRAM]["base"][.List]{ SetItem(X:Id) GlobalVars:Set }[true]</node>
                  ... </FreeVars> => .Bag)
          ... </thread>
              <StdOut>
                (.List => #if size(GlobalVars) ==Int 0 #then
                 ListItem("Error: " +String Id2String( X ) +String " is a free variable.")
                #else
                ListItem("Error: There are "
                +String Int2String( (size(GlobalVars) +Int 1) )
                +String " free variables. One of them is "
                +String Id2String( X )
                +String ".")
                #fi) StdOutList:List
              </StdOut>

// Success: there are no globally free variables
rule <thread> ... <k> (#(INITIALCHECK)[PROGRAM:Proc] => PROGRAM) </k>
                  (<FreeVars> ...
                    <node> [ 0 ][ PROGRAM:Proc ][ "base" ][ .List ]{ .Set }[ true ] </node>
                  ... </FreeVars> => .Bag)
          ... </thread>


endmodule
