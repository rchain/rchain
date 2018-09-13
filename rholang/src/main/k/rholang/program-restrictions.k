module PROGRAM-RESTRICTIONS
imports FREE

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
rule <thread> ... <k> fvInitial( PROGRAM:HigherProc ) => #(INITIALCHECK)[ PROGRAM ] ... </k>
              (.Bag => <FreeVars>
                  <node> [0][ PROGRAM ]["base"][ .List ]{ .List }[ false ] </node>
                  <NodeCounter> 1 </NodeCounter>
              </FreeVars>) ... </thread>

// Once the variables have been found, either fail or continue
// Failure: there is a globally free variable
rule <thread> ... <k> (#(INITIALCHECK)[PROGRAM:HigherProc] => .) </k>
                  (<FreeVars> ...
                    <node>[0][PROGRAM]["base"][.List]{ ListItem(X:Id) GlobalVars:List }[true]</node>
                  ... </FreeVars> => .)
          ... </thread>
              <StdOut>
                (.List => ListItem("Error: Globally free variable.")) StdOutList:List
              </StdOut>

// Success: there are no globally free variables
rule <thread> ... <k> #(INITIALCHECK)[PROGRAM:HigherProc] => PROGRAM </k>
                  (<FreeVars> ...
                    <node>[0][PROGRAM]["base"][.List]{ .List }[true]</node>
                  ... </FreeVars> => .)
          ... </thread>


// 3. The program cannot be a list of processes
//    A list of processes is not a HigherProcess, so this is taken care of by the semantics.

// 4. The program cannot be a tuple of programs
//    A tuple of processes is not a HigherProcess, so this is taken care of by the semantics.


endmodule
