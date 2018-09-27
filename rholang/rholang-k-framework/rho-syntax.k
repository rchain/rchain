require "substitution.k"

module RHO-SYNTAX
imports SUBSTITUTION
imports DOMAINS-SYNTAX

// We need processes, including variables
syntax Proc ::= ProcNoVars
              | Id

// We need processes, excluding variables
syntax ProcNoVars ::=
              // Ground terms, including the empty process and expressions
                Ground
              // Listen
              | "for" "(" NamePats "<-" Name ")" "{" Proc "}" [binder(1 -> 3)]
              // Persistent listen
              | "for" "(" NamePats "<=" Name ")" "{" Proc "}" [binder(1 -> 3)]
              | "contract" Name "(" NamePats ")" "=" "{" Proc "}" [binder(2 -> 3)]
              // Send
              | Name "!" "(" Procs ")"
              // Persistent send
              | Name "!!" "(" Procs ")"
              // Evaluate
              | "*" Name
              // New
              | "new" Ids "in" "{" Proc "}" [binder(1 -> 2)]
              // Match
              | "match" Proc "{" MatchCases "}"
              // Parallel
              | Proc "|" Proc [left,ndbParallelProcesses]
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

// Names and Processes, used specifically for -arity matching
syntax Names ::= List{ Name, "," }
            | Ids
syntax Procs ::= List{ Proc, "," }
            | Ids
            | Ints
syntax NamePats ::= List{ NamePat, "," }
            | Names
            | Vars
syntax ProcPats ::= List{ ProcPat, "," }
            | Procs
            | Vars
syntax Ids   ::= List{Id, ","}
syntax Vars  ::= List{Var, ","}
            | Ids

// A list for matching parallel processes
syntax ParMatchList ::= List{ HigherProc, ";"}


// Syntax for both name and process patterns
syntax Pat ::=
            // A name pattern
              NamePat
            // A process pattern
            | ProcPat

syntax NamePat ::=
            // Either a variable (incl. wildcards)
              Var
            // Or a Name Pattern
            | NamePatNoVars
            // Or it could just be a name
            | Name

syntax ProcPat ::=
            // Either a variable (incl. wildcards)
              Var
            // Or a Process Pattern
            | ProcPatNoVars
            // Or it could just be Proc
            | Proc
            // logical "and"
            | ProcPat "/\\" ProcPat [left]
            // logical "or"
            > ProcPat "\\/" ProcPat [left]

// For matching, we need to make a distinction between variables and no variables
syntax NamePatNoVars ::=
            // A name pattern can be a standard name
              NameNoVars
            // It can also be a name with the more general version of variables
            | "@" ProcPat

syntax ProcPatNoVars ::=
            // A process pattern can be a standard process
              ProcNoVars
            // It can also be a process with the more general version of variables
            | "for" "(" NamePats "<-" NamePat ")" "{" ProcPat "}" [binder(1 -> 3)]
            // Persistent listen
            | "for" "(" NamePats "<=" NamePat ")" "{" ProcPat "}" [binder(1 -> 3)]
            | "contract" NamePat "(" NamePats ")" "=" "{" ProcPat "}" [binder(2 -> 3)]
            // Send
            | NamePat "!" "(" ProcPats ")"
            // Persistent send
            | NamePat "!!" "(" ProcPats ")"
            // Evaluate
            | "*" NamePat
            // New
            | "new" Ids "in" "{" ProcPat "}" [binder(1 -> 2)]
            // Match
            | "match" ProcPat "{" MatchCasesPat "}"
            // Parallel
            | ProcPat "|" ProcPat [left,ndbParallelProcesses]
            // Bracket
            | "{" ProcPat "}" [bracket]


// Syntax that will show up b/c of the new construct,
// but that cannot be written (unforgeable things)

// We need higher processes, including variables
syntax HigherProc ::= HigherProcNoVars
              | ProcPat
              | Id

// Higher Names and Hiher Processes, specifically for sending and listening for tuples
syntax HigherNames ::= List{ HigherName, "," }
              | NamePats
              | Names
syntax HigherNamePats ::= List{ HigherNamePat, "," }
              | HigherNames
syntax HigherProcs ::= List{ HigherProc, "," }
              | Ids
              | ProcPats
              | Procs
syntax HigherProcPats ::= List{ HigherProcPat, "," }
              | HigherProcs
syntax HigherPats ::= List{ HigherPat, "," }
              | HigherNamePats
              | HigherProcPats
              | HigherProcsOrHigherNames

// We need higher processes, excluding variables
syntax HigherProcNoVars ::=
                "for" "(" HigherNamePats "<-" HigherName ")" "{" HigherProc "}" [binder(1 -> 3)]
              | "for" "(" HigherNamePats "<=" HigherName ")" "{" HigherProc "}" [binder(1 -> 3)]
              | "contract" HigherName "(" HigherNamePats ")" "=" "{" HigherProc "}" [binder(2 -> 3)]
              | HigherName "!" "(" HigherProcs ")"
              | HigherName "!!" "(" HigherProcs ")"
              | "*" HigherName
              | "new" Ids "in" "{" HigherProc "}" [binder(1 -> 2)]
              | "match" HigherProc "{" HigherMatchCases "}"
              | ProcPatNoVars
              // Syntax for new names--the whole reason for "Higher" anything
              | "unforgeable(" Int ")"
              | HigherProc "|" HigherProc [left]
              | "{" HigherProc "}" [bracket]

// The general idea of a HigherName can be a (bound) variable. It can also
// contain logical connectives. We need to distinguish between non-variable
// terms, with no logical connectives, etc and those that are or have them.
syntax HigherName ::= HigherNameNoVars
              | NamePat
              | Id

// Patterns for HigherProcs and HigherNames
// Syntax for both name and process patterns
syntax HigherPat ::=
            // A name pattern
              HigherNamePat
            // A process pattern
            | HigherProcPat
            // Or either, with no vars
            | HigherPatNoVars
            // To include any instances of HigherProcOrHigherName
            | HigherProcOrHigherName

syntax HigherPatNoVars ::=
              HigherNamePatNoVars
            // A process pattern
            | HigherProcPatNoVars


syntax HigherNamePat ::=
            // It could just be a variable (or wildcard)
              Var
            // A name pattern can be a standard name
            | HigherNamePatNoVars
            // Or it could be a HigherName
            | HigherName

// For matching, we need to make a distinction between variables and no variables
syntax HigherNamePatNoVars ::=
            // A name pattern can be a standard name
              HigherNameNoVars
            // It can also be a name with the more general version of variables
            | "@" HigherProcPat

syntax HigherProcPat ::=
            // It could just be a variable (or wildcard)
              Var
            // A name pattern can be a standard name
            | HigherProcPatNoVars
            // Or it could be a HigherProc
            | HigherProc
            // logical "and"
            | HigherProcPat "/\\" HigherProcPat [left]
            // logical "or"
            > HigherProcPat "\\/" HigherProcPat [left]

syntax HigherProcPatNoVars ::=
            // A name pattern can be a standard name
              HigherProcNoVars
            // It can also be a process with the more general version of variables
            | "for" "(" HigherNamePats "<-" HigherNamePat ")" "{" HigherProcPat "}" [binder(1 -> 3)]
            // Persistent listen
            | "for" "(" HigherNamePats "<=" HigherNamePat ")" "{" HigherProcPat "}" [binder(1 -> 3)]
            | "contract" HigherNamePat "(" HigherNamePats ")" "=" "{" HigherProcPat "}"
                                                                                    [binder(2 -> 3)]
            // Send
            | HigherNamePat "!" "(" HigherProcPats ")"
            // Persistent send
            | HigherNamePat "!!" "(" HigherProcPats ")"
            // Evaluate
            | "*" HigherNamePat
            // New
            | "new" Ids "in" "{" HigherProcPat "}" [binder(1 -> 2)]
            // Match
            | "match" HigherProcPat "{" HigherMatchCasesPat "}"
            // Syntax for new names--the whole reason for "Higher" anything
            | "unforgeable(" Int ")"
            // Parallel
            | HigherProcPat "|" HigherProcPat [left,ndbParallelProcesses]
            // Bracket
            | "{" HigherProcPat "}" [bracket]



// We need HigherNames, with no variables
syntax HigherNameNoVars ::= "@" HigherProc
              | NamePatNoVars

syntax Exp  ::= Int
              | Bool
              | String

syntax KVariable ::= Id
syntax KResult ::= Int
              | String StringlessMatchFormSyntax
              | HigherNameNoVars
              | Id
              | Bool

// For exact text matching
syntax String ::= "toString(" HigherPat ")"


// In Rholang we distinguish between variables and wildcards
syntax Var ::=
            // Standard variable
              Id
            // Wildcard
            | Wildcard

syntax Wildcard ::= "_"

syntax Fun  ::=
              // General matching functions which use the "matching engine" below
                "match" "[" String "]" "(" HigherPat ";" HigherPat ")"
              | "aritymatch" "[" String "]" "(" HigherProcPats ";" HigherNamePats ")"
              | "matchstdform" "[" String "]" "(" CanBeStdMatched ";" CanBeStdMatched ")"
                                                                                       [strict(2,3)]
              | "matchstdform" "[" String "]" "(" CanBeStdMatched ";" CanBeStdMatched ";" Bool ")"
                                                                                         [strict(4)]
              // Functions for matching parallel processes
              | "toparmatchstdform" "(" HigherPat ")"
              | "fromparmatchstdform" "(" HigherPat ")"
              // Errors and pauses, dependent on the match function we're using
              | "#(MATCHFAIL)[" String "]"
              | "#(MATCHPAUSE)[" String "]"
              | "#(ORPAUSE)[" String "]"
              | "#(MATCHFAIL)[" String "," Int "]"
              | "#(PARPAUSE)[" String "," Int "]"
              | "#(PARMATCHRESET)[" Int "]"
              | "#(MATCHPAUSE)"
              // For variable matching
              | "#(VARPAUSE)[" String "," HigherPat "," Id "]"
              | "#(VARPAUSE)[" String "," HigherPat "," Id "]{" Set "}"
              | "#(VARFAIL)"
              // For checking the entire program
              | "#(INITIALCHECK)[" HigherProc "]"
              | "#(INITIALCHECK)"
              // A function to find free variables
              | "fv(" String "," HigherPat "," Id ")"
              // For determining if the entire program has free variables in it
              | "fvInitial(" HigherPat ")"
              // A function to determine if two names are equivalent
              | "areNameEquivalent(" HigherNamePat "," HigherNamePat ")"
              // A function to determine alpha equivalence
              | "areAlphaEquivalent(" HigherPat "," HigherPat ")"

// Some syntax specifically for the "match" process
syntax MatchCase  ::= ProcPat "=>" "{" Proc "}" [binder]
syntax MatchCases ::= List{MatchCase, " "}
// The same but for Higher Processes
syntax HigherMatchCase  ::= HigherProcPat "=>" "{" HigherProc "}" [binder]
            | MatchCasePat
syntax HigherMatchCases ::= List{HigherMatchCase, " "}
            | MatchCasesPat
// The same but for patterns
syntax MatchCasePat  ::= ProcPat "=>" "{" ProcPat "}" [binder]
            | MatchCase
syntax MatchCasesPat ::= List{MatchCasePat, " "}
            | MatchCases

// The same but for Higher Processes
syntax HigherMatchCasePat  ::= HigherProcPat "=>" "{" HigherProcPat "}" [binder]
            | HigherMatchCase
syntax HigherMatchCasesPat ::= List{HigherMatchCasePat, " "}
            | HigherMatchCases

// *******************************************************************
//       Syntax specifically for the SENDING-RECEIVING module
// *******************************************************************

// Some syntax for the data we need to keep on our in and out cells
syntax InOutData ::= Int ";" Set

// *******************************************************************
//       Syntax specifically for the MATCHING-FUNCTION module
// *******************************************************************

// Some needed syntax for matching
// This distinguishes between Processes and Names with and without variables
syntax ProcOrName ::= Id
              | ProcOrNameNoVars
              | Name
              | Proc

syntax HigherProcOrHigherName ::= Id
              | HigherProcOrHigherNameNoVars
              | HigherName
              | HigherProc

// For matching, we need to make a distinction between variables and no variables
syntax ProcOrNameNoVars ::= NameNoVars
              | ProcNoVars

syntax HigherProcOrHigherNameNoVars ::= HigherNameNoVars
              | HigherProcNoVars

syntax StringlessMatchFormSyntax ::=
              // Quote, Eval and Var
                "[" HigherPat "]"
              // Listen and Persistent Listen, as well as Contracts
              | "[" HigherNamePat "][" HigherNamePat "][" HigherProcPat "]"
              // New
              | "[" Ids "][" HigherProcPat "]"
              // Send
              | "[" HigherNamePat "][" HigherProcPat "]"
              // Match
              | "[" HigherProcPat "][" HigherMatchCasesPat "]"
              | "[" HigherProcPat "][" HigherProcPat "]"
              // Logical And's and Or's
              | "[" String "][" HigherPat "][" HigherPat "]"
              // Nil
              | "[" "]"
              // Hashed new HigherNames
              | "[" Int "]"
              | "[" HigherProcPat "][" HigherProcPat "]"

// Syntax which is needed for various matching operations
syntax CanBeStdMatched ::= Id
              | "intostdmatchform(" HigherPat ")"
              | "intostdtfmatchform(" HigherPat ")"
              | String StringlessMatchFormSyntax
              | Ground
              | HigherNameNoVars
              | KResult

// *******************************************************************
//        Syntax specifically for the MATCHING-WITH-PAR module
// *******************************************************************

// Some syntax specifically for matching with "|"

// When matching against the pattern "par" the matching process becomes a little more complicated
syntax PatternMatchingCells ::=
   "[" String "][" HigherProcPat "][" HigherProcPat "][" HigherProcPat "][" Int "]["
                                                                           HigherProcPat "]{"Set"}"
 | "[" HigherProcPat "][" HigherProcPat "][" HigherProcPat "][" HigherProcPat "][" HigherProcPat "]"
     // For variable matching at the end
 | "[" HigherProcPat "][" HigherProcPat "][" HigherProcPat "][" HigherProcPat "]["
                                                                  HigherProcPat "]{"HigherProcPat"}"

// *******************************************************************
//           Syntax specifically for the FREE module
// *******************************************************************

// First we construct the syntax for our nodes
syntax Ints ::= List{ Int, ","}

syntax Set ::= "List2Set(" List ")"
syntax List ::= "Set2List(" Set ")"

syntax HigherProcsOrHigherNames ::= List{HigherProcOrHigherName, ","}
              | HigherProcs
              | HigherNames

// Sets and Lists
syntax SetOrList ::= Set
              | List

syntax NodeSyntax ::=
            // For normal nodes, the syntax is
            //[ Id ][ Proc/Name ][ Parent ][ Children ]{ Set of free vars }[ Node constructed ]
             "[" Int "][" HigherPat "][" Exp "][" List "]{" SetOrList "}[" Bool "]"
            // Nodes corresponding to binders (bnodes) are given the syntax
            // [ Id ][ Parent ][ Plus ID ; Plus Process/Name ][ Minus ID ; Minus Process/Name
            //                                        ]{ Set of free vars }[ Node constructed (T/F)]
          | "[" Int "][" Exp "][" Int ";" HigherPats "][" List ";" HigherPats "]{" SetOrList "}[" Bool "]"
            // We need to make sure that a binder doesn't try to bind two variables twice. To do
            // this, we put a special marker on those elements corresponding to a binder
          //| "[" Int "][" HigherPat "][" Exp "][" List "]{" SetOrList "}{" List "}{""}[" Bool "]"

endmodule
