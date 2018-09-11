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
              | "for" "(" NamePats "<-" Name ")" "{" Proc "}" [binder(1 -> 3)]
              // Persistent listen
              | "for" "(" NamePats "<=" Name ")" "{" Proc "}" [binder(1 -> 3)]
              | "contract" Name "(" NamePats ")" "=" "{" Proc "}"
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
syntax Procs ::= List{ Proc, "," }
syntax NamePats ::= List{ NamePat, "," }
syntax ProcPats ::= List{ ProcPat, "," }
syntax Ids   ::= List{Id, ","}
syntax Vars  ::= List{Var, ","}
// A list for matching parallel processes
syntax ParMatchList ::= List{ HigherProc, ";"}


// Syntax for both name and process patterns
syntax Pat ::=
            // A name pattern
              NamePat
            // A process pattern
            | ProcPat
            // Or a ProcOrName
            | ProcOrName

syntax NamePat ::=
            // Either a variable (incl. wildcards)
              Var
            // Or a Name Pattern
            | NamePatNoVars
            // Or a Name
            | Name

syntax ProcPat ::=
            // Either a variable (incl. wildcards)
              Var
            // Or a Process Pattern
            | ProcPatNoVars
            // Or a Proc
            | Proc

// For matching, we need to make a distinction between variables and no variables
syntax NamePatNoVars ::=
            // A name pattern can be a standard name
              NameNoVars
            // It can also be a name with the more general version of variables
            | "@" ProcPat

syntax ProcPatNoVars ::=
            // A name pattern can be a standard name
              ProcNoVars
            // It can also be a process with the more general version of variables
            | "for" "(" NamePats "<-" NamePat ")" "{" ProcPat "}" [binder(1 -> 3)]
            // Persistent listen
            | "for" "(" NamePats "<=" NamePat ")" "{" ProcPat "}" [binder(1 -> 3)]
            | "contract" NamePat "(" NamePats ")" "=" "{" ProcPat "}"
            // Send
            | NamePat "!" "(" ProcPats ")"
            // Persistent send
            | NamePat "!!" "(" ProcPats ")"
            // Evaluate
            | "*" NamePat
            // New
            | "new" Ids "in" "{" ProcPat "}" [binder]
            // Match
            | "match" Pat "{" MatchCasesPat "}"
            // Parallel
            | ProcPat "|" ProcPat [left,ndbParallelProcesses]
            // Bracket
            | "{" ProcPat "}" [bracket]
            // logical "and"
            | ProcPat "/\\" ProcPat [left]
            // logical "or"
            > ProcPat "\\/" ProcPat [left]


// Syntax that will show up b/c of the new construct,
// but that cannot be written (unforgeable things)

// We need higher processes, including variables
syntax HigherProc ::= HigherProcNoVars
              | Id

// Higher Names and Hiher Processes, specifically for sending and listening for tuples
syntax HigherNames ::= List{ HigherName, "," }
syntax HigherNamePats ::= List{ HigherNamePat, "," }
              | HigherNames
syntax HigherProcs ::= List{ HigherProc, "," }
syntax HigherProcPats ::= List{ HigherProcPat, "," }
              | HigherProcs
syntax HigherPats ::= List{ HigherPat, "," }

// We need higher processes, excluding variables
syntax HigherProcNoVars ::=
                "for" "(" HigherNamePats "<-" HigherName ")" "{" HigherProc "}" [binder(1 -> 3)]
              | "for" "(" HigherNamePats "<=" HigherName ")" "{" HigherProc "}" [binder(1 -> 3)]
              | "contract" HigherName "(" HigherNamePats ")" "=" "{" HigherProc "}"
              | HigherName "!" "(" HigherProcs ")"
              | HigherName "!!" "(" HigherProcs ")"
              | "*" HigherName
              | "new" Ids "in" "{" HigherProc "}" [binder]
              | "match" HigherProcOrHigherName "{" HigherMatchCases "}"
              | ProcNoVars
              // Syntax for new names--the whole reason for "Higher" anything
              | "#(" Int ")"
              | HigherProc "|" HigherProc [left]
              | "{" HigherProc "}" [bracket]

// The general idea of a HigherName can be a (bound) variable. It can also
// contain logical connectives. We need to distinguish between non-variable
// terms, with no logical connectives, etc and those that are or have them.
syntax HigherName ::= HigherNameNoVars
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
            // Or a HigherName
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
            // Or a HigherProc
            | HigherProc

syntax HigherProcPatNoVars ::=
            // A name pattern can be a standard name
              HigherProcNoVars
            // It can also be a process with the more general version of variables
            | "for" "(" HigherNamePats "<-" HigherNamePat ")" "{" HigherProcPat "}" [binder(1 -> 3)]
            // Persistent listen
            | "for" "(" HigherNamePats "<=" HigherNamePat ")" "{" HigherProcPat "}" [binder(1 -> 3)]
            | "contract" HigherNamePat "(" HigherNamePats ")" "=" "{" HigherProcPat "}"
            // Send
            | HigherNamePat "!" "(" HigherProcPats ")"
            // Persistent send
            | HigherNamePat "!!" "(" HigherProcPats ")"
            // Evaluate
            | "*" HigherNamePat
            // New
            | "new" Ids "in" "{" HigherProcPat "}" [binder]
            // Match
            | "match" HigherPat "{" HigherMatchCasesPat "}"
            // Parallel
            | HigherProcPat "|" HigherProcPat [left,ndbParallelProcesses]
            // Bracket
            | "{" HigherProcPat "}" [bracket]
            // logical "and"
            | HigherProcPat "/\\" HigherProcPat [left]
            // logical "or"
            > HigherProcPat "\\/" HigherProcPat [left]



// We need HigherNames, with no variables
syntax HigherNameNoVars ::= "@" HigherProc
              | NameNoVars

syntax Exp  ::= Int
              | Bool
              | String

syntax KVariable ::= Id
syntax KResult ::= Int
              | String StringlessMatchFormSyntax
              | HigherNameNoVars
              | Id
              | Bool


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
              | "aritymatch" "[" String "]" "(" HigherProcs ";" HigherNames ")"
              | "aritymatch" "[" String "]" "(" HigherProcPats ";" HigherNamePats ")"
              | "matchstdform" "[" String "]" "(" CanBeStdMatched ";" CanBeStdMatched ")"
                                                                                       [strict(2,3)]
              | "matchstdform" "[" String "]" "(" CanBeStdMatched ";" CanBeStdMatched ";" Bool ")"
                                                                                         [strict(4)]
              // Functions for matching parallel processes
              | "toparmatchstdform" "(" HigherProcOrHigherName ")"
              | "fromparmatchstdform" "(" HigherProcOrHigherName ")"
              | "toparmatchstdform" "(" HigherPat ")"
              | "fromparmatchstdform" "(" HigherPat ")"
              // Errors and pauses, dependent on the match function we're using
              | "#(MATCHFAIL)[" String "]"
              | "#(MATCHPAUSE)[" String "]"
              | "#(ORPAUSE)[" String "]"
              | "#(MATCHFAIL)[" String "," Int "]"
              | "#(PARPAUSE)[" String "," Int "]"
              | "#(MATCHPAUSE)"
              // For variable matching
              | "#(VARPAUSE)[" String "," HigherPat "," Id "]"
              | "#(VARPAUSE)[" String "," HigherPat "," Id "][" List "]"
              | "#(VARFAIL)"
              // For checking the entire program
              | "#(INITIALCHECK)[" HigherProc "]"
              // A function to find free variables
              | "fv(" String "," HigherPat "," Id ")"
              // For determining if the entire program has free variables in it
              | "fvInitial(" Proc ")"
              | "fvInitial(" HigherProc ")"

// Some syntax specifically for the "match" process
syntax MatchCase  ::= Pat "=>" "{" Proc "}" [binder]
syntax MatchCases ::= List{MatchCase, " "}
// The same but for Higher Processes
syntax HigherMatchCase  ::= HigherPat "=>" "{" HigherProc "}" [binder]
syntax HigherMatchCases ::= List{HigherMatchCase, " "}
// The same but for patterns
syntax MatchCasePat  ::= Pat "=>" "{" ProcPat "}" [binder]
syntax MatchCasesPat ::= List{MatchCasePat, " "}
// The same but for Higher Processes
syntax HigherMatchCasePat  ::= HigherPat "=>" "{" HigherProcPat "}" [binder]
syntax HigherMatchCasesPat ::= List{HigherMatchCasePat, " "}


endmodule
