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
