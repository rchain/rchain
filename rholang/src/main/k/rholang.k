module RHO-SYNTAX
imports SUBSTITUTION

// Syntax for processes
syntax Proc ::=
              // Empty process
                "Nil"
              // Listen
              | "for(" Name "<-" Name "){" Proc "}" [binder]
              // Persistent listen
              | "for(" Name "<=" Name "){" Proc "}" [binder]
              | "contract" Name "(" Name ")" "=" "{" Proc "}" [binder]
              // Send
              | Name "!(" Proc ")"
              // Evaluate
              | "*" Name
              // New
              | "new" Id "in" "{" Proc "}" [binder]
              // Parallel
              | Proc "|" Proc [left]
              // Misc
              | Exp

// Syntax for Names; we allow variables to take the place of names, but note
// that no rholang program will compile if there are free variables left in
// the program. Thus the variable must be bound to a name of the form @Proc
syntax Name ::= "@" Proc
              | Id


// We have to add Higher Processes (HigherProc) as well as Chan (Channels— this
// word choice is perhaps unfortunate, as in rholang channels and names are the
// same). Thus far in the K-framework we will allow channels to be names, but also
// the channels which spawn from the "new" construct. The higher processes are
// processes which allow channels to be substituted into the places that names
// used to be--i.e. the syntax supports the "new" channel construct.
//
// You'll note that in the configuration below, only programs of the syntactic
// category Proc can be written; this means that one cannot write the syntax that
// K gives for new channels, even though the syntax is considered correct. This
// is to make the "new" channels unforgeable.
syntax HigherProc ::=
                "for(" Chan "<-" Chan "){" HigherProc "}" [binder]
              | "for(" Chan "<=" Chan "){" HigherProc "}" [binder]
              | "contract" Chan "(" Chan ")" "=" "{" HigherProc "}" [binder]
              | Chan "!(" HigherProc ")"
              | "*" Chan
              | "new" Id "in" "{" HigherProc "}" [binder]
              | Proc
              | "#(" Int ")"
              | HigherProc "|" HigherProc [left] // should this be changed to a different category? Procs?
              | "{" HigherProc "}" [bracket]

syntax Chan ::= "@" HigherProc
              | Name

syntax Exp  ::= AExp
              | String

syntax AExp ::= Int | Id
              | AExp "+" AExp [strict,left]
              | AExp "-" AExp [strict,left]
              | AExp "*" AExp [strict,left]
              | AExp "/" AExp [strict,left]
              | AExp AExp [left]
              | "{" AExp "}" [bracket]

// KVariable and KResult defined to make strictness properties work fine.
syntax KVariable ::= Id
syntax KResult ::= Int

endmodule
module RHO
imports RHO-SYNTAX
// Configuration
// Right now we're only using k-cells, but we may switch to using <in /> and
// <out /> cells, where the <in /> cells contain listening processes, and the
// <out /> cells contain sending processes.
configuration <T color="yellow">
                <k multiplicity="*" color="red"> $PGM:Proc </k>
                <NewChannelCounter color="purple"> 0 </NewChannelCounter>
              </T>


// PROCESSES
// Nil process
rule Nil => .

// The semantics for send/receive is being developed. This is the very basic
// version which only receives a channel. There needs to be a pattern-matching
// construct here, which I have and is almost functional. Will update when it is.

// This will also support logical connectives.

// Send/receive, waiting for a channel
rule <k> C:Chan !(P:HigherProc) => . </k>
     <k> for(X:Id <- C:Chan){ S:HigherProc } => S[ @P / X ] </k> [sendreceive]

// Persistent send/receive, waiting for a channel
rule <k> C:Chan !(P:HigherProc) => . </k>
     <k> for(X:Id <= C:Chan){ S:HigherProc } </k>
     (.Bag => <k> S[ @P / X ] </k>) [sendreceive]

// Contracts (persistent send/receive rewritten)
rule <k> contract C:Chan(X:Id) = { P:HigherProc } => for(X <= C){ P } </k>


// Evaluation and Quotation: Inverses
// In rholang, * and @ are inverses, although that is not
// true in RHO calculus.
rule <k> ... * @ P:HigherProc  => P ... </k>
rule <k> ... @ * C:Chan  => C ... </k>

// Parallel processes
rule <k> ... P1:HigherProc | P2:HigherProc => P1 ... </k>
     (.Bag => <k> P2 </k>)

// New construct
// It would be better here to have the semantics #(!N:Int), but the
// version of K supported by the K VM available online doesn't support
// this yet; in the final version we'll do it with !N:Int and get rid
// of the NewChannelCounter cell.
rule <k> new X:Id in { P:HigherProc } => P[@ #(I:Int) / X] </k>
     <NewChannelCounter> I => I +Int 1 </NewChannelCounter> [newchannel]

// Delete empty k-cells
rule <k> .K </k> => .

// Delete k-cells which have only an expression in them
rule <k> E:Exp </k> => .

endmodule
