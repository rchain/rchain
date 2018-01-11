require "substitution.k"

module MINPI-SYNTAX
  imports SUBSTITUTION
  
  syntax Var ::= Id
  syntax Zero ::= "0"
  syntax For ::= "for" "(" Var "<-" Var ")" "{" Terms "}" [binder]
  syntax Send ::= Var "!" Var
  syntax New ::= "new" Var "." "(" Terms ")" [binder]
  syntax Rep ::= "rep" "{" Terms "}"
  syntax Term ::= Zero
                | For
                | Send
                | New
                | Rep
  syntax Terms ::= List{Term, "|"}
  syntax KVariable ::= Var
endmodule

module MINPI
  imports MINPI-SYNTAX
  
  configuration <T> 
    <ins>
      <in multiplicity="*"> . </in>
    </ins>
    <outs>
      <out multiplicity="*"> . </out>
    </outs>
    <k> $PGM:Terms </k> 
  </T>

  rule
    <k> _:Zero | Ts:Terms => Ts </k>

  rule
    <k> F:For | Ts:Terms => Ts </k>
    <ins> (Is) => (Is <in> F </in>) </ins>

  rule
    <k> S:Send | Ts:Terms => Ts </k>
    <outs> (Os) => (Os <out> S </out>) </outs>

  rule
      <in> for (Y <- X) { Ts:Terms } => . </in>
      <out> X ! Z => . </out>
      <k> .Terms => Ts[Z / Y] </k>

endmodule