module YR-SYNTAX
  syntax Zero ::= "0"
  syntax In ::= "d" "(" Name "," Name "," Name ")"
              | "k" "(" Name ")"
              | "fw" "(" Name "," Name ")"
              | "br" "(" Name "," Name ")"
              | "bl" "(" Name "," Name ")"
              | "s" "(" Name "," Name "," Name ")"
              | "*" Name
  syntax Out ::= "m" "(" Name "," Term ")"
  syntax Term ::= Zero | In | Out
  syntax Terms ::= List{Term, "|"}
  syntax Name ::= Id | "@" "{" Terms "}"
endmodule

module YR
  imports YR-SYNTAX

  configuration <T> 
    <ins>
      <in multiplicity="*"> . </in>
    </ins>
    <outs>
      <out multiplicity="*"> . </out>
    </outs>
    <k> $PGM:Terms </k> 
  </T>

  rule <k> _:Zero | Ts:Terms => Ts </k>

  rule
    <k> I:In | Ts:Terms => Ts </k>
    <ins> (Is) => (Is <in> I </in>) </ins>

  rule
    <k> O:Out | Ts:Terms => Ts </k>
    <outs> (Os) => (Os <out> O </out>) </outs>

  rule
      <in> d(A, B, C) => . </in>
      <out> m(A, P) => . </out>
      <k> .Terms => (m(B, P) | m(C, P)) </k>

  rule
      <in> k(A) => . </in>
      <out> m(A, P) => . </out>

  rule
      <in> fw(A, B) => . </in>
      <out> m(A, P) => m(B, P) </out>

  rule
      <in> br(A, B) => fw(B, @{P}) </in>
      <out> m(A, P) => . </out>

  rule
      <in> bl(A, B) => fw(@{P}, B) </in>
      <out> m(A, P) => . </out>

  rule
      <in> s(A, B, C) => fw(B, C) </in>
      <out> m(A, P) => . </out>

  rule
      <in> *A => P </in>
      <out> m(A, P) => . </out>

  rule <in> . </in> => .Bag

  rule <out> . </out> => .Bag
endmodule
