module SKIR-SYNTAX
  syntax Zero ::= "0"
  syntax Par ::= "|"
  syntax In ::= "in"
  syntax Out ::= "out"
  syntax Ref ::= "&"
  syntax Deref ::= "*"
  syntax SComb ::= "s"
  syntax KComb ::= "k"
  syntax IComb ::= "i"
  syntax Comb ::= Zero | Par | In | Out | Ref | Deref | SComb | KComb | IComb
  syntax Apply ::= "(" Term Term ")"
  syntax Term ::= Id | Comb | Apply // Id is only for convenience in testing
  syntax Empty ::= "empty"
endmodule

module SKIR
  imports SKIR-SYNTAX
  
  configuration 
    <T>
      <k multiplicity="*"> $PGM:Term </k>
    </T>
    
  rule _:Zero => .Bag
  
  rule (i X:Term) => X [anywhere]
  rule ((k X:Term) Y:Term) => X [anywhere]
  rule (((s X:Term) Y:Term) Z:Term) => ((X Z) (Y Z)) [anywhere]

  rule <k> ((| T:Term) U:Term) </k> => <k> T </k> <k> U </k>

  rule <k> ((in  (& P:Term)) Q:Term) </k> <k> ((out (& P)) R:Term) </k>
    => <k> (Q (& R)) </k>
    
  // This is just the previous rule but with Ids for convenience
  rule <k> ((in  N:Id) Q:Term) </k> <k> ((out N) R:Term) </k>
    => <k> (Q (& R)) </k>

  rule (* (& P)) => P
endmodule
