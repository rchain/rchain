module RHO-SYNTAX
  syntax Zero = "0"
  syntax Input = "for" "(" Var "<-" Var ")" "{" Proc "}"
  syntax Output = Var "!" Proc
  syntax Deref = "*" Var
  syntax Atomic = Zero | Input | Output | Deref
  syntax Proc = List{Atomic, "|"}
endmodule

module RHO
  imports RHO-SYNTAX
  
  syntax Cont = "Cont" Hash Var "." Proc
  syntax Data = "Data" Hash Proc
  
  configuration 
    <T>
      <k multiplicity="*"> $PGM:Term </k>
    </T>

  rule <k> ((| T:Term) U:Term) </k> => <k> T </k> <k> U </k>
  
  rule _:Zero => .Bag

  rule for ( Y:Var <- X:Var ) { P } =>
endmodule