module SK-SYNTAX
  syntax Comb ::= "s" | "k" | "i"
  syntax Apply ::= "(" Term Term ")" [function]
  syntax Term ::= Id | Comb | Apply
  syntax KResult ::= Comb
endmodule

module SK
  imports SK-SYNTAX

  rule (i X:Term) => X
  rule ((k X:Term) Y:Term) => X
  rule (((s X:Term) Y:Term) Z:Term) => ((X Z) (Y Z))

endmodule
