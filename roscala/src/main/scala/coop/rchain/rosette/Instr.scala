package coop.rchain.rosette

// Covers 16-bit args; may want 64-bit ones eventually
case class Instr(meta: Ob, parent: Ob, opcode: Op, args: List[Int]) extends Ob
