package coop.rchain.rosette

import scala.collection.mutable

// Covers 16-bit args; may want 64-bit ones eventually
case class Instr(opcode: Op,
                 args: List[Int],
                 override val _slot: mutable.Seq[Ob])
    extends Ob
