package coop.rchain.roscala.ob

import coop.rchain.roscala.Opcode

case class Code(litvec: Seq[Ob], codevec: Seq[Opcode]) extends Ob
