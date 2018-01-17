package coop.rchain.rosette

case class PC(relative: Int) {
  //def fetch(): Instr = {}
}

object PC {
  object PLACEHOLDER extends PC(0)
}
