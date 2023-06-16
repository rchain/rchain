package coop.rchain.models.rholangN

final class BoundVar(val value: Int) extends VarN
object BoundVar { def apply(value: Int): BoundVar = new BoundVar(value) }

final class FreeVar(val value: Int) extends VarN
object FreeVar { def apply(value: Int): FreeVar = new FreeVar(value) }

final class Wildcard() extends VarN
object Wildcard { def apply(): Wildcard = new Wildcard }
