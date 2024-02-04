package coop.rchain.models.rholangn

final class BoundVarN(val idx: Int) extends VarN
object BoundVarN { def apply(value: Int): BoundVarN = new BoundVarN(value) }

final class FreeVarN(val idx: Int) extends VarN
object FreeVarN { def apply(value: Int): FreeVarN = new FreeVarN(value) }

object WildcardN extends VarN
