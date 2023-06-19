package coop.rchain.models.rholangN

final class BoundVarN(val value: Int) extends VarN
object BoundVarN { def apply(value: Int): BoundVarN = new BoundVarN(value) }

final class FreeVarN(val value: Int) extends VarN
object FreeVarN { def apply(value: Int): FreeVarN = new FreeVarN(value) }

final class WildcardN() extends VarN
object WildcardN { def apply(): WildcardN = new WildcardN }
