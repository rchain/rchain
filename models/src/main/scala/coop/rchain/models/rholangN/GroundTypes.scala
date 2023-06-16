package coop.rchain.models.rholangN

final class GNilN() extends ParN
object GNilN { def apply(): GNilN = new GNilN }

final class GIntN(val v: Long) extends ExprN
object GIntN { def apply(v: Long): GIntN = new GIntN(v) }
