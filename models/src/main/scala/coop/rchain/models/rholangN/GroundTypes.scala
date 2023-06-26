package coop.rchain.models.rholangN

final class GNilN() extends ParN
object GNilN { def apply(): GNilN = new GNilN }

final class GBoolN(val v: Boolean) extends ExprN
object GBoolN { def apply(v: Boolean): GBoolN = new GBoolN(v) }

final class GIntN(val v: Long) extends ExprN
object GIntN { def apply(v: Long): GIntN = new GIntN(v) }

final class GBigIntN(val v: BigInt) extends ExprN
object GBigIntN { def apply(v: BigInt): GBigIntN = new GBigIntN(v) }

final class GStringN(val v: String) extends ExprN
object GStringN { def apply(v: String): GStringN = new GStringN(v) }
