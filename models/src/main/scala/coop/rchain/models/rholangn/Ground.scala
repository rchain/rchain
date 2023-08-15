package coop.rchain.models.rholangn

final class GBoolN(val v: Boolean) extends GroundN
object GBoolN { def apply(v: Boolean): GBoolN = new GBoolN(v) }

final class GIntN(val v: Long) extends GroundN
object GIntN { def apply(v: Long): GIntN = new GIntN(v) }

final class GBigIntN(val v: BigInt) extends GroundN
object GBigIntN { def apply(v: BigInt): GBigIntN = new GBigIntN(v) }

final class GStringN(val v: String) extends GroundN
object GStringN { def apply(v: String): GStringN = new GStringN(v) }

final class GByteArrayN(val v: Array[Byte]) extends GroundN
object GByteArrayN {
  def apply(bytes: Array[Byte]): GByteArrayN = new GByteArrayN(bytes)
}

final class GUriN(val v: String) extends GroundN
object GUriN { def apply(v: String): GUriN = new GUriN(v) }
