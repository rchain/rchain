package coop.rchain.rholang.interpreter
import com.google.protobuf.ByteString
import coop.rchain.models.Expr.ExprInstance.{GBool, GByteArray, GString}
import coop.rchain.models.{Expr, Par}

object RhoType {
  object ByteArray {
    import coop.rchain.models.rholang.implicits._
    def unapply(p: Par): Option[Array[Byte]] =
      p.singleExpr().collect {
        case Expr(GByteArray(bs)) => bs.toByteArray
      }

    def apply(bytes: Array[Byte]): Par =
      Expr(GByteArray(ByteString.copyFrom(bytes)))
  }

  object String {
    def unapply(p: Par): Option[String] =
      p.singleExpr().collect {
        case Expr(GString(bs)) => bs
      }

    def apply(s: String): Par = GString(s)
  }

  object Bool {
    def apply(b: Boolean) = Expr(GBool(b))
  }
}
