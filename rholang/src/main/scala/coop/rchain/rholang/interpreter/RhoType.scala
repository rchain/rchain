package coop.rchain.rholang.interpreter
import com.google.protobuf.ByteString
import coop.rchain.models.Expr.ExprInstance.{ETupleBody, GBool, GByteArray, GInt, GString}
import coop.rchain.models.{ETuple, Expr, Par}

object RhoType {
  import coop.rchain.models.rholang.implicits._

  object ByteArray {
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

  object Boolean {
    def apply(b: Boolean) = Expr(GBool(b))

    def unapply(p: Par): Option[Boolean] =
      p.singleExpr().collect {
        case Expr(GBool(b)) => b
      }
  }

  object Number {
    def unapply(p: Par): Option[Long] =
      p.singleExpr().collect {
        case Expr(GInt(v)) => v
      }
  }

  object Tuple2 {
    def apply(tuple: (Par, Par)): Par = Expr(ETupleBody(ETuple(Seq(tuple._1, tuple._2))))

    def unapply(p: Par): Option[(Par, Par)] =
      p.singleExpr().collect {
        case Expr(ETupleBody(ETuple(Seq(a,b), _, _))) => (a,b)
      }
  }
}
