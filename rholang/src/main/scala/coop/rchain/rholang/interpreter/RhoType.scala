package coop.rchain.rholang.interpreter
import com.google.protobuf.ByteString
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.GUnforgeable.UnfInstance.GDeployerIdBody
import coop.rchain.models.{ETuple, Expr, GUnforgeable, Par}

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

    def apply(i: Long): Par = Expr(GInt(i))
  }

  object Tuple2 {
    def apply(tuple: (Par, Par)): Par = Expr(ETupleBody(ETuple(Seq(tuple._1, tuple._2))))

    def unapply(p: Par): Option[(Par, Par)] =
      p.singleExpr().collect {
        case Expr(ETupleBody(ETuple(Seq(a, b), _, _))) => (a, b)
      }
  }

  object Uri {
    def unapply(p: Par): Option[String] =
      p.singleExpr().collect {
        case Expr(GUri(s)) => s
      }

    def apply(s: String): Par = GUri(s)
  }

  object GDeployerId {
    def unapply(p: Par): Option[Array[Byte]] =
      p.singleUnforgeable().collect {
        case GUnforgeable(GDeployerIdBody(id)) => id.publicKey.toByteArray
      }

    def apply(bytes: Array[Byte]): Par = GDeployerId(bytes)
  }
}
