package coop.rchain.rholang.interpreter
import com.google.protobuf.ByteString
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.GUnforgeable.UnfInstance.{GDeployerIdBody, GPrivateBody}
import coop.rchain.models.{ETuple, Expr, GDeployerId, GPrivate, GUnforgeable, Par}
import coop.rchain.shared.ByteStringOps._

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

  object DeployerId {
    def unapply(p: Par): Option[Array[Byte]] =
      p.singleUnforgeable().collect {
        case GUnforgeable(GDeployerIdBody(id)) => id.publicKey.toByteArray
      }

    def apply(bytes: Array[Byte]): Par = GDeployerId(bytes.toByteString)
  }

  object Name {
    def unapply(p: Par): Option[GPrivate] =
      p.singleUnforgeable().collect {
        case GUnforgeable(GPrivateBody(gprivate)) => gprivate
      }

    def apply(gprivate: GPrivate): Par = GUnforgeable(GPrivateBody(gprivate))
  }

  object Unforgeable {
    def unapply(p: Par): Option[GUnforgeable] = p.singleUnforgeable()

    def apply(unforgeable: GUnforgeable): Par = unforgeable
  }

  object Expression {
    def unapply(p: Par): Option[Expr] = p.singleExpr()

    def apply(expr: Expr): Par = expr
  }
}
