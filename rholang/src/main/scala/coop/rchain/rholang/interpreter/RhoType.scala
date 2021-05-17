package coop.rchain.rholang.interpreter
import com.google.protobuf.ByteString
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.GUnforgeable.UnfInstance.{
  GDeployerIdBody,
  GPrivateBody,
  GSysAuthTokenBody
}
import coop.rchain.models.{ETuple, Expr, GDeployerId, GPrivate, GSysAuthToken, GUnforgeable, Par}
import coop.rchain.shared.ByteStringOps._

object RhoType {
  import coop.rchain.models.rholang.implicits._

  type RhoNil = Nil.type
  object Nil {
    def unapply(p: Par): Boolean = p.isNil()
    def apply(): Par             = Par()
  }
  type RhoByteArray = ByteArray.type
  object ByteArray {
    def unapply(p: Par): Option[Array[Byte]] =
      p.singleExpr().collect {
        case Expr(GByteArray(bs)) => bs.toByteArray
      }

    def apply(bytes: Array[Byte]): Par =
      Expr(GByteArray(ByteString.copyFrom(bytes)))
  }

  type RhoString = String.type
  object String {
    def unapply(p: Par): Option[String] =
      p.singleExpr().collect {
        case Expr(GString(bs)) => bs
      }

    def apply(s: String): Par = GString(s)
  }

  type RhoBoolean = Boolean.type
  object Boolean {
    def apply(b: Boolean) = Expr(GBool(b))

    def unapply(p: Par): Option[Boolean] =
      p.singleExpr().collect {
        case Expr(GBool(b)) => b
      }
  }

  type RhoNumber = Number.type
  object Number {
    def unapply(p: Par): Option[Long] =
      p.singleExpr().collect {
        case Expr(GInt(v)) => v
      }

    def apply(i: Long): Par = Expr(GInt(i))
  }

  type RhoTuple2 = Tuple2.type
  object Tuple2 {
    def apply(tuple: (Par, Par)): Par = Expr(ETupleBody(ETuple(Vector(tuple._1, tuple._2))))

    def unapply(p: Par): Option[(Par, Par)] =
      p.singleExpr().collect {
        case Expr(ETupleBody(ETuple(Seq(a, b), _, _))) => (a, b)
      }
  }

  type RhoUri = Uri.type
  object Uri {
    def unapply(p: Par): Option[String] =
      p.singleExpr().collect {
        case Expr(GUri(s)) => s
      }

    def apply(s: String): Par = GUri(s)
  }

  type RhoDeployerId = DeployerId.type
  object DeployerId {
    def unapply(p: Par): Option[Array[Byte]] =
      p.singleUnforgeable().collect {
        case GUnforgeable(GDeployerIdBody(id)) => id.publicKey.toByteArray
      }

    def apply(bytes: Array[Byte]): Par = GDeployerId(bytes.toByteString)
  }

  type RhoName = Name.type
  object Name {
    def unapply(p: Par): Option[GPrivate] =
      p.singleUnforgeable().collect {
        case GUnforgeable(GPrivateBody(gprivate)) => gprivate
      }

    def apply(gprivate: GPrivate): Par = GUnforgeable(GPrivateBody(gprivate))
  }

  type RhoUnforgeable = Unforgeable.type
  object Unforgeable {
    def unapply(p: Par): Option[GUnforgeable] = p.singleUnforgeable()

    def apply(unforgeable: GUnforgeable): Par = unforgeable
  }

  type RhoExpression = Expression.type

  object SysAuthToken {
    def unapply(p: Par): Option[GSysAuthToken] =
      p.singleUnforgeable().collect {
        case GUnforgeable(GSysAuthTokenBody(token)) => token
      }

    def apply(token: GSysAuthToken): Par = GUnforgeable(GSysAuthTokenBody(token))
  }

  object Expression {
    def unapply(p: Par): Option[Expr] = p.singleExpr()

    def apply(expr: Expr): Par = expr
  }

  sealed abstract class Extractor[RhoType] {
    type ScalaType
    def unapply(p: Par): Option[ScalaType]
  }

  object Extractor {
    def derive[RhoType, Aux](implicit ev: Extractor[RhoType] { type ScalaType = Aux }) = ev

    implicit object BooleanExtractor extends Extractor[Boolean.type] {
      override type ScalaType = Boolean
      override def unapply(p: Par) = Boolean.unapply(p)
    }
    implicit object ByteArrayExtractor extends Extractor[ByteArray.type] {
      override type ScalaType = Array[Byte]
      override def unapply(p: Par) = ByteArray.unapply(p)
    }
    implicit object DeployerIdExtractor extends Extractor[DeployerId.type] {
      override type ScalaType = Array[Byte]
      override def unapply(p: Par) = DeployerId.unapply(p)
    }
    implicit object NameExtractor extends Extractor[Name.type] {
      override type ScalaType = GPrivate
      override def unapply(p: Par) = Name.unapply(p)
    }
    implicit object NilExtractor extends Extractor[Nil.type] {
      override type ScalaType = Unit
      override def unapply(p: Par) = if (Nil.unapply(p)) Some(()) else None
    }
    implicit object NumberExtractor extends Extractor[Number.type] {
      override type ScalaType = Long
      override def unapply(p: Par) = Number.unapply(p)
    }
    implicit object StringExtractor extends Extractor[String.type] {
      override type ScalaType = String
      override def unapply(p: Par) = String.unapply(p)
    }
    implicit object UriExtractor extends Extractor[Uri.type] {
      override type ScalaType = String
      override def unapply(p: Par) = Uri.unapply(p)
    }
    implicit object UnforgeableExtractor extends Extractor[Unforgeable.type] {
      override type ScalaType = GUnforgeable
      override def unapply(p: Par) = Unforgeable.unapply(p)
    }
    implicit object ExpressionExtractor extends Extractor[Expression.type] {
      override type ScalaType = Expr
      override def unapply(p: Par) = Expression.unapply(p)
    }

    implicit def Tuple2Extractor[A, B](
        implicit A: Extractor[A],
        B: Extractor[B]
    ): Extractor[(A, B)] {
      type ScalaType = (A.ScalaType, B.ScalaType)
    } =
      new Extractor[(A, B)] {
        override type ScalaType = (A.ScalaType, B.ScalaType)
        override def unapply(p: Par) =
          for {
            (p1, p2) <- Tuple2.unapply(p)
            a        <- A.unapply(p1)
            b        <- B.unapply(p2)
          } yield (a, b)
      }

    implicit def EitherExtractor[A, B](
        implicit A: Extractor[A],
        B: Extractor[B]
    ): Extractor[Either[A, B]] {
      type ScalaType = Either[A.ScalaType, B.ScalaType]
    } = new Extractor[Either[A, B]] {
      override type ScalaType = Either[A.ScalaType, B.ScalaType]
      override def unapply(p: Par) = B.unapply(p).map(Right(_)).orElse(A.unapply(p).map(Left(_)))
    }
  }
}
