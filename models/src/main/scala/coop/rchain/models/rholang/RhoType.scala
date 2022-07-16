package coop.rchain.models.rholang

import com.google.protobuf.ByteString
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.GUnforgeable.UnfInstance.{
  GDeployIdBody,
  GDeployerIdBody,
  GPrivateBody,
  GSysAuthTokenBody
}
import coop.rchain.models._
import coop.rchain.models.syntax._

object RhoType {
  import coop.rchain.models.rholang.implicits._

  type RhoNil = RhoNil.type
  object RhoNil {
    def unapply(p: Par): Boolean = p.isNil()
    def apply(): Par             = Par()
  }
  type RhoByteArray = RhoByteArray.type
  object RhoByteArray {
    def unapply(p: Par): Option[Array[Byte]] =
      p.singleExpr().collect {
        case Expr(GByteArray(bs)) => bs.toByteArray
      }

    def apply(bytes: Array[Byte]): Par =
      Expr(GByteArray(ByteString.copyFrom(bytes)))
  }

  type RhoString = RhoString.type
  object RhoString {
    def unapply(p: Par): Option[String] =
      p.singleExpr().collect {
        case Expr(GString(bs)) => bs
      }

    def apply(s: String): Par = GString(s)
  }

  type RhoBoolean = RhoBoolean.type
  object RhoBoolean {
    def apply(b: Boolean): Par = Expr(GBool(b))

    def unapply(p: Par): Option[Boolean] =
      p.singleExpr().collect {
        case Expr(GBool(b)) => b
      }
  }

  type RhoNumber = RhoNumber.type
  object RhoNumber {
    def unapply(p: Par): Option[Long] =
      p.singleExpr().collect {
        case Expr(GInt(v)) => v
      }

    def apply(i: Long): Par = Expr(GInt(i))
  }

  type RhoTupleN = RhoTupleN.type
  object RhoTupleN {
    def apply(tuple: Seq[Par]): Par = ETuple(tuple)

    def unapply(p: Par): Option[Seq[Par]] =
      p.singleExpr().collect { case Expr(ETupleBody(ETuple(tuple, _, _))) => tuple }
  }

  type RhoTuple2 = RhoTuple2.type
  object RhoTuple2 {
    def apply(tuple: (Par, Par)): Par = ETuple(Seq(tuple._1, tuple._2))

    def unapply(p: Par): Option[(Par, Par)] =
      RhoTupleN.unapply(p).collect { case Seq(a, b) => (a, b) }
  }

  type RhoTuple3 = RhoTuple3.type
  object RhoTuple3 {
    def apply(tuple: (Par, Par, Par)): Par = ETuple(Seq(tuple._1, tuple._2, tuple._3))

    def unapply(p: Par): Option[(Par, Par, Par)] =
      RhoTupleN.unapply(p).collect { case Seq(a, b, c) => (a, b, c) }
  }

  type RhoTuple4 = RhoTuple4.type
  object RhoTuple4 {
    def apply(tuple: (Par, Par, Par, Par)): Par =
      ETuple(Seq(tuple._1, tuple._2, tuple._3, tuple._4))

    def unapply(p: Par): Option[(Par, Par, Par, Par)] =
      RhoTupleN.unapply(p).collect { case Seq(a, b, c, d) => (a, b, c, d) }
  }

  type RhoUri = RhoUri.type
  object RhoUri {
    def unapply(p: Par): Option[String] =
      p.singleExpr().collect {
        case Expr(GUri(s)) => s
      }

    def apply(s: String): Par = GUri(s)
  }

  type RhoList = RhoList.type
  object RhoList {
    def unapply(p: Par): Option[List[Par]] =
      p.singleExpr().collect {
        case Expr(EListBody(EList(s, _, _, _))) => s.toList
      }

    def apply(s: List[Par]): Par = EListBody(EList(s))
  }

  type RhoSet = RhoSet.type
  object RhoSet {
    def unapply(p: Par): Option[Set[Par]] =
      p.singleExpr().collect {
        case Expr(ESetBody(ParSet(s, _, _, _))) => s.toSet
      }

    def apply(s: Seq[Par]): Par = ESetBody(ParSet(s))
  }

  type RhoMap = RhoMap.type
  object RhoMap {
    def unapply(p: Par): Option[Map[Par, Par]] =
      p.singleExpr().collect {
        case Expr(EMapBody(ParMap(s, _, _, _))) => s.toMap
      }

    def apply(s: Map[Par, Par]): Par = EMapBody(ParMap(s.toSeq))
  }

  type RhoDeployerId = RhoDeployerId.type
  object RhoDeployerId {
    def unapply(p: Par): Option[Array[Byte]] =
      p.singleUnforgeable().collect {
        case GUnforgeable(GDeployerIdBody(id)) => id.publicKey.toByteArray
      }

    def apply(bytes: Array[Byte]): Par = GDeployerId(bytes.toByteString)
  }

  type RhoDeployId = RhoDeployId.type
  object RhoDeployId {
    def unapply(p: Par): Option[Array[Byte]] =
      p.singleUnforgeable().collect {
        case GUnforgeable(GDeployIdBody(id)) => id.sig.toByteArray
      }

    def apply(bytes: Array[Byte]): Par = GDeployId(bytes.toByteString)
  }

  type RhoName = RhoName.type
  object RhoName {
    def unapply(p: Par): Option[GPrivate] =
      p.singleUnforgeable().collect {
        case GUnforgeable(GPrivateBody(gprivate)) => gprivate
      }

    def apply(gprivate: GPrivate): Par         = GUnforgeable(GPrivateBody(gprivate))
    def apply(gprivateBytes: Array[Byte]): Par = apply(ByteString.copyFrom(gprivateBytes))
    def apply(gprivateBytes: ByteString): Par  = apply(GPrivate(gprivateBytes))
  }

  type RhoUnforgeable = RhoUnforgeable.type
  object RhoUnforgeable {
    def unapply(p: Par): Option[GUnforgeable] = p.singleUnforgeable()

    def apply(unforgeable: GUnforgeable): Par = unforgeable
  }

  type RhoExpression = RhoExpression.type
  object RhoExpression {
    def unapply(p: Par): Option[Expr] = p.singleExpr()

    def apply(expr: Expr): Par = expr
  }

  object SysAuthToken {
    def unapply(p: Par): Option[GSysAuthToken] =
      p.singleUnforgeable().collect {
        case GUnforgeable(GSysAuthTokenBody(token)) => token
      }

    def apply(token: GSysAuthToken): Par = GUnforgeable(GSysAuthTokenBody(token))
  }

  sealed abstract class Extractor[RhoType] {
    type ScalaType
    def unapply(p: Par): Option[ScalaType]
  }

  object Extractor {
    def derive[RhoType, Aux](implicit ev: Extractor[RhoType] { type ScalaType = Aux }) = ev

    implicit object BooleanExtractor extends Extractor[RhoBoolean.type] {
      override type ScalaType = Boolean
      override def unapply(p: Par) = RhoBoolean.unapply(p)
    }
    implicit object ByteArrayExtractor extends Extractor[RhoByteArray.type] {
      override type ScalaType = Array[Byte]
      override def unapply(p: Par) = RhoByteArray.unapply(p)
    }
    implicit object DeployerIdExtractor extends Extractor[RhoDeployerId.type] {
      override type ScalaType = Array[Byte]
      override def unapply(p: Par) = RhoDeployerId.unapply(p)
    }
    implicit object NameExtractor extends Extractor[RhoName.type] {
      override type ScalaType = GPrivate
      override def unapply(p: Par) = RhoName.unapply(p)
    }
    implicit object NilExtractor extends Extractor[RhoNil.type] {
      override type ScalaType = Unit
      override def unapply(p: Par) = if (RhoNil.unapply(p)) Some(()) else None
    }
    implicit object NumberExtractor extends Extractor[RhoNumber.type] {
      override type ScalaType = Long
      override def unapply(p: Par) = RhoNumber.unapply(p)
    }
    implicit object StringExtractor extends Extractor[RhoString.type] {
      override type ScalaType = String
      override def unapply(p: Par) = RhoString.unapply(p)
    }
    implicit object UriExtractor extends Extractor[RhoUri.type] {
      override type ScalaType = String
      override def unapply(p: Par) = RhoUri.unapply(p)
    }
    implicit object UnforgeableExtractor extends Extractor[RhoUnforgeable.type] {
      override type ScalaType = GUnforgeable
      override def unapply(p: Par) = RhoUnforgeable.unapply(p)
    }
    implicit object ExpressionExtractor extends Extractor[RhoExpression.type] {
      override type ScalaType = Expr
      override def unapply(p: Par) = RhoExpression.unapply(p)
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
            (p1, p2) <- RhoTuple2.unapply(p)
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
