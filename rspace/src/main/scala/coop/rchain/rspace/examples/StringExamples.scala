package coop.rchain.rspace.examples

import java.nio.charset.StandardCharsets

import cats.Monoid
import coop.rchain.rspace.Match.MatchResult
import coop.rchain.shared.Language.ignore
import coop.rchain.rspace.{Match, Serialize}
import scodec.bits.ByteVector

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Seq

object StringExamples {

  /** An example pattern type
    */
  sealed trait Pattern extends Product with Serializable {

    def isMatch(a: Any): Boolean =
      this match {
        case Wildcard           => true
        case StringMatch(value) => value == a
      }
  }
  final case class StringMatch(value: String) extends Pattern
  case object Wildcard                        extends Pattern

  /** An example continuation type
    *
    * It captures the data it consumes.
    */
  class StringsCaptor extends ((Seq[String]) => Unit) with Serializable {

    @transient
    private final lazy val res: ListBuffer[Seq[String]] = mutable.ListBuffer.empty[Seq[String]]

    final def results: Seq[Seq[String]] = res.toList

    final def apply(v1: Seq[String]): Unit = ignore(res += v1)

    override def hashCode(): Int =
      res.hashCode() * 37

    override def equals(obj: scala.Any): Boolean = obj match {
      case sc: StringsCaptor => sc.res == res
      case _                 => false
    }
  }

  object implicits {
    implicit object nullMonoid extends Monoid[Null] {
      override def empty: Null                     = null
      override def combine(x: Null, y: Null): Null = null
    }

    implicit object integerAdditionMonoid extends Monoid[Int] {
      override def empty: Int                   = 0
      override def combine(x: Int, y: Int): Int = x + y
    }

    implicit object stringMatch extends Match[Pattern, Nothing, String, Null, String] {
      def get(p: Pattern, a: String): MatchResult[String, Null, Nothing] =
        MatchResult.fromEither(Right(Some(a).filter(p.isMatch)), null)
    }

    implicit object stringSerialize extends Serialize[String] {

      def encode(a: String): ByteVector =
        ByteVector.view(a.getBytes(StandardCharsets.UTF_8))

      def decode(bytes: ByteVector): Either[Throwable, String] =
        Right(new String(bytes.toArray, StandardCharsets.UTF_8))
    }

    implicit val stringClosureSerialize: Serialize[StringsCaptor] =
      makeSerializeFromSerializable[StringsCaptor]

    implicit val patternSerialize: Serialize[Pattern] =
      makeSerializeFromSerializable[Pattern]
  }
}
