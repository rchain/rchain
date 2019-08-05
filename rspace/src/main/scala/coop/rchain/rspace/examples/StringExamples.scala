package coop.rchain.rspace.examples

import java.nio.charset.StandardCharsets

import cats.effect.Sync
import coop.rchain.metrics.Span.TraceId
import coop.rchain.shared.Language.ignore
import coop.rchain.rspace.{Match, Serialize}
import scodec.bits.ByteVector

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
  @SuppressWarnings(Array("org.wartremover.warts.MutableDataStructures"))
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

    implicit def stringMatch[F[_]: Sync]: Match[F, Pattern, String] =
      new Match[F, Pattern, String] {
        override def get(p: Pattern, a: String)(implicit traceId: TraceId): F[Option[String]] =
          Sync[F].pure(Some(a).filter(p.isMatch))
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
