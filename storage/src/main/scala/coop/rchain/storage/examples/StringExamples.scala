package coop.rchain.storage.examples

import java.nio.charset.StandardCharsets

import cats.syntax.either._
import coop.rchain.storage.util._
import coop.rchain.storage.{Match, Serialize}

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
  class StringsCaptor extends ((List[String]) => Unit) with Serializable {

    @transient
    private final lazy val res: ListBuffer[List[String]] = mutable.ListBuffer.empty[List[String]]

    final def results: List[List[String]] = res.toList

    final def apply(v1: List[String]): Unit = ignore(res += v1)
  }

  object implicits {

    implicit object stringMatch extends Match[Pattern, String] {
      def get(p: Pattern, a: String): Option[String] = Some(a).filter(p.isMatch)
    }

    implicit object stringSerialize extends Serialize[String] {

      def encode(a: String): Array[Byte] =
        a.getBytes(StandardCharsets.UTF_8)

      def decode(bytes: Array[Byte]): Either[Throwable, String] =
        Right(new String(bytes, StandardCharsets.UTF_8))
    }

    implicit val stringClosureSerialize: Serialize[StringsCaptor] =
      makeSerializeFromSerializable[StringsCaptor]

    implicit val patternSerialize: Serialize[Pattern] =
      makeSerializeFromSerializable[Pattern]
  }

  /* Convenience Functions */

  /** Runs a given test continuation with given data as its arguments.
    */
  def runK(t: Option[(StringsCaptor, List[String])]): Unit =
    t.foreach { case (k, data) => k(data) }

  def getK(t: Option[(StringsCaptor, List[String])]): StringsCaptor =
    t.map(_._1).get
}
