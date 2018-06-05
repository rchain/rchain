package coop.rchain.rspace.trace

import cats.implicits._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.examples.StringExamples.{Pattern, StringsCaptor}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.test.ArbitraryInstances._
import coop.rchain.rspace.{Blake2b256Hash, Serialize}
import coop.rchain.shared.AttemptOps._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import scodec.bits.BitVector
import scodec.interop.cats._
import scodec.{Attempt, Codec}

import scala.collection.immutable.Seq

class EventTests extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  implicit val codecString: Codec[String]        = implicitly[Serialize[String]].toCodec
  implicit val codecPattern: Codec[Pattern]      = implicitly[Serialize[Pattern]].toCodec
  implicit val codecCaptor: Codec[StringsCaptor] = implicitly[Serialize[StringsCaptor]].toCodec

  "A Produce" should "contain the expected hash" in {
    forAll { (channel: String, data: String) =>
      val actual = Produce(channel, data)

      val expectedHash =
        List(Codec[String].encode(channel), Codec[String].encode(data))
          .sequence[Attempt, BitVector]
          .map { (vectors: List[BitVector]) =>
            Blake2b256Hash.create(vectors.toArray.flatMap(_.toByteArray))
          }
          .get

      actual.hash shouldBe expectedHash
    }
  }

  "A Consume" should "contain the expected hash" in {
    forAll { (channelPatterns: List[(String, Pattern)]) =>
      val (channels, patterns) = channelPatterns.unzip

      val continuation = new StringsCaptor

      val actual = Consume(channels, patterns, continuation)

      val expectedHash =
        List(Codec[Seq[String]].encode(channels),
             Codec[Seq[Pattern]].encode(patterns),
             Codec[StringsCaptor].encode(continuation))
          .sequence[Attempt, BitVector]
          .map { (vectors: List[BitVector]) =>
            Blake2b256Hash.create(vectors.toArray.flatMap(_.toByteArray))
          }
          .get

      actual.hash shouldBe expectedHash
    }
  }
}
