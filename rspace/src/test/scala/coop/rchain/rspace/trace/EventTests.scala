package coop.rchain.rspace.trace

import cats.implicits._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.examples.StringExamples.{Pattern, StringsCaptor}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.test.ArbitraryInstances._
import coop.rchain.rspace.{Blake2b256Hash, Serialize}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs.{ignore => cignore, _}
import scodec.interop.cats._
import scodec.{Attempt, Codec}
import coop.rchain.rspace.util

import scala.collection.immutable.Seq

class EventTests extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "A Produce" should "contain the expected hash" in {
    forAll { (channel: String, data: String, persist: Boolean) =>
      val actual = Produce.create(channel, data, persist)

      val expectedHash =
        Blake2b256Hash.create(
          List(
            Serialize[String].encode(channel),
            Serialize[String].encode(data),
            (cignore(7) ~> bool).encode(persist).map(_.bytes).get
          )
        )

      actual.hash shouldBe expectedHash
    }
  }

  "A Consume" should "contain the expected hash" in {
    forAll { (channelPatterns: List[(String, Pattern)], persist: Boolean) =>
      val (channels, patterns) = channelPatterns.unzip

      val continuation = new StringsCaptor

      val actual = Consume.create(channels, patterns, continuation, persist)

      val (encodedChannels, encodedPatterns) =
        channelPatterns
          .map {
            case (channel, pattern) =>
              (Serialize[String].encode(channel), Serialize[Pattern].encode(pattern))
          }
          .sorted(util.ordByteVectorPair)
          .unzip

      val expectedHash =
        Blake2b256Hash.create(
          encodedChannels ++ encodedPatterns
            ++ List(
              Serialize[StringsCaptor].encode(continuation),
              (cignore(7) ~> bool).encode(persist).map(_.bytes).get
            )
        )

      actual.hash shouldBe expectedHash
    }
  }

  "A Consume hash" should "be same for reordered (channel, pattern) pairs" in {
    forAll { (channelPatterns: List[(String, Pattern)], persist: Boolean) =>
      val (channels, patterns) = channelPatterns.unzip
      val continuation         = new StringsCaptor

      val actual   = Consume.create(channels, patterns, continuation, persist)
      val reversed = Consume.create(channels.reverse, patterns.reverse, continuation, persist)

      reversed.hash shouldBe actual.hash
    }
  }
}
