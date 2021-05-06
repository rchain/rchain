package coop.rchain.rspace.trace

import coop.rchain.rspace.hashing.StableHashProvider._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.examples.StringExamples.{Pattern, StringsCaptor}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.serializers.ScodecSerialize.{RichAttempt, _}
import coop.rchain.rspace.test.ArbitraryInstances._
import coop.rchain.shared.Serialize
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import scodec.codecs.{ignore => cignore, _}

class EventTests extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "A Produce" should "contain the expected hash" in {
    forAll { (channel: String, data: String, persist: Boolean) =>
      val actual = Produce(channel, data, persist)

      val expectedHash =
        Blake2b256Hash.create(
          List(
            Serialize[String].encode(channel),
            Serialize[String].encode(data),
            (cignore(7) ~> bool).encode(persist).map(_.bytes).getUnsafe
          )
        )

      actual.channelsHash shouldBe hash(channel)
      actual.hash shouldBe expectedHash
    }
  }

  "A Consume" should "contain the expected hash" in {
    forAll { (channelPatterns: List[(String, Pattern)], persist: Boolean) =>
      val (channels, patterns) = channelPatterns.unzip

      val continuation = new StringsCaptor

      val actual = Consume(channels, patterns, continuation, persist)

      val encodedChannels = toOrderedByteVectors(channels)
      val encodedPatterns = toOrderedByteVectors(patterns)

      val expectedHash =
        Blake2b256Hash.create(
          encodedChannels ++ encodedPatterns
            ++ List(
              Serialize[StringsCaptor].encode(continuation),
              (cignore(7) ~> bool).encode(persist).map(_.bytes).getUnsafe
            )
        )

      actual.channelsHashes shouldBe encodedChannels.map(Blake2b256Hash.create)
      actual.hash shouldBe expectedHash
    }
  }

  "A Consume hash" should "be same for reordered (channel, pattern) pairs" in {
    forAll { (channelPatterns: List[(String, Pattern)], persist: Boolean) =>
      val (channels, patterns) = channelPatterns.unzip
      val continuation         = new StringsCaptor

      val actual   = Consume(channels, patterns, continuation, persist)
      val reversed = Consume(channels.reverse, patterns.reverse, continuation, persist)

      reversed.hash shouldBe actual.hash
    }
  }
}
