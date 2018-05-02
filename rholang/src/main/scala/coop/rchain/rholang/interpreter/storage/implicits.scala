package coop.rchain.rholang.interpreter.storage

import cats.implicits._
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models._
import coop.rchain.models.implicits.mkProtobufInstance
import coop.rchain.rholang.interpreter.HasLocallyFree
import coop.rchain.rholang.interpreter.SpatialMatcher._
import coop.rchain.rholang.interpreter.implicits._
import coop.rchain.rspace.{Serialize, Match => StorageMatch}

//noinspection ConvertExpressionToSAM
object implicits {

  /* Match instance */

  private def toChannels(fm: FreeMap, max: Int): Seq[Channel] =
    (0 until max).map { (i: Int) =>
      fm.get(i) match {
        case Some(par) => Channel(Quote(par))
        case None      => Channel(Quote(Par.defaultInstance))
      }
    }

  private def freeCount(c: Channel): Int = implicitly[HasLocallyFree[Channel]].freeCount(c)

  implicit val matchListQuote: StorageMatch[Seq[Channel], Seq[Channel]] =
    new StorageMatch[Seq[Channel], Seq[Channel]] {

      def get(patterns: Seq[Channel], data: Seq[Channel]): Option[Seq[Channel]] =
        foldMatch(data, patterns)
          .runS(emptyMap)
          .map { (freeMap: FreeMap) =>
            toChannels(freeMap, patterns.map((c: Channel) => freeCount(c)).sum)
          }
    }

  /* Serialize instances */

  implicit val serializeChannel: Serialize[Channel] =
    mkProtobufInstance(Channel)

  implicit val serializeChannels: Serialize[Seq[Channel]] =
    new Serialize[Seq[Channel]] {

      override def encode(a: Seq[Channel]): Array[Byte] =
        ListChannel.toByteArray(ListChannel(a))

      override def decode(bytes: Array[Byte]): Either[Throwable, Seq[Channel]] =
        Either.catchNonFatal(ListChannel.parseFrom(bytes).channels.toList)
    }

  implicit val serializeTaggedContinuation: Serialize[TaggedContinuation] =
    mkProtobufInstance(TaggedContinuation)
}
