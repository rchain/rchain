package coop.rchain.rholang.interpreter.storage

import cats.implicits._
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.{Channel, HasLocallyFree, Par}
import coop.rchain.rholang.interpreter.SpatialMatcher._
import coop.rchain.rholang.interpreter.implicits._
import coop.rchain.rspace.{Match => StorageMatch}

//noinspection ConvertExpressionToSAM
object implicits {

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
        foldMatch(data, patterns, (t: Channel, p: Channel) => spatialMatch(t, p))
          .runS(emptyMap)
          .map { (freeMap: FreeMap) =>
            toChannels(freeMap, patterns.map((c: Channel) => freeCount(c)).sum)
          }
    }
}
