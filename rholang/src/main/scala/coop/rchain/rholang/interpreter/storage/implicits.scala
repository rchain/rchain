package coop.rchain.rholang.interpreter.storage

import cats.implicits._
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.{Channel, HasLocallyFree, Par}
import coop.rchain.rholang.interpreter.SpatialMatcher._
import coop.rchain.rholang.interpreter.implicits._
import coop.rchain.storage.{Match => StorageMatch}

//noinspection ConvertExpressionToSAM
object implicits {

  private def toChannels(fm: FreeMap, max: Int): List[Channel] =
    (0 until max).map { (i: Int) =>
      fm.get(i) match {
        case Some(par) => Channel(Quote(par))
        case None      => Channel(Quote(Par.defaultInstance))
      }
    }.toList

  private def freeCount(c: Channel): Int = implicitly[HasLocallyFree[Channel]].freeCount(c)

  implicit val matchListQuote: StorageMatch[List[Channel], List[Channel]] =
    new StorageMatch[List[Channel], List[Channel]] {

      def get(patterns: List[Channel], data: List[Channel]): Option[List[Channel]] =
        foldMatch(data, patterns, (t: Channel, p: Channel) => spatialMatch(t, p))
          .runS(emptyMap)
          .map { (freeMap: FreeMap) =>
            toChannels(freeMap, patterns.map((c: Channel) => freeCount(c)).sum)
          }
    }
}
