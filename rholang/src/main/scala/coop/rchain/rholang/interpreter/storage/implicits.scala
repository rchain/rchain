package coop.rchain.rholang.interpreter.storage

import cats.implicits._
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.{Channel, HasLocallyFree, Par}
import coop.rchain.rholang.interpreter.SpatialMatcher._
import coop.rchain.rholang.interpreter.implicits._
import coop.rchain.storage.{Match => StorageMatch}

import scala.collection.immutable.SortedMap

//noinspection ConvertExpressionToSAM
object implicits {

  private def toQuotes(fm: FreeMap, max: Int): List[Quote] =
    (0 until max).map { (i: Int) =>
      fm.get(i) match {
        case Some(par) => Quote(par)
        case None      => Quote(Par.defaultInstance)
      }
    }.toList

  private def freeCount(c: Channel): Int = implicitly[HasLocallyFree[Channel]].freeCount(c)

  implicit val matchListChannelListQuote: StorageMatch[List[Channel], List[Quote]] =
    new StorageMatch[List[Channel], List[Quote]] {

      def get(patterns: List[Channel], data: List[Quote]): Option[List[Quote]] =
        foldMatch(data.map(Channel.apply), patterns, (t: Channel, p: Channel) => spatialMatch(t, p))
          .runS(emptyMap)
          .map { (freeMap: FreeMap) =>
            toQuotes(freeMap, patterns.map((c: Channel) => freeCount(c)).sum)
          }
    }
}
