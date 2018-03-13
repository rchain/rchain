package coop.rchain.rholang.interpreter.storage

import cats.implicits._
import coop.rchain.models.{Channel, Par}
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.storage.{Match => StorageMatch}
import coop.rchain.rholang.interpreter.SpatialMatcher._

import scala.annotation.tailrec

//noinspection ConvertExpressionToSAM
object implicits {

  implicit def orderingIntTuple[T]: Ordering[(Int, T)] =
    new Ordering[(Int, T)] {

      def compare(x: (Int, T), y: (Int, T)): Int =
        (x, y) match {
          case ((i1, _), (i2, _)) => i1.compare(i2)
        }
    }

  private def toQuotes[T](fm: FreeMap)(implicit ord: Ordering[(Int, Par)]): Option[List[Quote]] = {
    @tailrec
    def loop(sortedParList: List[(Int, Par)], curr: Int, acc: List[Quote]): Option[List[Quote]] =
      sortedParList match {
        case Nil                         => Some(acc)
        case (i, t) :: tail if i == curr => loop(tail, i + 1, Quote(t) :: acc)
        case _                           => None
      }
    loop(fm.toList.sorted, 0, Nil)
  }

  implicit val matchListChannelListQuote: StorageMatch[List[Channel], List[Quote]] =
    new StorageMatch[List[Channel], List[Quote]] {

      def get(patterns: List[Channel], data: List[Quote]): Option[List[Quote]] =
        foldMatch(patterns, data.map(Channel.apply), (t: Channel, p: Channel) => spatialMatch(t, p))
          .runS(emptyMap)
          .flatMap(toQuotes)
    }
}
