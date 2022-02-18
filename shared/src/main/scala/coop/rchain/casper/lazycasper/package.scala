package coop.rchain.casper
import cats.effect.Sync
import cats.syntax.all._

package object lazycasper {

  /**
    * Partial fringe - map from sender to a message.
    * Represents fringe advancement inside a partition. Unlike Local Casper, fringe message can be at any height.
    */
  type Fringe[M, S] = Map[S, M]

  def isSupermajority[S](senders: Iterable[S], bondsMap: Map[S, Long]): Boolean =
    senders.nonEmpty && {
      val totalStake   = bondsMap.keysIterator.map(bondsMap).sum
      val sendersStake = senders.toIterator.map(bondsMap.getOrElse(_, 0L)).sum
      sendersStake * 3 >= totalStake * 2
    }

  def totalStake[S](bonds: Map[S, Long]): Long = bonds.valuesIterator.sum

  def highestMessages[M, S](views: Iterable[Map[S, M]])(seqNum: M => Long): Map[S, M] =
    views
      .reduceOption { (acc, v) =>
        val higherMsgs = v.filter {
          case (s, m) => seqNum(m) > acc.get(s).map(seqNum).getOrElse(Long.MinValue)
        }
        acc ++ higherMsgs
      }
      .getOrElse(Map())

  def lowestMessages[M, S](views: Iterable[Map[S, M]])(seqNum: M => Long): Map[S, M] =
    views
      .reduceOption { (acc, v) =>
        val higherMsgs = v.filter {
          case (s, m) => seqNum(m) > acc.get(s).map(seqNum).getOrElse(Long.MinValue)
        }
        acc ++ higherMsgs
      }
      .getOrElse(Map())

  def closestMessages[F[_]: Sync, M, S](
      target: Map[S, M]
  )(witnessesF: M => F[Map[S, M]]): F[Map[S, M]] =
    target.toVector
      .traverse { case (s, m) => witnessesF(m).map(_.find { case (s1, _) => s1 == s }) }
      .map(_.flatten.toMap)

  def isLate[M, S](view: Map[S, M])(boundary: Map[S, M], seqNum: M => Long): Boolean =
    view.mapValues(seqNum).forall { case (s, vSn) => vSn < seqNum(boundary(s)) }
}
