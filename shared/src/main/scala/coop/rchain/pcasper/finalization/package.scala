package coop.rchain.pcasper

import cats.Applicative
import cats.effect.Sync
import cats.syntax.all._

package object finalization {

  // Fringe going all across boded senders, or level of a partition
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

  def closestMessages[F[_]: Sync, M, S](
      base: Map[S, M]
  )(witnessesF: M => F[Map[S, M]]): F[Map[S, M]] =
    base.toVector
      .traverse { case (s, m) => witnessesF(m).map(_.find { case (s1, _) => s1 == s }) }
      .map(_.flatten.toMap)

  def isLate[F[_]: Applicative, M, S](
      message: M
  )(boundary: Map[S, M], seqNum: M => Long, justificationsF: M => F[Map[S, M]]): F[Boolean] =
    justificationsF(message).map { jss =>
      jss.mapValues(seqNum).forall { case (s, vSn) => vSn < seqNum(boundary(s)) }
    }
}
