package coop.rchain.casper.v2.core
import cats.effect.Sync
import fs2.Pipe
import fs2.Stream

object Validation {
  trait Offence
  trait SlashableOffence extends Offence
  final case class Slashing[M](message: M, offence: SlashableOffence)

  /** Message should have justifications for validators mentioned in message scope:
    * all slashed, all bonded. */
  case object IncompleteJustification

  /**
    * Sender created message which does not have previous message in justification.
    *    *   *   *
    *    | /   /
    *    |/  /
    *    * *
    *    \/
    *    *
    *    1   2   3
    */
  case object OrderEquivocation

  /**
    * Sender created message which does not agree on self justification.
    *
    */
  case object StateEquivocation
//
//  /**
//    * Sequence equivocation is creation of two messages with the same sequence number.
//    * From state standpoint it is safe as seq equivocating messages will be merged, but still violation of the protocol
//    * adn should be slashing offence.
//    * */
//  def collectSequenceEquivocators[F[_]: Sync, M, S](
//      seqNum: M => Int,
//      sender: M => S
//  ): Pipe[F, M, (M, (Int, M))] =
//    _.zipWithScan(Map.empty[S, Map[Int, Set[M]]]) {
//      case (acc, m) =>
//        val sn     = seqNum(m)
//        val s      = sender(m)
//        val newVal = Sync[F].pure()
//        acc
//          .get(s)
//          .map { seqNumMap =>
//            val newVal = seqNumMap.get(sn).map(_ + m).getOrElse(Set())
//            seqNumMap.updated(sn, newVal)
//          }
//          .getOrElse(Map(s -> Map(sn -> m)))
//        acc.updated(s, newVal)
//    }.compile.lastOrError
//
//  def justificationRegression[F[_], M, S](
//      justifications: List[M],
//      selfJJustifications: F[Set[M]],
//      seqNum: M => Int,
//      sender: M => S
//  )

}
