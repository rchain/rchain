package coop.rchain.casper.pcasper
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.pcasper.Finalizer.FinalityDecision
import coop.rchain.casper.pcasper.Fringe.Fringe

/**
  * Finalizer searches for safe messages against some view on top of current fringe.
  */
final case class Finalizer[F[_]: Sync, M, S](view: Map[S, M], curFringe: Fringe[M, S]) {

  /** Find messages above current fringe that can be finalized as part of some partition. */
  def run(
      witnessesF: M => F[Map[S, M]],     // lowest messages from all senders that have input in the view
      justificationsF: M => F[Map[S, M]] // justifications of the message
  )(seqNum: M => Long, sender: M => S): F[Map[S, FinalityDecision[M, S]]] = {

    // Witnesses that are in the view
    val witnessesInViewF = witnessesF(_: M).map(_.filter {
      case (s, m) =>
        val latestInView = view.get(s)
        assert(
          latestInView.isDefined,
          s"Sender $s is not present in latest messages defining the view $view."
        )
        seqNum(m) <= seqNum(latestInView.get)
    }.toMap)

    // Targets for finalization
    val targetsF = curFringe.toList
      .traverse { case (s, m) => witnessesInViewF(m).map(_.find { case (s1, _) => s1 == s }) }
      .map(_.flatten)

    // Check whether the message is safe
    val check = SafetyOracle.run[F, M, S](_: M)(witnessesInViewF, justificationsF)(sender)

    targetsF
      .flatMap(_.traverse { case v @ (_, m) => check(m).map(v -> _) })
      .map(_.collect { case (v, Some(partition)) => v -> partition })
      .map { result =>
        assert(
          result.map { case ((_, message), _) => message }.distinct.size == result.size,
          s"Finalizer should output only unique messages but result is: \n $result."
        )
        result.map { case ((s, m), partition) => (s, FinalityDecision(m, partition)) }.toMap
      }
  }
}

object Finalizer {

  /** Message is final against partition S. */
  final case class FinalityDecision[M, S](msg: M, partition: Set[S])
}
