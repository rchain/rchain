package coop.rchain.casper.sim

import cats.Id
import cats.syntax.all._
import coop.rchain.casper.lazycasper.LazyCasper.FinalityState
import coop.rchain.casper.lazycasper.{DagData, Fringe, LazyCasper}
import coop.rchain.casper.sim.Simulation.{Msg, Sender}

/**
  * A high level logic for local finalization.
  */
object LazyFinalization {
  def run(
      finalBondsMap: Map[Sender, Long], // full bonds map from final state
      mView: Set[Msg],                  // view of the message replayed
      realFringes: List[Fringe[Msg, Sender]],
      witnessF: Msg => Map[Sender, Msg],
      justificationsF: Msg => Map[Sender, Msg]
  ): (Vector[Fringe[Msg, Sender]], Vector[Fringe[Msg, Sender]]) = {
    val genesisView = finalBondsMap.map {
      case (s, _) => s -> Msg("g", 0, Sender(0, 1), -1, Map(), Map())
    }
    // This sum of maps to make sure view includes all senders
    val view      = genesisView ++ mView.map(m => (m.sender, m))
    val jsF       = genesisView ++ justificationsF(_: Msg)
    val curFringe = realFringes.last

    val dag = new DagData[Id, Msg, Sender] {
      override def witnessesF: Msg => Id[Map[
        Sender,
        Msg
      ]]                                                        = witnessF
      override def justificationsF: Msg => Id[Map[Sender, Msg]] = jsF
      override def seqNum: Msg => Long                          = _.senderSeq.toLong
      override def sender: Msg => Sender                        = _.sender
    }

    import coop.rchain.catscontrib.effect.implicits._
    val newFinState = LazyCasper.finalise[Id, Msg, Sender](FinalityState(curFringe, curFringe))(
      finalBondsMap,
      view,
      dag,
      _ => ()
    )

    val newCompleteFringe = newFinState.completeFringe

    val advancement = (newCompleteFringe != curFringe).guard[Option].as(newCompleteFringe)

    (advancement.map(Vector(_)).getOrElse(Vector()), Vector.empty[Fringe[Msg, Sender]])
  }
}
