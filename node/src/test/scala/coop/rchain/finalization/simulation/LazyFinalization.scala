package coop.rchain.finalization.simulation

import cats.Id
import coop.rchain.finalization.simulation.Simulation.{Msg, MsgView, Sender}
import coop.rchain.pcasper.finalization.`lazy`.{DagData, LazyFinal, LazyFinalizer}

/**
  * A high level logic for local finalization.
  */
object LazyFinalization {
  def run(
      sender: Sender,
      minGenJs: Set[MsgView],
      finalBondsMap: Map[Sender, Long], // full bonds map from final state
      mView: Set[Msg],                  // view of the message replayed
      witnessF: Msg => Map[Sender, Msg],
      justificationsF: Msg => Map[Sender, Msg]
  ): LazyFinal[Msg, Sender] = {
    val genesisView = finalBondsMap.map {
      case (s, _) => s -> Msg("g", 0, Sender(0, 1), -1, Map(), Map())
    }
    // This sum of maps to make sure view includes all senders
    val view = genesisView ++ mView.map(m => (m.sender, m))
    val jsF  = genesisView ++ justificationsF(_: Msg)

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
    LazyFinalizer[Id, Msg, Sender, Msg](
      sender,
      view,
      finalBondsMap,
      dag
    ).computeFinal(minGenJs.map(_.finalized))
  }
}
