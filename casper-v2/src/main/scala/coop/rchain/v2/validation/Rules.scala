package coop.rchain.v2.validation

object Rules {

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
