package coop.rchain.blockstorage.casper

trait SafetyOracle2[F[_], M, S] {

  /** Whether source can propagate agreement on a target. */
  def compatible(source: M, target: M): Boolean

  /** Stake bonded as per view of the message. */
  def bondsMap(message: M): Map[S, Long]
}

object SafetyOracle2 {
  // Todo gradual FT not implemented yet, for now its just > 2/3 for finalization
  val FAULT_TOLERANCE_MIN: Float = -1
  val FAULT_TOLERANCE_MAX: Float = 1

  /** Agreement of the source message on target message. */
  final case class Agreement[M](source: M, target: M)

  /**
    * @return Fault tolerance, given set of agreements.
    */
  def faultTolerance[F[_], M, S](
      agreements: Set[Agreement[M]],
      bondsMap: M => Map[S, Long],
      sender: M => S
  ): Float = {
    val targets               = agreements.map(_.target)
    val multipleTargetsErrMsg = s"Multiple targets when computing fault tolerance."
    require(targets.size == 1, multipleTargetsErrMsg)
    val targetBondsMap = bondsMap(targets.head)
    val stakeAgreed = agreements.toList.map {
      case Agreement(source, _) => targetBondsMap.getOrElse(sender(source), 0L)
    }.sum
    // TODO clique oracle should be adjusted, for now if 67% agree => finalized
    // CliqueOracle.normalizedFaultTolerance(agreements.head.target.block, dag)
    if (stakeAgreed * 3 >= targetBondsMap.values.sum * 2) 1f else -1f
  }
}
