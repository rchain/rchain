package coop.rchain.v2.casper
import coop.rchain.v2.casper.syntax.all._
import fs2.{Pure, Stream}

trait SafetyOracle[M, S] {

  /**
   * Whether source can propagate agreement on a target.
   */
  def compatible(source: M, target: M): Boolean

  /**
   * Stake bonded as per view of the message.
   */
  def bondsMap(message: M): Map[S, Long]
}

object SafetyOracle {

  /**
   * Agreement of the source message on target message.
   */
  final case class Agreement[M](source: M, target: M)

  // Todo gradual FT not implemented yet, for now its just > 2/3 for finalization
  val FAULT_TOLERANCE_MIN: Float = -1
  val FAULT_TOLERANCE_MAX: Float = 1

  /**
   * Fault tolerance, given set of agreements.
   */
  def faultTolerance[F[_], M, S](agreements: Set[Agreement[M]])(
      bondsMap: M => Map[S, Long],
      sender: M => S
  ): Float = {
    val targets               = agreements.map(_.target)
    val multipleTargetsErrMsg = s"Multiple targets when computing fault tolerance."
    require(targets.size == 1, multipleTargetsErrMsg)
    val target                = targets.head
    val targetBondsMap        = bondsMap(target)
    val selfBond              = targetBondsMap.getOrElse(sender(target), 0L)
    require(selfBond > 0, "Message creator is not bonded in its bonds map")
    val stakeAgreed           = selfBond + agreements.toList.collect {
      case Agreement(source, _) if sender(target) != sender(source) =>
        targetBondsMap.getOrElse(sender(source), 0L)
    }.sum
    // TODO clique oracle should be adjusted, for now if 67% agree => finalized
    // CliqueOracle.normalizedFaultTolerance(agreements.head.target.block, dag)
    if (stakeAgreed * 3 >= targetBondsMap.values.sum * 2) FAULT_TOLERANCE_MAX
    else FAULT_TOLERANCE_MIN
  }

  /**
   * Stream of fault tolerance - result of accumulating agreements.
   *
   * Messages can be repeating, but with growing fault tolerance.
   * Stream preserves the notion of distance from agreeing messages.
   */
  def faultTolerances[M, S](
      agreeingMessages: Set[M],
      so: SafetyOracle[M, S],
      dg: DependencyGraph[M, S]
  )(implicit ordering: Ordering[M]): Stream[Pure, List[(M, Float)]] = {
    import so._
    dg.messagesView(agreeingMessages)
      .mapAccumulate(Map.empty[M, Set[Agreement[M]]]) { (acc, chunk) =>
        val newAcc       = chunk.foldLeft(acc) { case (lvlAcc, (visitor, targets)) =>
          targets.foldLeft(lvlAcc) { (visitorAcc, target) =>
            if (compatible(visitor, target)) {
              visitorAcc.updated(
                target,
                visitorAcc.getOrElse(target, Set()) + Agreement(visitor, target)
              )
            } else visitorAcc
          }
        }
        val chunkTargets = chunk.toList.flatMap { case (_, targets) => targets }
        val out          = newAcc.filterKeys(chunkTargets.contains).map { case (target, agreements) =>
          (target, SafetyOracle.faultTolerance(agreements)(bondsMap, dg.sender))
        }
        (newAcc, out)
      }
      .map { case (_, v) => v.toList.sortBy { case (m, _) => m } }
  }
}
