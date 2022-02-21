package coop.rchain.pcasper.finalization.`lazy`

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.pcasper.finalization._
import coop.rchain.shared.syntax._

/**
  * Partitioner ensuring two properties:
  * 1. No other overlapping partition can be detected.
  * 2. Partition senders match supplied criteria.
  */
final case class LazyPartitioner[F[_]: Sync, M, S](
    dag: DagData[F, M, S],
    bondsMap: Map[S, Long],
    partitionConstraint: Set[S] => Boolean
) extends Partitioner[F, M, S] {
  import dag._

  /** Next set witnesses given current set witnesses. */
  private def next(cur: Map[S, M]): F[Map[S, M]] = {
    val nextWitsMap: F[Map[S, Map[S, M]]] = {
      cur.toList.traverse { case (s, m) => witnessesF(m).map(s -> _) }.map(_.toMap)
    }

    nextWitsMap.map { witMaps =>
      // next biggest partition for each message - senders of all witnesses of the message.
      val nextSendersMap = witMaps.map { case (s, witMap) => s -> witMap.keySet }
      // possible partitions that messages can be part of
      val partitionOptions = Vector(Set.empty[S]) ++ nextSendersMap.map {
        case (s, wits) => wits.filter(w => nextSendersMap.get(w).exists(_.contains(s)))
      }
      // maximum possible partition detected
      val nextPartition = partitionOptions.maxBy(_.size)
      // views on the next level inside partition from members of a partition
      val partitionWits = witMaps.map { case (_, wits) => wits.filterKeys(nextPartition.contains) }
      // Highest witnesses is the next level
      highestMessages(partitionWits)(seqNum)
    }
  }

  /**
    * Check whether partition can be declared.
    * @param base current fringe to search partition forming above
    * @param bondsMap current final bonds map
    * @param partitionConstraint constraint on partition senders,
    *                            mostly intent here is to enable constraint on cumulative stake.
    * @return First messages for all senders of a partition
    */
  override def findPartition(base: Fringe[M, S]): F[Option[Fringe[M, S]]] = {
    // next set witnesses that are matching partition constraint
    val nextOK = next(_: Map[S, M]).map { v =>
      partitionConstraint(v.keySet).guard[Option].as(v).getOrElse(Map())
    }

    val certainPartitionF = for {
      lvl1 <- nextOK(base)
      lvl2 <- nextOK(lvl1)
      r <- if (lvl2.isEmpty || lvl1.isEmpty) none[Set[S]].pure[F]
          else
            (lvl1, lvl2).tailRecM {
              case (l1, l2) =>
                // TODO add optimisation when l1 is all across nad l2 is supermajority

                val partitionImplied = l2.keySet

                val settledF = Sync[F].delay(l1.keySet == l2.keySet)

                val isSafeF = {
                  val partitionBonds = bondsMap.filterKeys(partitionImplied)
                  nextOK(l2).map(l3 => isSupermajority(l3.keys, partitionBonds))
                }

                val cannotOverlapF = {
                  val partitionProvers = Vector(l1, l2).flatMap(_.valuesIterator).distinct
                  val sideJustificationsF = partitionProvers
                    .traverse(justificationsF)
                    .map(_.map(_.filterNot { case (s, _) => partitionImplied.contains(s) }))
                  val sameSideJsF = sideJustificationsF.map(_.distinct.size == 1)
                  val lateSideJsF = sideJustificationsF.map(_.forall(isLate(_)(base, seqNum)))
                  sameSideJsF ||^ lateSideJsF
                }

                val loadNextLevelF = nextOK(l2).map { nextLvl =>
                  if (nextLvl.isEmpty) none[Set[S]].asRight[(Map[S, M], Map[S, M])]
                  else (l2, nextLvl).asLeft[Option[Set[S]]]
                }

                (settledF &&^ isSafeF &&^ cannotOverlapF).ifM(
                  partitionImplied.some.asRight[(Map[S, M], Map[S, M])].pure[F],
                  loadNextLevelF
                )
            }
    } yield r

    // First level of a partition
    def firstLevel(partition: Set[S]): F[Fringe[M, S]] =
      closestMessages(base.filterKeys(partition).filterKeys(partition))(witnessesF)

    certainPartitionF.flatMap(pOpt => pOpt.traverse(firstLevel))
  }
}
