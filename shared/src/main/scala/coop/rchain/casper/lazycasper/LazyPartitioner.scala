package coop.rchain.casper.lazycasper

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.shared.syntax._

/** Partitioner ensuring that no other overlapping partition can be detected. */
final case class LazyPartitioner[F[_]: Sync, M, S](dag: DagData[F, M, S]) {
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
    * @param baseFringe current fringe to search partition forming above
    * @param bondsMap current final bonds map
    * @param partitionConstraint constraint on partition senders,
    *                            mostly intent here is to enable constraint on cumulative stake.
    * @return First messages for all senders of a partition
    */
  def find(
      baseFringe: Map[S, M]
  )(bondsMap: Map[S, Long], partitionConstraint: Set[S] => Boolean): F[Option[Set[S]]] = {
    // next set witnesses that are matching partition constraint
    val nextOK = next(_: Map[S, M]).map { v =>
      partitionConstraint(v.keySet).guard[Option].as(v).getOrElse(Map())
    }

    // if partition formation is detected - return latest messages in a partition
    val certainPartitionF = for {
      lvl1 <- nextOK(baseFringe)
      lvl2 <- nextOK(lvl1)
      lvl3 <- nextOK(lvl2)
      r <- if (lvl3.isEmpty) none[Set[S]].pure[F]
          else
            (lvl1, lvl2, lvl3).tailRecM {
              case (l1, l2, l3) =>
                val partitionImplied = l2.keySet
                val partitionBonds   = bondsMap.filterKeys(partitionImplied)
                val partitionProvers = Vector(l1, l2, l3).flatMap(_.valuesIterator).distinct

                val isSafeF = Sync[F].delay(isSupermajority(l3.keys, partitionBonds))

                val cannotOverlap = {
                  val sideJustificationsF = partitionProvers
                    .traverse(justificationsF)
                    .map(_.map(_.filter { case (s, _) => partitionImplied.contains(s) }))
                  val allAcrossF  = Sync[F].delay(bondsMap.keySet == partitionImplied)
                  val sameSideJsF = sideJustificationsF.map(_.distinct.size == 1)
                  val lateSideJsF = sideJustificationsF.map(_.forall(isLate(_)(baseFringe, seqNum)))
                  allAcrossF ||^ sameSideJsF ||^ lateSideJsF
                }

                // if safety is not proven yet - proceed with the next layer
                val uncertainCase = nextOK(l3).map { nextLlv =>
                  if (nextLlv.isEmpty) none[Set[S]].asRight[(Map[S, M], Map[S, M], Map[S, M])]
                  else (l2, l3, nextLlv).asLeft[Option[Set[S]]]
                }

                isSafeF.ifM(
                  cannotOverlap.ifM(
                    partitionImplied.some.asRight[(Map[S, M], Map[S, M], Map[S, M])].pure[F],
                    uncertainCase
                  ),
                  none[Set[S]].asRight[(Map[S, M], Map[S, M], Map[S, M])].pure[F]
                )

            }
    } yield r

    certainPartitionF
  }
}
