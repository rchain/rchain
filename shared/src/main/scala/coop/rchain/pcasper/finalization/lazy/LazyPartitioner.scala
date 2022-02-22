package coop.rchain.pcasper.finalization.`lazy`

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.pcasper.finalization._
import coop.rchain.shared.syntax._

/**
  * Partitioner ensuring two properties:
  * 1. Given the same base no other overlapping partition can be detected.
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
      step1 <- nextOK(base)
      step2 <- nextOK(step1)
      allAcrossCaseF = Sync[F].delay {
        if (isSupermajority(step2.keys, bondsMap)) bondsMap.keySet.some else none[Set[S]]
      }
      partitionCaseF = (step1, step2).tailRecM {
        case (s1, s2) =>
          nextOK(s2).flatMap { s3 =>
            val partitionImplied = s3.keySet

            val settledF = Sync[F].delay(s1.keySet == s2.keySet && s1.keySet == s3.keySet)

            val cannotOverlapF = {
              val partitionProvers = Vector(s1, s2, s3).flatMap(_.valuesIterator).distinct
              val sideJustificationsF = partitionProvers
                .traverse(justificationsF)
                .map(_.map(_.filterNot { case (s, _) => partitionImplied.contains(s) }))
              val sameSideJsF = sideJustificationsF.map(_.size == 1)
              val lateSideJsF = sideJustificationsF
                .map(highestMessages(_)(seqNum).valuesIterator.toList)
                .flatMap(_.forallM(isLate[F, M, S](_)(base, seqNum, justificationsF)))
              sameSideJsF ||^ lateSideJsF
            }

            val loadNextLevelF = Sync[F].delay {
              if (s3.isEmpty) none[Set[S]].asRight[(Map[S, M], Map[S, M])]
              else (s2, s3).asLeft[Option[Set[S]]]
            }

            val partitionFoundF = partitionImplied.some.asRight[(Map[S, M], Map[S, M])].pure[F]

            (settledF &&^ cannotOverlapF).ifM(partitionFoundF, loadNextLevelF)
          }
      }

      r <- if (step1.isEmpty || step2.isEmpty) none[Set[S]].pure[F]
          else {
            // TODO enable optimisation for all across
            //  now its not done to test more general logic of partition detection
            val allAcross = false // step1.keySet == bondsMap.keySet
            if (allAcross) allAcrossCaseF else partitionCaseF
          }
    } yield r

    // First level of a partition
    def firstLevel(partition: Set[S]): F[Fringe[M, S]] =
      closestMessages(base.filterKeys(partition).filterKeys(partition))(witnessesF)

    certainPartitionF.flatMap(pOpt => pOpt.traverse(firstLevel))
  }
}
