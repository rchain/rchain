package coop.rchain.casper.eagercasper.safety

import cats.Monad
import cats.syntax.all._

/**
  * Safety Oracle defines whether message should be finalized against some partition.
  * It outputs a partition of senders inside which message is safe.
  * If/when message is declared as a safe against a partition, it is safe to merge message into provisional state,
  * rejecting conflicts with already provisioned body of a partition.
  *
  * IMPORTANT: once message safe against supermajority partition is found, after merging this is the new final state.
  */
object SafetyOracle {

  def findPartition[F[_]: Monad, M, S](m: M)(
      witnessesF: M => F[Map[S, M]],
      justificationsF: M => F[Map[S, M]]
  )(sender: M => S): F[Option[Set[S]]] = {

    def nextLvl(curLvl: Vector[M]): F[Vector[M]] =
      curLvl
        .traverse(m => witnessesF(m).map(m -> _))
        .map(_.toMap)
        .map { witMaps => // map message of current level -> witnesses of the message
          // Next next partition for each message - senders of all witnesses of the message.
          val nextSendersMap = witMaps.map { case (m, witMap) => sender(m) -> witMap.keySet }
          // Possible partitions.
          val partitionOptions = Vector(Set.empty[S]) ++ nextSendersMap.map {
            case (s, wits) => wits.filter(w => nextSendersMap.get(w).exists(_.contains(s)))
          }
          // Maximum possible partition detected
          val nextPartition = partitionOptions.maxBy(_.size)
          witMaps.toVector.flatMap {
            case (_, wits) => wits.filterKeys(nextPartition.contains).valuesIterator
          }.distinct
        }

    for {
      // Biggest possible partition that message can be part of is the partition
      // consisting of senders that witness the message.
      lvl1 <- witnessesF(m).map(_.valuesIterator.toVector)
      lvl2 <- nextLvl(lvl1)
      isSafe = (lvl1, lvl2, 0).tailRecM {
        case (l1, l2, num) =>
          // partition visible in the last level
          val partition = l2.map(sender).toSet
          // message is part of detected partition
          val partitionIncludesM = partition.contains(sender(m))
          // partition implied does not include sender of the target message - not safe
          val provedNotSafe = !partitionIncludesM
          if (provedNotSafe)
            none[Set[S]].asRight[(Vector[M], Vector[M], Int)].pure
          else {
            // Once safety is proved return partition inside which message is safe
            val safeCase = partition.some.asRight[(Vector[M], Vector[M], Int)].pure
            // If safety is not proved yet - proceed with the next layer
            val uncertainCase = nextLvl(l2).map { nextL =>
              if (nextL.isEmpty) none[Set[S]].asRight[(Vector[M], Vector[M], Int)]
              else (l2, nextL, num + 1).asLeft[Option[Set[S]]]
            }
            val jsOOP = justificationsF(_: M).map(_.filterNot {
              case (s, _) => partition.contains(s)
            })
            val partitionIsCertainF = (l1 ++ l2).distinct
              .filter(m => partition.contains(sender(m)))
              .traverse(jsOOP)
              .map(_.distinct.size == 1)
            partitionIsCertainF.ifM(safeCase, uncertainCase)
          }
      }
      r <- isSafe
    } yield r
  }
}
