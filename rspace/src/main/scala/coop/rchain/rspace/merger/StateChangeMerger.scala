package coop.rchain.rspace.merger

import cats.effect.Concurrent
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.HistoryReaderBinary
import coop.rchain.rspace.{
  util,
  HotStoreTrieAction,
  TrieDeleteConsume,
  TrieDeleteJoins,
  TrieDeleteProduce,
  TrieInsertBinaryConsume,
  TrieInsertBinaryJoins,
  TrieInsertBinaryProduce
}
import fs2.Stream
import coop.rchain.shared.syntax._
import scodec.bits.ByteVector
import cats.syntax.all._

object StateChangeMerger {
  def computeTrieActions[F[_]: Concurrent, C, P, A, K](
      changes: StateChange,
      baseReader: HistoryReaderBinary[F, C, P, A, K],
      joinsPointerForChannel: Blake2b256Hash => F[Blake2b256Hash]
  ): F[List[HotStoreTrieAction]] = {
    val produceActionsCompute: Stream[F, HotStoreTrieAction] = Stream
      .emits(changes.datumChanges.toVector)
      .parEvalMapProcBounded {
        case (valuePointerHash, changes) =>
          for {
            init <- baseReader.getData(valuePointerHash).map(_.map(_.raw))
            r = (init diff changes.removed) ++ changes.added match {
              // if no datums left - the whole produce is removed
              case Seq() if init.nonEmpty => TrieDeleteProduce(valuePointerHash).some
              // if base state had no datums - produce is created,
              // if base state had datums but value is changed - produce adjusted (new value inserted)
              case newVal if init != newVal =>
                TrieInsertBinaryProduce(valuePointerHash, newVal).some
              // if value not changed - do nothing
              case _ => None
            }
          } yield r
      }
      .collect { case Some(v) => v }

    /**
      * This classes are used to compute joins.
      * Consume value pointer that stores continuations on some channel is identified by channels involved in.
      * Therefore when no continuations on some consume is left and the whole consume ponter is removed -
      * no joins with corresponding seq of channels exist in tuple space. So join should be removed.
      * */
    trait JoinAction {
      def joinChannels: Seq[Blake2b256Hash]
    }
    final case class AddJoin(c: Seq[Blake2b256Hash]) extends JoinAction {
      override def joinChannels: Seq[Blake2b256Hash] = c
    }
    final case class RemoveJoin(c: Seq[Blake2b256Hash]) extends JoinAction {
      override def joinChannels: Seq[Blake2b256Hash] = c
    }
    final case class ConsumeAndJoinActions(
        consumeAction: HotStoreTrieAction,
        joinAction: Option[JoinAction]
    )

    val consumeAndJoinActionsCompute: Stream[F, ConsumeAndJoinActions] = Stream
      .emits(changes.kontChanges.toVector)
      .parEvalMapProcBounded {
        case (valuePointerHash, ChannelChange(added, removed)) =>
          for {
            init <- baseReader.getContinuations(valuePointerHash).map(_.map(_.raw))

            /** NOTE: all konts inside the same consume value pointer are put by consumes listening on the same
              * seq of channels so to get join affected its enough to read it from one of kont */
            r = (init diff removed.map(_.body)) ++ added.map(_.body) match {

              case newVal if init == newVal =>
                //assert(false)
                None // nothing changed, should not be the case. But it is. TODO figure out why

              /** if all konts from base are removed - remove consume, remove join */
              case Seq() if init.nonEmpty => {
                val joinToRemove = removed.head.channels
                ConsumeAndJoinActions(
                  TrieDeleteConsume(valuePointerHash),
                  RemoveJoin(joinToRemove).some
                ).some
              }

              /** if no konts were in base state and some are added - insert value and add join */
              case newVal if init.isEmpty => {
                val joinToAdd = added.head.channels
                ConsumeAndJoinActions(
                  TrieInsertBinaryConsume(valuePointerHash, newVal),
                  AddJoin(joinToAdd).some
                ).some
              }

              /** if konts were updated but consume is present in base state - update value, joins are not affected */
              case newVal if (init.nonEmpty && newVal != init) =>
                ConsumeAndJoinActions(
                  TrieInsertBinaryConsume(valuePointerHash, newVal),
                  None
                ).some
            }
          } yield r
      }
      .collect { case Some(v) => v }

    for {
      produceTrieActions    <- produceActionsCompute.compile.toList
      consumeAndJoinActions <- consumeAndJoinActionsCompute.compile.toList

      consumeTrieActions = consumeAndJoinActions.collect {
        case ConsumeAndJoinActions(v, _) => v
      }
      joinActions = consumeAndJoinActions.collect {
        case ConsumeAndJoinActions(_, Some(v)) => v
      }

      // this is populated on indexing to find match between seq of channels and join binary value
      joinsChannelsToBodyMap = changes.joinsIndex.map {
        case JoinIndex(body, channels) => (channels -> body)
      }.toMap

      // for all channels in joins affected - load joins values from base state
      initJoins <- joinActions
                    .flatMap(v => v.joinChannels)
                    .distinct
                    .traverse { channel =>
                      for {
                        joinsPointer <- joinsPointerForChannel(channel)
                        joins        <- baseReader.getJoins(joinsPointer).map(_.map(_.raw))

                      } yield (channel, joins)
                    }
                    .map(_.toMap)
      newJoins <- joinActions
                   .foldLeftM[F, Map[Blake2b256Hash, Seq[ByteVector]]](
                     initJoins
                   ) {
                     case (acc, action) =>
                       val channels = action.joinChannels
                       // join binary value that corresponds to channels
                       val joinBodyOpt = joinsChannelsToBodyMap.get(channels)
                       joinBodyOpt match {
                         case Some(joinBody) =>
                           for {
                             joinPointers <- channels.toList.traverse(joinsPointerForChannel)
                             next = joinPointers.foldLeft(acc)((a, jp) => {
                               val cur = a.getOrElse(jp, Seq.empty)
                               val newVal = action match {
                                 case AddJoin(_)    => (cur.toSet + joinBody)
                                 case RemoveJoin(_) => (cur.toSet - joinBody)
                               }
                               a.updated(jp, newVal.toSeq)
                             })
                           } yield next
                         // this clause means that consume adding/deleting did not influence joins
                         case None => acc.pure
                       }

                   }

      joinsActions = newJoins.map {
        case (joinPointer, joins) =>
          joins match {
            case Seq() => TrieDeleteJoins(joinPointer)
            // Joins must be sorted to produce replayable root hash
            case newVal => TrieInsertBinaryJoins(joinPointer, newVal.sorted(util.ordByteVector))
          }
      }

    } yield produceTrieActions ++ consumeTrieActions ++ joinsActions
  }
}
