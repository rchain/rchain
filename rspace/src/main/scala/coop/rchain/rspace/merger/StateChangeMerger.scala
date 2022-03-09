package coop.rchain.rspace.merger

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.rspace._
import coop.rchain.rspace.hashing.{Blake2b256Hash, StableHashProvider}
import coop.rchain.rspace.history.HistoryReaderBinary
import coop.rchain.rspace.merger.MergingLogic.NumberChannelsDiff
import scodec.bits.ByteVector

object StateChangeMerger {

  /**
    * This classes are used to compute joins.
    * Consume value pointer that stores continuations on some channel is identified by channels involved in.
    * Therefore when no continuations on some consume is left and the whole consume ponter is removed -
    * no joins with corresponding seq of channels exist in tuple space. So join should be removed.
    * */
  sealed trait JoinAction { def channels: Seq[Blake2b256Hash] }
  final case class AddJoin(channels: Seq[Blake2b256Hash])    extends JoinAction
  final case class RemoveJoin(channels: Seq[Blake2b256Hash]) extends JoinAction
  final case class ConsumeAndJoinActions(
      consumeAction: HotStoreTrieAction,
      joinAction: Option[JoinAction]
  )

  def computeTrieActions[F[_]: Concurrent, C, P, A, K](
      changes: StateChange,
      baseReader: HistoryReaderBinary[F, C, P, A, K],
      mergeableChs: NumberChannelsDiff,
      // Override channel change
      handleChannelChange: (
          Blake2b256Hash,
          ChannelChange[ByteVector],
          NumberChannelsDiff
      ) => F[Option[HotStoreTrieAction]]
  ): F[Vector[HotStoreTrieAction]] = {

    val consumeAndJoinActionsCompute: F[Vector[ConsumeAndJoinActions]] =
      changes.kontChanges.toVector.traverse {
        case (consumeChannels, ChannelChange(added, removed)) =>
          val historyPointer = StableHashProvider.hash(consumeChannels)
          for {
            init <- baseReader.getContinuations(historyPointer).map(_.map(_.raw))
            r <- (init diff removed) ++ added match {
                  case newVal if init == newVal =>
                    val err =
                      "Merging logic error: empty consume change when computing trie action."
                    new Exception(err).raiseError[F, ConsumeAndJoinActions]

                  /** All konts present in base are removed - remove consume, remove join. */
                  case Seq() if init.nonEmpty =>
                    val joinToRemove = consumeChannels
                    ConsumeAndJoinActions(
                      TrieDeleteConsume(historyPointer),
                      RemoveJoin(joinToRemove).some
                    ).pure

                  /** No konts were in base state and some are added - insert konts and add join. */
                  case newVal if init.isEmpty =>
                    val joinToAdd = consumeChannels
                    ConsumeAndJoinActions(
                      TrieInsertBinaryConsume(historyPointer, newVal),
                      AddJoin(joinToAdd).some
                    ).pure

                  /** if konts were updated but consume is present in base state - update konts, no joins. */
                  case newVal if (init.nonEmpty && newVal != init) =>
                    ConsumeAndJoinActions(
                      TrieInsertBinaryConsume(historyPointer, newVal),
                      None
                    ).pure
                }
          } yield r
      }

    def mkTrieAction(
        historyPointer: Blake2b256Hash,
        initValue: Blake2b256Hash => F[Seq[ByteVector]],
        changes: ChannelChange[ByteVector],
        removeAction: Blake2b256Hash => HotStoreTrieAction,
        updateAction: (Blake2b256Hash, Seq[ByteVector]) => HotStoreTrieAction
    ): F[HotStoreTrieAction] =
      for {
        init <- initValue(historyPointer)
        r <- (init diff changes.removed) ++ changes.added match {
              case Seq() if init.nonEmpty   => removeAction(historyPointer).pure
              case newVal if init != newVal => updateAction(historyPointer, newVal).pure
              case newVal if init == newVal =>
                val err =
                  "Merging logic error: empty channel change for produce or join when computing trie action."
                new Exception(err).raiseError[F, HotStoreTrieAction]
            }
      } yield r

    for {
      //compute consume actions and related joins changes
      consumeWithJoinActions <- consumeAndJoinActionsCompute
      // trie actions for consumes
      consumeTrieActions = consumeWithJoinActions.collect { case ConsumeAndJoinActions(v, _) => v }
      // trie actions for produces
      produceTrieActions <- changes.datumsChanges.toVector.traverse {
                             case (historyPointer, changes) =>
                               for {
                                 actionOpt <- handleChannelChange(
                                               historyPointer,
                                               changes,
                                               mergeableChs
                                             )

                                 trieAction <- actionOpt.map(_.pure).getOrElse {
                                                mkTrieAction(
                                                  historyPointer,
                                                  baseReader.getData(_).map(_.map(_.raw)),
                                                  changes,
                                                  TrieDeleteProduce,
                                                  TrieInsertBinaryProduce
                                                )
                                              }
                               } yield trieAction
                           }
      // trie actions for joins
      joinsChannelsToBodyMap = changes.consumeChannelsToJoinSerializedMap
      joinsChanges = consumeWithJoinActions.foldLeft(
        Map.empty[Blake2b256Hash, ChannelChange[ByteVector]]
      ) {
        case (acc, ConsumeAndJoinActions(_, Some(joinAction))) =>
          val joinChannels = joinAction.channels
          require(
            joinsChannelsToBodyMap.contains(joinChannels),
            "No ByteVector value for join found when merging when computing trie action."
          )
          val join = joinsChannelsToBodyMap(joinChannels)
          joinChannels.foldLeft(acc) { (a, c) =>
            val curVal = a.getOrElse(c, ChannelChange.empty)
            val newVal = joinAction match {
              case AddJoin(_)    => curVal.copy(added = join +: curVal.added)
              case RemoveJoin(_) => curVal.copy(removed = join +: curVal.removed)
            }
            a.updated(c, newVal)
          }
        case (acc, _) => acc // no join action
      }
      joinsTrieActions <- joinsChanges.toVector
                           .traverse {
                             case (historyPointer, changes) =>
                               mkTrieAction(
                                 historyPointer,
                                 baseReader.getJoins(_).map(_.map(_.raw)),
                                 changes,
                                 TrieDeleteJoins,
                                 TrieInsertBinaryJoins
                               )
                           }
    } yield produceTrieActions ++ consumeTrieActions ++ joinsTrieActions
  }
}
