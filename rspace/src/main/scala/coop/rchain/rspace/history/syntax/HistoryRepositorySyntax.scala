package coop.rchain.rspace.history.syntax

import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
import cats.implicits.none
import cats.syntax.all._
import coop.rchain.rspace.channelStore.{ContinuationHash, DataJoinHash}
import coop.rchain.rspace.history.{HistoryHashReader, HistoryRepository}
import coop.rchain.rspace.internal.WaitingContinuation
import coop.rchain.rspace.syntax.syntaxHistoryRepository
import coop.rchain.rspace.trace.{COMM, Consume, Event, EventGroup, IOEvent, Produce}
import coop.rchain.rspace.trace.Event.{
  containConflictingEvents,
  extractJoinedChannels,
  extractRSpaceEventGroup,
  produceChannels,
  Conflict,
  IsConflict,
  NonConflict
}
import coop.rchain.rspace.{internal, Blake2b256Hash, StableHashProvider}
import coop.rchain.shared.{Log, Serialize, Stopwatch}

import scala.collection.immutable.Set
import scala.collection.parallel.immutable.ParSeq
import scala.language.{higherKinds, implicitConversions}

trait HistoryRepositorySyntax {
  implicit final def syntaxHistoryRepository[F[_]: Concurrent, C, P, A, K](
      historyRepo: HistoryRepository[F, C, P, A, K]
  ): HistoryRepositoryOps[F, C, P, A, K] =
    new HistoryRepositoryOps[F, C, P, A, K](historyRepo)
}
final class HistoryRepositoryOps[F[_]: Concurrent, C, P, A, K](
    private val historyRepo: HistoryRepository[F, C, P, A, K]
) {
  def getData(state: Blake2b256Hash, channel: C): F[Seq[internal.Datum[A]]] =
    historyRepo.reset(state) >>= { h =>
      h.getData(channel)
    }

  def getJoins(state: Blake2b256Hash, channel: C): F[Seq[Seq[C]]] =
    historyRepo.reset(state) >>= { h =>
      h.getJoins(channel)
    }

  def getContinuations(
      state: Blake2b256Hash,
      channels: Seq[C]
  ): F[Seq[internal.WaitingContinuation[P, K]]] =
    historyRepo.reset(state) >>= { h =>
      h.getContinuations(channels)
    }

  def getData(
      state: Blake2b256Hash,
      dataHash: Blake2b256Hash
  ): F[Seq[internal.Datum[A]]] =
    historyRepo.reset(state) >>= { h =>
      h.getData(dataHash)
    }

  def getJoins(state: Blake2b256Hash, joinHash: Blake2b256Hash): F[Seq[Seq[C]]] =
    historyRepo.reset(state) >>= { h =>
      h.getJoins(joinHash)
    }

  def getContinuations(
      state: Blake2b256Hash,
      continuationHash: Blake2b256Hash
  ): F[Seq[internal.WaitingContinuation[P, K]]] =
    historyRepo.reset(state) >>= { h =>
      h.getContinuations(continuationHash)
    }

  def getDataFromChannelHash(
      channelHash: Blake2b256Hash
  ): F[Seq[internal.Datum[A]]] =
    for {
      maybeDataHash <- historyRepo.getChannelHash(channelHash)
      dataHash <- maybeDataHash match {
                   case Some(DataJoinHash(dataHash, _)) => dataHash.pure[F]
                   case _ =>
                     Sync[F].raiseError[Blake2b256Hash](
                       new Exception(s"not found data hash for $channelHash in channel store")
                     )
                 }
      data <- historyRepo.getData(dataHash)
    } yield data

  def getDataFromChannelHash(
      state: Blake2b256Hash,
      channelHash: Blake2b256Hash
  ): F[Seq[internal.Datum[A]]] =
    historyRepo.reset(state) >>= { h =>
      getDataFromChannelHash(channelHash)
    }

  def getJoinsFromChannelHash(
      channelHash: Blake2b256Hash
  ): F[Seq[Seq[C]]] =
    for {
      maybeJoinHash <- historyRepo.getChannelHash(channelHash)
      joinHash <- maybeJoinHash match {
                   case Some(DataJoinHash(_, joinHash)) => joinHash.pure[F]
                   case _ =>
                     Sync[F].raiseError[Blake2b256Hash](
                       new Exception(s"not found join hash for $channelHash in channel store")
                     )
                 }
      data <- historyRepo.getJoins(joinHash)
    } yield data

  def getJoinsFromChannelHash(
      state: Blake2b256Hash,
      channelHash: Blake2b256Hash
  ): F[Seq[Seq[C]]] =
    historyRepo.reset(state) >>= { h =>
      getJoinsFromChannelHash(channelHash)
    }

  def getContinuationFromChannelHash(
      channelHash: Blake2b256Hash
  ): F[Seq[internal.WaitingContinuation[P, K]]] =
    for {
      maybeContinuationHash <- historyRepo.getChannelHash(channelHash)
      continuations <- maybeContinuationHash match {
                        case Some(ContinuationHash(continuationHash)) =>
                          historyRepo.getContinuations(continuationHash)
                        case _ => Seq.empty[WaitingContinuation[P, K]].pure[F]
                      }
    } yield continuations

  def getContinuationFromChannelHash(
      state: Blake2b256Hash,
      channelHash: Blake2b256Hash
  ): F[Seq[internal.WaitingContinuation[P, K]]] =
    historyRepo.reset(state) >>= { h =>
      getContinuationFromChannelHash(channelHash)
    }

//  def calculateConflictingEvents(
//      notCommedProducesMain: Seq[Produce],
//      notCommedConsumesMain: Seq[Consume],
//      commedProducesMain: Seq[Produce],
//      commedConsumesMain: Seq[Consume],
//      notCommedProducesMerging: Seq[Produce],
//      notCommedConsumesMerging: Seq[Consume],
//      commedProducesMerging: Seq[Produce],
//      commedConsumesMerging: Seq[Consume],
//      baseState: Blake2b256Hash
//  ) = {
//    // if main produce channel is met in any merging consume - produce is conflicting
//    val (conflictingFreeProduces, _) = notCommedProducesMerging.partition(
//      p =>
//        notCommedConsumesMain
//          .foldLeft(false)((acc, consume) => acc || consume.channelsHashes.contains(p.channelsHash))
//    )
//
//    // if main consume channel is met in any merging produce - consume is conflicting
//    val (conflictingFreeConsumes, _) = notCommedConsumesMerging.partition(
//      c =>
//        notCommedProducesMain
//          .foldLeft(false)((acc, produce) => acc || c.channelsHashes.contains(produce.channelsHash))
//    )
//
//    for {
//      h <- historyRepo.reset(baseState)
//      conflictingCommedProduces <- fs2.Stream
//                                    .emits((commedProducesMerging ++ commedProducesMain).map { p =>
//                                      fs2.Stream
//                                        .eval(
//                                          for {
//                                            baseConts <- h.getContinuationFromChannelHash(
//                                                          p.channelsHash
//                                                        )
//                                          } yield (baseConts.nonEmpty, p)
//                                        )
//                                        .filter(v => v._1)
//                                        .map(_._2)
//                                    })
//                                    .parJoinUnbounded
//                                    .compile
//                                    .toVector
//      conflictingCommedConsumes <- fs2.Stream
//                                    .emits((commedConsumesMerging ++ commedConsumesMain).map { c =>
//                                      fs2.Stream
//                                        .eval(
//                                          for {
//                                            conflictsWithBase <- c.channelsHashes.toList
//                                                                  .foldLeftM(false)(
//                                                                    (acc, ch) =>
//                                                                      h.getDataFromChannelHash(ch)
//                                                                        .map(_.nonEmpty && acc)
//                                                                  )
//                                          } yield (conflictsWithBase, c)
//                                        )
//                                        .filter(v => v._1)
//                                        .map(_._2)
//                                    })
//                                    .parJoinUnbounded
//                                    .compile
//                                    .toVector
//      conflictingCommedProduceFromMerging = commedProducesMerging.filter(
//        p => conflictingCommedProduces.exists(_.channelsHash == p.channelsHash)
//      )
//      conflictingCommedConsumesFromMerging = commedConsumesMerging.filter(
//        c => conflictingCommedConsumes.exists(_.channelsHashes.contains(c.channelsHashes))
//      )
//
//    } yield (
//      conflictingFreeProduces,
//      conflictingCommedProduceFromMerging,
//      conflictingFreeConsumes,
//      conflictingCommedConsumesFromMerging
//    )
//  }

//  def simpleConflictDetector(
//      baseState: Blake2b256Hash,
//      mainFreeEvents: Seq[IOEvent],
//      mergeFreeEvents: Seq[IOEvent]
//  ): F[Set[Blake2b256Hash]] = {
//
//    case class ChannelPolarityState(
//        consumes: Int,
//        produces: Int,
//        persistentProdPresent: Boolean,
//        persistentConsPresent: Boolean
//    )
//
//    def processChan(
//        channelsPolarities: Ref[F, Map[Blake2b256Hash, Ref[F, ChannelPolarityState]]],
//        baseHistoryReader: HistoryHashReader[F, C, P, A, K],
//        channel: Blake2b256Hash,
//        isProduce: Boolean,
//        isConsume: Boolean,
//        persistent: Boolean
//    ) =
//      for {
//        // prepare/get polarity state for channel - nothing interesting here
//        x <- Ref.of[F, ChannelPolarityState](ChannelPolarityState(0, 0, false, false))
//        pRef <- channelsPolarities.modify[Ref[F, ChannelPolarityState]] { s =>
//                 s.get(channel) match {
//                   case Some(a) => (s, a)
//                   case None    => (s.updated(channel, x), x)
//                 }
//               }
//
//        // extract what is in base state - ho many produces/consumes at the channel, if there are persistent ones
//        producesAtBase <- baseHistoryReader.getData(channel)
//        consumesAtBase <- baseHistoryReader.getContinuations(channel)
//        (prodNumAtBase, persistentProdAtBase) = (
//          producesAtBase.size,
//          producesAtBase.foldLeft(false)((acc, pr) => acc || pr.persist)
//        )
//        (consNumAtBase, persistentConsAtBase) = (
//          consumesAtBase.size,
//          consumesAtBase.foldLeft(false)((acc, co) => acc || co.persist)
//        )
//        //_ = assert(prodNumAtBase != 0 && consNumAtBase != 0)
//
//        persistentProdFound = (isProduce && persistent) || persistentProdAtBase
//        persistentConsFound = (isConsume && persistent) || persistentConsAtBase
//
//        _ <- pRef.update(s => {
//              val newPersistentProdPresent = s.persistentProdPresent || persistentProdFound
//              val newPersistentConsPresent = s.persistentConsPresent || persistentConsFound
//              assert(!(newPersistentProdPresent && newPersistentConsPresent))
//              val newProdCount =
//                // persistent consume disables all produces
//                if (newPersistentConsPresent) 0
//                else if (isProduce) s.produces + prodNumAtBase + 1
//                else s.produces + prodNumAtBase
//              val newConsCount =
//                // persistent produce disables all consumes
//                if (newPersistentProdPresent) 0
//                else if (isConsume) s.consumes + consNumAtBase + 1
//                else s.consumes + consNumAtBase
//
//              ChannelPolarityState(
//                consumes = newConsCount,
//                produces = newProdCount,
//                persistentConsPresent = newPersistentConsPresent,
//                persistentProdPresent = newPersistentProdPresent
//              )
//            })
//      } yield ()
//
//    for {
//      // polarity (consume on chan equals -1, produce equals +1, persistent produce eliminates all consumes, persistent consume eliminates all produces)
//      channelsPolarities <- Ref.of[F, Map[Blake2b256Hash, Ref[F, ChannelPolarityState]]](Map.empty)
//
//      h <- historyRepo.reset(baseState)
//
//      processingStreams = (mainFreeEvents ++ mergeFreeEvents).flatMap {
//        case Produce(channelsHash, _, persistent) =>
//          fs2.Stream.eval(
//            processChan(channelsPolarities, h, channelsHash, true, false, persistent)
//          ) :: Nil
//        case c: Consume =>
//          c.channelsHashes.map(
//            channelsHash =>
//              fs2.Stream.eval(
//                processChan(channelsPolarities, h, channelsHash, false, true, c.persistent)
//              )
//          )
//      }
//
//      _ <- fs2.Stream
//            .emits(processingStreams)
//            .parJoinUnbounded
//            .compile
//            .toVector
//
//      polaritiesResult <- channelsPolarities.get
//      conflictingChans <- polaritiesResult.toList
//                           .filterA(
//                             p =>
//                               p._2.get.map(
//                                 chanPolarity =>
//                                   !(chanPolarity.produces == 0 ||
//                                     chanPolarity.consumes == 0 ||
//                                     chanPolarity.produces == chanPolarity.consumes)
//                               )
//                           )
//                           .map(_.map(_._1))
//      _ = println(
//        s"Found ${conflictingChans.size} conflicting channels out of ${polaritiesResult.size}"
//      )
//    } yield conflictingChans.toSet
//  }

  /**
    * detect conflict events between rightEvents and rightEvents.
    * In conflict case, conflict set contains the conflict channel hash.
    * @param baseState baseState needed here for detect conflict in joins
    * @return
    */
  def isConflict(
      baseState: Blake2b256Hash,
      leftEvents: List[Event],
      rightEvents: List[Event]
  )(implicit sc: Serialize[C]): F[IsConflict] = {
    val leftEventGroup  = extractRSpaceEventGroup(leftEvents)
    val rightEventGroup = extractRSpaceEventGroup(rightEvents)
    val conflictJoinInLeft =
      extractJoinedChannels(leftEventGroup).intersect(produceChannels(rightEventGroup))
    val conflictJoinInRight =
      extractJoinedChannels(rightEventGroup).intersect(produceChannels(leftEventGroup))
    val otherConflict          = containConflictingEvents(leftEventGroup, rightEventGroup)
    val normalConflictChannels = conflictJoinInLeft ++ conflictJoinInRight ++ otherConflict
    val nonconflictRightProduceChannels =
      rightEventGroup.produces.filter(p => !normalConflictChannels.contains(p.channelsHash))
    for {
      rightJoins <- nonconflictRightProduceChannels.toList.traverse { produce =>
                     for {
                       joins <- getJoinsFromChannelHash(
                                 baseState,
                                 produce.channelsHash
                               )
                       joinM = joins.filter(_.length > 1)
                     } yield (produce, joinM)
                   }
      leftProduceChannel = leftEventGroup.produces.map(_.channelsHash).toSet
      conflictJoinChannels = rightJoins
        .filter {
          case (produce, joins) => {
            val joinsChannelHashes  = joins.map(_.map(StableHashProvider.hash(_)(sc))).flatten
            val joinsWithoutProduce = joinsChannelHashes diff Seq(produce.channelsHash)
            joinsWithoutProduce.exists(p => leftProduceChannel.contains(p))
          }
        }
        .map(_._1.channelsHash)
        .toSet
    } yield
      if (normalConflictChannels.isEmpty && conflictJoinChannels.isEmpty) {
        NonConflict(leftEventGroup, rightEventGroup)
      } else {
        Conflict(
          leftEventGroup,
          rightEventGroup,
          normalConflictChannels ++ conflictJoinChannels
        )
      }
  }
}
