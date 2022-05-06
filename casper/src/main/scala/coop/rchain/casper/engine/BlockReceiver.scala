package coop.rchain.casper.engine

import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync, Timer}
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.casperbuffer.{CasperBufferKeyValueStorage, CasperBufferStorage}
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper._
import coop.rchain.casper.engine.BlockRetriever.RequestedBlocks
import coop.rchain.casper.protocol.{BlockMessage, CommUtil}
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.syntax._
import coop.rchain.shared.{Log, Time}
import coop.rchain.store.KeyValueStoreManager
import fs2.Stream
import fs2.concurrent.Queue

sealed trait RecvStatus
case object Unvalidated extends RecvStatus // Block received but not validated (put in BlockStore)

final case class BlockReceiverState[MId](state: Map[MId, RecvStatus]) {
  def add(m: MId): BlockReceiverState[MId]    = BlockReceiverState(state + (m -> Unvalidated))
  def remove(m: MId): BlockReceiverState[MId] = BlockReceiverState(state - m)
}

object BlockReceiver {
  def streams[F[_]: Concurrent: Sync: RuntimeManager: RequestedBlocks: RPConfAsk: TransportLayer: BlockStore: BlockDagStorage: CommUtil: Log: Timer: Time: Metrics: Span /*: Concurrent: Sync: RuntimeManager: BlockDagStorage: BlockStore: RequestedBlocks: Timer: Time: RPConfAsk: TransportLayer: CommUtil: Metrics: Log: Span*/ ](
      storeManager: KeyValueStoreManager[F],
      incomingBlocks: Queue[F, BlockMessage],
      receiverOutputQueue: Queue[F, BlockHash],
      finishedProcessingStream: Stream[F, BlockHash],
      casperShardConf: CasperShardConf,
      state: Ref[F, BlockReceiverState[BlockHash]]
  ): (Stream[F, BlockMessage], Stream[F, Unit]) = {
    val casperBufferF = CasperBufferKeyValueStorage.create[F](storeManager)

    def blockStr(b: BlockMessage)       = PrettyPrinter.buildString(b, short = true)
    def logMissingDeps(b: BlockMessage) = Log[F].info(s"Block ${blockStr(b)} missing dependencies.")
    def logNotOfInterest(b: BlockMessage) =
      Log[F].info(s"Block ${blockStr(b)} is not of interest. Dropped")
    def logMalformed(b: BlockMessage) =
      Log[F].info(s"Block ${blockStr(b)} is not of malformed. Dropped")

    def incomingBlocksStream =
      incomingBlocks.dequeue
        .parEvalMapUnorderedProcBounded[F, BlockMessage](_.pure)
        .filter(block => block.shardId == casperShardConf.shardName)
        .evalFilter(
          block =>
            casperBufferF >>= { casperBuffer =>
              implicit val cb: CasperBufferStorage[F] = casperBuffer
              checkIfOfInterest(block)
            } >>= { r =>
              logNotOfInterest(block).unlessA(r).as(r)
            }
        )
        .evalFilter(
          block =>
            checkIfWellFormedAndStore(block) >>= { r =>
              logMalformed(block).unlessA(r).as(r)
            }
        )
        .evalMap(
          block =>
            casperBufferF >>= { casperBuffer =>
              {
                implicit val cb = casperBuffer
                commitToBuffer(block, None)
              } *> block.pure
            }
        )
        .evalMap(block => state.modify(st => (st.add(block.blockHash), block)))

    def processesBlocksStream =
      finishedProcessingStream
        .parEvalMapUnorderedProcBounded(
          _ =>
            for {
              casperBuffer   <- casperBufferF
              blockRetriever = BlockRetriever.of[F]
              // TODO: Checking and updating state not atomic
              hashesInReceive <- state.get.map(_.state.keySet)
              blocksInReceive <- hashesInReceive.toList.traverse(BlockStore[F].getUnsafe)
              blocksResolved <- {
                implicit val (br, cb) = (blockRetriever, casperBuffer)
                blocksInReceive
                  .filterA(
                    block =>
                      checkDependenciesWithEffects[F](block) >>= { r =>
                        logMissingDeps(block).unlessA(r).as(r)
                      }
                  )
                  .map(_.toSet)
              }
              hashesResolved   = blocksResolved.map(_.blockHash)
              hashesUnresolved = hashesInReceive.diff(hashesResolved)
              removedHashes    <- hashesResolved.toList.traverse(casperBuffer.remove)
              _ = state.update { st =>
                // TODO: The pattern (removedHashes.contains) not guarantee atomically removing hash
                //  from CasperBuffer and state because incomingBlocksStream can push hashes in parallel.
                val newSt = hashesResolved.filter(removedHashes.contains).foldLeft(st) {
                  case (st, hash) => st.remove(hash)
                }
                newSt
              }
              _ = hashesResolved
                .filter(removedHashes.contains)
                .foreach(receiverOutputQueue.enqueue1)
              // TODO: Should we request dependencies for hashesUnresolved?
            } yield ()
        )
    (incomingBlocksStream, processesBlocksStream)
  }

  // check if block should be processed
  private def checkIfOfInterest[F[_]: Sync: CasperBufferStorage: BlockDagStorage](
      b: BlockMessage
  ): F[Boolean] =
    for {
      dag <- BlockDagStorage[F].getRepresentation
      alreadyProcessed <- dag.contains(b.blockHash).pure[F] ||^ CasperBufferStorage[F].contains(
                           b.blockHash
                         )
      lowestBlockHeight = dag.heightMap.headOption.map(_._1).getOrElse(-1L)
      oldBlock          = ProtoUtil.blockNumber(b) < lowestBlockHeight
    } yield !alreadyProcessed && !oldBlock

  // check block format and store if check passed
  private def checkIfWellFormedAndStore[F[_]: Sync: BlockStore: Log](b: BlockMessage): F[Boolean] =
    for {
      validFormat <- Validate.formatOfFields(b)
      validSig    <- Validate.blockSignature(b)
      isValid     = validFormat && validSig
      _           <- BlockStore[F].put(b).whenA(isValid)
    } yield isValid

  // check if block has all dependencies available and can be validated
  private def checkDependenciesWithEffects[F[_]: Sync: BlockRetriever: CasperBufferStorage: BlockDagStorage: Log](
      b: BlockMessage
  ): F[Boolean] =
    for {
      r                                    <- getNonValidatedDependencies(b)
      (isReady, depsToFetch, depsInBuffer) = r
      _ <- if (isReady)
            // store pendant block in buffer, it will be removed once block is validated and added to DAG
            commitToBuffer(b, None)
          else
            for {
              // associate parents with new block in casper buffer
              _ <- commitToBuffer(b, (depsToFetch ++ depsInBuffer).some)
              _ <- requestMissingDependencies(depsToFetch)
              _ <- ackProcessed(b)
            } yield ()
    } yield isReady

  private def getNonValidatedDependencies[F[_]: Sync: CasperBufferStorage: BlockDagStorage: Log](
      b: BlockMessage
  ): F[(Boolean, Set[BlockHash], Set[BlockHash])] = {
    val allDeps = ProtoUtil.dependenciesHashesOf(b)
    for {
      // in addition, equivocation tracker has to be checked, as admissible equivocations are not stored in DAG
      //        equivocationHashes <- BlockDagStorage[F].accessEquivocationsTracker { tracker =>
      //                               tracker.equivocationRecords.map { equivocations =>
      //                                 equivocations.flatMap(_.equivocationDetectedBlockHashes)
      //                               }
      //                             }
      equivocationHashes <- Set[BlockHash]().pure[F] // TEMP
      depsInBuffer <- allDeps.filterA(
                       d =>
                         CasperBufferStorage[F].contains(d) ||^ CasperBufferStorage[F]
                           .isPendant(d)
                     )
      dag             <- BlockDagStorage[F].getRepresentation
      depsInDag       = allDeps.filter(dag.contains)
      depsInEqTracker = allDeps.filter(equivocationHashes.contains)
      depsValidated   = depsInDag ++ depsInEqTracker
      depsToFetch     = allDeps diff depsInBuffer diff depsInDag diff depsInEqTracker
      ready           = (depsToFetch ++ depsInBuffer).isEmpty
      _ <- Log[F]
            .info(
              s"Block ${PrettyPrinter.buildString(b, short = true)} missing dependencies. " +
                s"To fetch: ${PrettyPrinter.buildString(depsToFetch)}. " +
                s"In buffer: ${PrettyPrinter.buildString(depsInBuffer)}. " +
                s"Validated: ${PrettyPrinter.buildString(depsValidated)}."
            )
            .unlessA(ready)
    } yield (ready, depsToFetch.toSet, depsInBuffer.toSet)
  }

  private def commitToBuffer[F[_]: Sync: CasperBufferStorage](
      b: BlockMessage,
      deps: Option[Set[BlockHash]]
  ): F[Unit] = {
    import cats.instances.list._
    deps match {
      case None    => CasperBufferStorage[F].putPendant(b.blockHash)
      case Some(d) => d.toList.traverse_(h => CasperBufferStorage[F].addRelation(h, b.blockHash))
    }
  }

  private def requestMissingDependencies[F[_]: Sync: BlockRetriever](
      deps: Set[BlockHash]
  ): F[Unit] = {
    import cats.instances.list._
    deps.toList.traverse_(
      BlockRetriever[F]
        .admitHash(_, admitHashReason = BlockRetriever.MissingDependencyRequested)
    )
  }

  private def ackProcessed[F[_]: BlockRetriever](b: BlockMessage): F[Unit] =
    BlockRetriever[F].ackInCasper(b.blockHash)
}
