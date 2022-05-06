package coop.rchain.casper.blocks

import cats.effect.{Concurrent, Timer}
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.casperbuffer.CasperBufferStorage
import coop.rchain.blockstorage.dag.{BlockDagStorage, DagRepresentation}
import coop.rchain.casper._
import coop.rchain.casper.engine.BlockRetriever
import coop.rchain.casper.protocol.{BlockMessage, CommUtil}
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.{Log, Time}

/**
  * Logic for processing incoming blocks
  * Blocks created by node itself are not held here, but in Proposer.
  */
class BlockProcessor[F[_]: Concurrent: BlockDagStorage: BlockStore: CasperBufferStorage](
    storeBlock: BlockMessage => F[Unit],
    getDependenciesStatus: BlockMessage => F[(Boolean, Set[BlockHash], Set[BlockHash])],
    commitToBuffer: (BlockMessage, Option[Set[BlockHash]]) => F[Unit],
    removeFromBuffer: BlockMessage => F[Unit],
    requestMissingDependencies: Set[BlockHash] => F[Unit],
    ackProcessed: (BlockMessage) => F[Unit],
    // Casper state to validate block against
    getCasperSnapshot: F[CasperSnapshot],
    validateBlock: (CasperSnapshot, BlockMessage) => F[ValidBlockProcessing],
    effValidBlock: BlockMessage => F[DagRepresentation],
    effInvalidVBlock: (BlockMessage, InvalidBlock, CasperSnapshot) => F[DagRepresentation]
) {

  // check if block should be processed
  def checkIfOfInterest(b: BlockMessage): F[Boolean] =
    for {
      dag <- BlockDagStorage[F].getRepresentation
      alreadyProcessed <- dag.contains(b.blockHash).pure[F] ||^ CasperBufferStorage[F].contains(
                           b.blockHash
                         )
      lowestBlockHeight = dag.heightMap.headOption.map(_._1).getOrElse(-1L)
      oldBlock          = ProtoUtil.blockNumber(b) < lowestBlockHeight
    } yield !alreadyProcessed && !oldBlock

  // check block format and store if check passed
  def checkIfWellFormedAndStore(b: BlockMessage)(
      implicit log: Log[F]
  ): F[Boolean] =
    for {
      validFormat <- Validate.formatOfFields(b)
      validSig    <- Validate.blockSignature(b)
      isValid     = validFormat && validSig
      _           <- storeBlock(b).whenA(isValid)
    } yield isValid

  // check if block has all dependencies available and can be validated
  def checkDependenciesWithEffects(b: BlockMessage): F[Boolean] =
    for {
      r                                    <- getDependenciesStatus(b)
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
    } yield (isReady)

  // validate block and invoke all effects required
  def validateWithEffects(b: BlockMessage): F[ValidBlockProcessing] =
    for {
      cs     <- getCasperSnapshot
      status <- validateBlock(cs, b)
      _ <- status
            .map(s => effValidBlock(b))
            .leftMap {
              // this is to maintain backward compatibility with casper validate method.
              // as it returns not only InvalidBlock or ValidBlock
              case i: InvalidBlock => effInvalidVBlock(b, i, cs)
              case _               => cs.dag.pure[F] // this should never happen
            }
            .merge
      // once block is validated and effects are invoked, it should be removed from buffer
      _ <- removeFromBuffer(b)
      _ <- ackProcessed(b)
    } yield (status)

  val getDependencyFreeFromBuffer = MultiParentCasper.getDependencyFreeFromBuffer
}

object BlockProcessor {
  // format: off
  def apply[F[_]
  /* Execution */   : Concurrent: Timer: Time: RuntimeManager
  /* Storage */     : BlockStore: BlockDagStorage: CasperBufferStorage
  /* Diagnostics */ : Log: Metrics: Span
  /* Comm */        : CommUtil: BlockRetriever // format: on
  ](casperShardConf: CasperShardConf): BlockProcessor[F] = {

    val storeBlock = (b: BlockMessage) => BlockStore[F].put(b)

    val getCasperStateSnapshot = MultiParentCasper.getSnapshot[F](casperShardConf)

    val getNonValidatedDependencies = (b: BlockMessage) => {
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

    val commitToBuffer = (b: BlockMessage, deps: Option[Set[BlockHash]]) => {
      import cats.instances.list._
      deps match {
        case None    => CasperBufferStorage[F].putPendant(b.blockHash)
        case Some(d) => d.toList.traverse_(h => CasperBufferStorage[F].addRelation(h, b.blockHash))
      }
    }

    val removeFromBuffer = (b: BlockMessage) => CasperBufferStorage[F].remove(b.blockHash)

    val requestMissingDependencies = (deps: Set[BlockHash]) => {
      import cats.instances.list._
      deps.toList.traverse_(
        BlockRetriever[F]
          .admitHash(_, admitHashReason = BlockRetriever.MissingDependencyRequested)
      )
    }

    val validateBlock = (s: CasperSnapshot, b: BlockMessage) => MultiParentCasper.validate(b, s)

    def ackProcessed =
      (b: BlockMessage) => BlockRetriever[F].ackInCasper(b.blockHash)

    val effectsForInvalidBlock =
      (b: BlockMessage, r: InvalidBlock, s: CasperSnapshot) =>
        for {
          r <- MultiParentCasper.handleInvalidBlock(b, r, s.dag)
          _ <- CommUtil[F].sendBlockHash(b.blockHash, b.sender)
        } yield r

    val effectsForValidBlock = (b: BlockMessage) =>
      for {
        r <- MultiParentCasper.handleValidBlock(b)
        _ <- CommUtil[F].sendBlockHash(b.blockHash, b.sender)
      } yield r

    new BlockProcessor[F](
      storeBlock,
      getNonValidatedDependencies,
      commitToBuffer,
      removeFromBuffer,
      requestMissingDependencies,
      ackProcessed,
      getCasperStateSnapshot,
      validateBlock,
      effectsForValidBlock,
      effectsForInvalidBlock
    )
  }
}
