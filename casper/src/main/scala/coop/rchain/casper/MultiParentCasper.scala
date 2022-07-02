package coop.rchain.casper

import cats.data.EitherT
import cats.effect.concurrent.Deferred
import cats.effect.{Concurrent, Sync, Timer}
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.dag.{BlockDagStorage, Finalizer}
import coop.rchain.casper.merging.{BlockIndex, DagMerger, DeployChainIndex}
import coop.rchain.casper.protocol._
import coop.rchain.casper.rholang.{InterpreterUtil, RuntimeManager}
import coop.rchain.casper.syntax._
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.syntax._
import coop.rchain.models.{BlockHash => _, _}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared._

final case class ParsingError(details: String)

object MultiParentCasper {

  // TODO: copied from previous code
  //  - remove it, no need to have error message "Error: error ... of error"
  def parsingError(details: String) = ParsingError(s"Parsing error: $details")

  // TODO: Extract hardcoded deployLifespan from shard config
  // Size of deploy safety range.
  // Validators will try to put deploy in a block only for next `deployLifespan` blocks.
  // Required to enable protection from re-submitting duplicate deploys
  val deployLifespan = 50

  def getPreStateForNewBlock[F[_]: Concurrent: RuntimeManager: BlockDagStorage: BlockStore: Log]
      : F[ParentsMergedState] =
    for {
      dag <- BlockDagStorage[F].getRepresentation

      // TEMP: take bonds map from first latest message if finalized fringe is not available
      latestMsgs = dag.dagMessageState.latestMsgs

      parentHashes = latestMsgs.map(_.id)

      preState <- getPreStateForParents(parentHashes)
    } yield preState

  def getPreStateForParents[F[_]: Concurrent: RuntimeManager: BlockDagStorage: BlockStore: Log](
      parentHashes: Set[BlockHash]
  ): F[ParentsMergedState] =
    for {
      dag <- BlockDagStorage[F].getRepresentation

      justifications <- parentHashes.toList.traverse(BlockDagStorage[F].lookupUnsafe(_))

      // Calculate finalized fringe from justifications
      msgMap    = dag.dagMessageState.msgMap
      parents   = parentHashes.map(msgMap)
      finalizer = Finalizer(dag.dagMessageState.msgMap)
      // Get currently finalized bonds map
      prevFringe       = finalizer.latestFringe(parents)
      prevFringeHashes = prevFringe.map(_.id)
      // Previous fringe state should be present (loaded from BlockMetadata store)
      fringeRecord <- dag.fringeStates
                       .get(prevFringeHashes)
                       .liftTo {
                         val fringeStr = PrettyPrinter.buildString(prevFringeHashes)
                         val errMsg =
                           s"Fringe state not available in state cache, fringe: $fringeStr"
                         new Exception(errMsg)
                       }

      (prevFringeState, prevFringeRejectedDeploys) = fringeRecord
      prevFringeStateHash                          = prevFringeState.bytes.toArray.toByteString
      // TODO: for empty fringe bonds map should be loaded from bonds file (if validated in replay)
      bondsMap <- if (prevFringe.isEmpty)
                   justifications.head.bondsMap.pure[F]
                 else
                   RuntimeManager[F].computeBonds(prevFringeStateHash)

      (_, newFringeOpt) = finalizer.calculateFinalization(parents, bondsMap)
      newFringeHashes   = newFringeOpt.map(_.map(_.id))

      // If new fringe is finalized, merge it
      newFringeResult <- newFringeHashes.traverse { fringe =>
                          // TODO where to get?
                          val finallyAccepted = Set.empty[DeployChainIndex]
                          val finallyRejected = Set.empty[DeployChainIndex]
                          for {
                            result <- DagMerger.merge[F](
                                       fringe,
                                       prevFringeHashes,
                                       prevFringeStateHash.toBlake2b256Hash,
                                       dag.dagMessageState.msgMap,
                                       finallyAccepted,
                                       finallyRejected,
                                       RuntimeManager[F].getHistoryRepo,
                                       BlockIndex.getBlockIndex[F]
                                     )
                            (finalizedState, rejected) = result
                            finalizedStateStr = PrettyPrinter.buildString(
                              finalizedState.toByteString
                            )
                            rejectedCount = rejected.size
                            msgFinalized  = s"Finalized fringe state: $finalizedStateStr, rejectedDeploys: $rejectedCount"
                            _             <- Log[F].info(msgFinalized)
                          } yield result
                        }
      (fringeState, rejectedDeploys) = newFringeResult getOrElse (prevFringeState, prevFringeRejectedDeploys)

      // TODO: merge non-finalized blocks and data to ParentsPreState data

      maxHeight  = justifications.map(_.blockNum).maximumOption.getOrElse(-1L)
      maxSeqNums = justifications.map(m => (m.sender, m.seqNum)).toMap
      newFringe  = newFringeHashes.getOrElse(prevFringeHashes)
    } yield ParentsMergedState(
      justifications = justifications.toSet,
      fringe = newFringe,
      fringeState = fringeState,
      bondsMap = bondsMap,
      rejectedDeploys = rejectedDeploys,
      maxHeight,
      maxSeqNums
    )

  def validate[F[_]: Concurrent: Timer: RuntimeManager: BlockDagStorage: BlockStore: Log: Metrics: Span](
      block: BlockMessage,
      shardId: String,
      minPhloPrice: Long
  ): F[Either[(BlockMetadata, BlockError), BlockMetadata]] = {
    val initBlockMeta = BlockMetadata.fromBlock(block)

    val validateSummary = EitherT(Validate.blockSummary(block, shardId, deployLifespan))
      .as(initBlockMeta)
      .leftMap(e => (initBlockMeta, e))

    val validationProcess: EitherT[F, (BlockMetadata, BlockError), BlockMetadata] =
      for {
        _                                <- validateSummary
        _                                <- EitherT.liftF(Span[F].mark("post-validation-block-summary"))
        validated                        <- EitherT.liftF(InterpreterUtil.validateBlockCheckpointNew(block))
        (blockMetadata, validatedResult) = validated
        _ <- EitherT.fromEither(validatedResult match {
              case Left(ex)       => Left((blockMetadata, ex))
              case Right(Some(_)) => Right(blockMetadata)
              case Right(None)    => Left((blockMetadata, BlockStatus.invalidTransaction))
            })
        _ <- EitherT.liftF(Span[F].mark("transactions-validated"))
        _ <- EitherT(Validate.bondsCache(block)).as(blockMetadata).leftMap(e => (blockMetadata, e))
        _ <- EitherT.liftF(Span[F].mark("bonds-cache-validated"))
        _ <- EitherT(Validate.neglectedInvalidBlock(block))
              .as(blockMetadata)
              .leftMap(e => (blockMetadata, e))
        _ <- EitherT.liftF(Span[F].mark("neglected-invalid-block-validated"))

        // This validation is only to punish validator which accepted lower price deploys.
        // And this can happen if not configured correctly.
        status <- EitherT(Validate.phloPrice(block, minPhloPrice))
                   .recoverWith {
                     case _ =>
                       val warnToLog = EitherT.liftF[F, BlockError, Unit](
                         Log[F].warn(s"One or more deploys has phloPrice lower than $minPhloPrice")
                       )
                       val asValid = EitherT.rightT[F, BlockError](BlockStatus.valid)
                       warnToLog *> asValid
                   }
                   .as(blockMetadata)
                   .leftMap(e => (blockMetadata, e))
        _ <- EitherT.liftF(Span[F].mark("phlogiston-price-validated"))
      } yield status

    val blockPreState  = block.preStateHash
    val blockPostState = block.postStateHash
    val blockSender    = block.sender.toByteArray

    // TODO: skip creating block index if already in cache
    val indexBlock = for {
      mergeableChs <- RuntimeManager[F].loadMergeableChannels(
                       blockPostState,
                       blockSender,
                       block.seqNum
                     )

      index <- BlockIndex(
                block.blockHash,
                block.state.deploys,
                block.state.systemDeploys,
                blockPreState.toBlake2b256Hash,
                blockPostState.toBlake2b256Hash,
                RuntimeManager[F].getHistoryRepo,
                mergeableChs
              )
      _ = BlockIndex.cache.putIfAbsent(block.blockHash, index)
    } yield ()

    val validationProcessDiag = for {
      // Create block and measure duration
      r                    <- Stopwatch.duration(validationProcess.value)
      (valResult, elapsed) = r
      _ <- valResult
            .map { blockMeta =>
              val blockInfo   = PrettyPrinter.buildString(block, short = true)
              val deployCount = block.state.deploys.size
              Log[F].info(s"Block replayed: $blockInfo (${deployCount}d) (Valid) [$elapsed]") *>
                indexBlock
            }
            .leftMap {
              case (_, err) =>
                val deployCount = block.state.deploys.size
                val blockInfo   = PrettyPrinter.buildString(block, short = true)
                Log[F].warn(s"Block replayed: $blockInfo (${deployCount}d) ($err) [$elapsed]")
            }
            .merge
    } yield valResult

    Log[F].info(s"Validating ${PrettyPrinter.buildString(block)}.") *> validationProcessDiag
  }

  def lastFinalizedBlock[F[_]: Sync: BlockDagStorage: BlockStore]: F[BlockMessage] =
    for {
      dag          <- BlockDagStorage[F].getRepresentation
      blockMessage <- dag.lastFinalizedBlockUnsafe.flatMap(BlockStore[F].getUnsafe)
    } yield blockMessage

  def deploy[F[_]: Sync: BlockDagStorage: Log](
      d: Signed[DeployData]
  ): F[Either[ParsingError, DeployId]] = {
    import coop.rchain.models.rholang.implicits._
    InterpreterUtil
      .mkTerm(d.data.term, NormalizerEnv(d))
      .flatMap(_ => addDeploy(d))
      .attempt
      .map(_.leftMap(err => parsingError(s"Error in parsing term: \n$err")))
  }

  private def addDeploy[F[_]: Sync: BlockDagStorage: Log](deploy: Signed[DeployData]): F[DeployId] =
    for {
      _ <- BlockDagStorage[F].addDeploy(deploy)
      _ <- Log[F].info(s"Received ${PrettyPrinter.buildString(deploy)}")
    } yield deploy.sig
}
