package coop.rchain.casper

import cats.data.EitherT
import cats.effect.{Concurrent, Sync, Timer}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.dag.{BlockDagStorage, Finalizer}
import coop.rchain.casper.merging.{BlockIndex, MergeScope, ParentsMergedState}
import coop.rchain.casper.protocol._
import coop.rchain.casper.rholang.{InterpreterUtil, RuntimeManager}
import coop.rchain.casper.syntax._
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.syntax._
import coop.rchain.models.{BlockHash => _, _}
import coop.rchain.sdk.error.FatalError
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
      _ <- FatalError(
            "Parents must not be empty to calculate pre-state. Genesis block pre-state is loaded from config."
          ).raiseError.whenA(parentHashes.isEmpty)

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
                         FatalError(errMsg)
                       }

      prevFringeState           = fringeRecord.stateHash
      prevFringeRejectedDeploys = fringeRecord.rejectedDeploys
      prevFringeStateHash       = prevFringeState.toByteString
      // TODO: for empty fringe bonds map should be loaded from bonds file (if validated in replay)
      bondsMap <- if (prevFringe.isEmpty)
                   justifications.head.bondsMap.pure[F]
                 else
                   RuntimeManager[F].computeBonds(prevFringeStateHash)

      (_, newFringeOpt) = finalizer.calculateFinalization(parents, bondsMap)
      newFringeHashes   = newFringeOpt.map(_.map(_.id))

      // If new fringe is finalized, merge it
      newFringeResult <- newFringeHashes.traverse { fringe =>
                          val (mScope, baseOpt) =
                            MergeScope.fromDag(fringe, prevFringeHashes, msgMap)
                          for {
                            baseStateOpt <- baseOpt.traverse { h =>
                                             BlockStore[F]
                                               .getUnsafe(h)
                                               .map(_.postStateHash.toBlake2b256Hash)
                                           }
                            result <- MergeScope.merge(
                                       mScope,
                                       baseStateOpt.getOrElse(prevFringeState),
                                       dag.fringeStates,
                                       RuntimeManager[F].getHistoryRepo,
                                       BlockIndex.getBlockIndex[F](_)
                                     )
                            (finalizedState, rejected) = result
                            finalizedStateStr = PrettyPrinter.buildString(
                              finalizedState.toByteString
                            )
                            rejectedDeploysStr = PrettyPrinter.buildString(rejected)
                            msgFinalized       = s"Finalized fringe state: $finalizedStateStr, rejectedDeploys: $rejectedDeploysStr"
                            _                  <- Log[F].info(msgFinalized)
                          } yield result
                        }
      (fringeState, rejectedDeploys) = newFringeResult getOrElse (prevFringeState, prevFringeRejectedDeploys)

      maxHeight  = justifications.map(_.blockNum).maximumOption.getOrElse(-1L)
      maxSeqNums = justifications.map(m => (m.sender, m.seqNum)).toMap
      newFringe  = newFringeHashes.getOrElse(prevFringeHashes)

      // Merge conflict scope (non-finalized blocks above fringe)
      conflictScopeMergeResult <- parentHashes.toSeq match {
                                   case Seq(parent) =>
                                     BlockStore[F]
                                       .getUnsafe(parent)
                                       .map(_.postStateHash)
                                       .map(x => (x.toBlake2b256Hash, Set.empty[ByteString]))
                                   case _ =>
                                     val lms    = dag.dagMessageState.latestMsgs.map(_.id)
                                     val msgMap = dag.dagMessageState.msgMap
                                     val (mScope, baseOpt) =
                                       MergeScope.fromDag(lms, prevFringeHashes, msgMap)
                                     for {
                                       baseStateOpt <- baseOpt.traverse { h =>
                                                        BlockStore[F]
                                                          .getUnsafe(h)
                                                          .map(_.postStateHash.toBlake2b256Hash)
                                                      }
                                       r <- MergeScope.merge(
                                             mScope,
                                             baseStateOpt.getOrElse(prevFringeState),
                                             dag.fringeStates,
                                             RuntimeManager[F].getHistoryRepo,
                                             BlockIndex.getBlockIndex[F](_)
                                           )
                                     } yield r

                                 }
      (preStateHash, csRejectedDeploys) = conflictScopeMergeResult

      // TODO: in validation (InterpreterUtil.validateBlockCheckpoint) this is logged also, check how to unify
      csRejectedDeploysStr = PrettyPrinter.buildString(csRejectedDeploys)
      csMsg                = s"Conflict scope merged with rejectedDeploys: $csRejectedDeploysStr"
      _                    <- Log[F].info(csMsg)
    } yield ParentsMergedState(
      justifications = justifications.toSet,
      maxHeight,
      maxSeqNums,
      fringe = newFringe,
      fringeState = fringeState,
      fringeBondsMap = bondsMap,
      fringeRejectedDeploys = rejectedDeploys,
      preStateHash = preStateHash,
      rejectedDeploys = csRejectedDeploys
    )

  def validate[F[_]: Concurrent: Timer: RuntimeManager: BlockDagStorage: BlockStore: Log: Metrics: Span](
      block: BlockMessage,
      shardId: String,
      minPhloPrice: Long
  ): F[Either[(BlockMetadata, InvalidBlock), BlockMetadata]] = {
    val initBlockMeta = BlockMetadata.fromBlock(block)

    val validateSummary = EitherT(Validate.blockSummary(block, shardId, deployLifespan))
      .as(initBlockMeta)
      .leftMap(e => (initBlockMeta, e))

    val validationProcess: EitherT[F, (BlockMetadata, InvalidBlock), BlockMetadata] =
      for {
        _                                <- validateSummary
        _                                <- EitherT.liftF(Span[F].mark("post-validation-block-summary"))
        validated                        <- EitherT.liftF(InterpreterUtil.validateBlockCheckpoint(block))
        (blockMetadata, validatedResult) = validated
        _ <- EitherT.fromEither(validatedResult match {
              case Left(ex)     => Left((blockMetadata, ex))
              case Right(true)  => Right(blockMetadata)
              case Right(false) => Left((blockMetadata, BlockStatus.invalidStateHash))
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
                       val warnToLog = EitherT.liftF[F, InvalidBlock, Unit](
                         Log[F].warn(s"One or more deploys has phloPrice lower than $minPhloPrice")
                       )
                       val asValid = EitherT.rightT[F, InvalidBlock](BlockStatus.valid)
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
      // TODO: update validated fields in a more clear way
      valResultUpdated <- valResult
                           .map { blockMeta =>
                             val blockInfo   = PrettyPrinter.buildString(block, short = true)
                             val deployCount = block.state.deploys.size
                             Log[F].info(
                               s"Block replayed: $blockInfo (${deployCount}d) (Valid) [$elapsed]"
                             ) *>
                               indexBlock as blockMeta
                               .copy(validated = true)
                               .asRight[(BlockMetadata, InvalidBlock)]
                           }
                           .leftMap {
                             case (blockMeta, err) =>
                               val deployCount = block.state.deploys.size
                               val blockInfo   = PrettyPrinter.buildString(block, short = true)
                               Log[F].warn(
                                 s"Block replayed: $blockInfo (${deployCount}d) ($err) [$elapsed]"
                               ) as
                                 (blockMeta.copy(validated = true, validationFailed = true), err)
                                   .asLeft[BlockMetadata]
                           }
                           .merge
    } yield valResultUpdated

    Log[F].info(s"Validating block ${PrettyPrinter.buildString(block)}.") *> validationProcessDiag
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
