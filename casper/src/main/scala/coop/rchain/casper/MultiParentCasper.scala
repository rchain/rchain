package coop.rchain.casper

import cats.data.EitherT
import cats.effect.{Concurrent, Sync, Timer}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.dag.{BlockDagStorage, DagRepresentation}
import coop.rchain.casper.merging.BlockIndex
import coop.rchain.casper.protocol._
import coop.rchain.casper.rholang.{InterpreterUtil, RuntimeManager}
import coop.rchain.casper.syntax._
import coop.rchain.casper.util._
import coop.rchain.crypto.signatures.Signed
import coop.rchain.dag.DagOps
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.syntax._
import coop.rchain.models.{BlockHash => _, _}
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

  def addedEvent(block: BlockMessage): RChainEvent = {
    val (blockHash, justifications, deployIds, creator, seqNum) = blockEvent(block)
    RChainEvent.blockAdded(
      blockHash,
      justifications,
      deployIds,
      creator,
      seqNum
    )
  }

  def createdEvent(b: BlockMessage): RChainEvent = {
    val (blockHash, justifications, deployIds, creator, seqNum) = blockEvent(b)
    RChainEvent.blockCreated(
      blockHash,
      justifications,
      deployIds,
      creator,
      seqNum
    )
  }

  private def blockEvent(block: BlockMessage) = {
    val blockHash           = block.blockHash.toHexString
    val justificationHashes = block.justifications.map(_.toHexString)
    val deployIds: List[String] =
      block.body.deploys.map(pd => PrettyPrinter.buildStringNoLimit(pd.deploy.sig))
    val creator = block.sender.toHexString
    val seqNum  = block.seqNum
    (blockHash, justificationHashes, deployIds, creator, seqNum)
  }

  // TODO: temporary function until multiparent casper is removed
  def getSnapshot[F[_]: Sync: RuntimeManager: BlockDagStorage: BlockStore](
      casperShardConf: CasperShardConf
  ): F[CasperSnapshot] = {
    def getOnChainState(b: BlockMessage): F[OnChainCasperState] =
      for {
        av <- RuntimeManager[F].getActiveValidators(b.body.state.postStateHash)
        // bonds are available in block message, but please remember this is just a cache, source of truth is RSpace.
        shardConfig = casperShardConf
      } yield OnChainCasperState(shardConfig, b.bonds, av)

    for {
      dag <- BlockDagStorage[F].getRepresentation
      // TODO LCA and TIPs does not make sense with multiparent finalizer
      t <- dag.latestMessages.map(
            _.valuesIterator.toIndexedSeq.sortBy(_.blockNum).reverse
          )
      (lca, tips)   = (ByteString.EMPTY, t.map(_.blockHash))
      invalidBlocks <- dag.invalidBlocksMap

      // TODO: replaced when parents are removed from BlockMessage
      onChainState <- for {
                       lms            <- dag.latestMessages
                       mainParentHash = lms.head._2.blockHash
                       mainParent     <- BlockStore[F].getUnsafe(mainParentHash)
                       res            <- getOnChainState(mainParent)
                     } yield res

      /**
        * We ensure that only the justifications given in the block are those
        * which are bonded validators in the chosen parent. This is safe because
        * any latest message not from a bonded validator will not change the
        * final fork-choice.
        */
      justifications <- {
        for {
          lms <- dag.latestMessages
          r = lms.toList
            .map { case (_, blockMetadata) => blockMetadata }
            .filter(b => onChainState.bondsMap.keySet.contains(b.sender))
        } yield r.toSet
      }

      parentMetas <- justifications.toList.traverse(j => dag.lookupUnsafe(j.blockHash))
      maxBlockNum = ProtoUtil.maxBlockNumberMetadata(parentMetas)
      maxSeqNums  <- dag.latestMessages.map(m => m.map { case (k, v) => k -> v.seqNum })
      deploysInScope <- {
        val currentBlockNumber  = maxBlockNum + 1
        val earliestBlockNumber = currentBlockNumber - onChainState.shardConf.deployLifespan
        for {
          result <- DagOps
                     .bfTraverseF[F, BlockMetadata](parentMetas)(
                       b =>
                         ProtoUtil
                           .getParentMetadatasAboveBlockNumber(
                             b,
                             earliestBlockNumber,
                             dag
                           )
                     )
                     .foldLeftF(Set.empty[Signed[DeployData]]) { (deploys, blockMetadata) =>
                       for {
                         block        <- BlockStore[F].getUnsafe(blockMetadata.blockHash)
                         blockDeploys = ProtoUtil.deploys(block).map(_.deploy)
                       } yield deploys ++ blockDeploys
                     }
        } yield result
      }
      lfb <- dag.lastFinalizedBlockUnsafe
    } yield CasperSnapshot(
      dag,
      lfb,
      lca,
      tips,
      justifications,
      deploysInScope,
      maxBlockNum,
      maxSeqNums,
      onChainState
    )
  }

  def validate[F[_]: Concurrent: Timer: Time: RuntimeManager: BlockDagStorage: BlockStore: Log: Metrics: Span](
      b: BlockMessage,
      s: CasperSnapshot
  ): F[Either[BlockError, ValidBlock]] = {
    val validationProcess: EitherT[F, BlockError, ValidBlock] =
      for {
        _ <- EitherT(
              Validate
                .blockSummary(b, s, s.onChainState.shardConf.shardName, deployLifespan)
            )
        _ <- EitherT.liftF(Span[F].mark("post-validation-block-summary"))
        _ <- EitherT(
              InterpreterUtil
                .validateBlockCheckpoint(b, s, RuntimeManager[F])
                .map {
                  case Left(ex)       => Left(ex)
                  case Right(Some(_)) => Right(BlockStatus.valid)
                  case Right(None)    => Left(BlockStatus.invalidTransaction)
                }
            )
        _ <- EitherT.liftF(Span[F].mark("transactions-validated"))
        _ <- EitherT(Validate.bondsCache(b, RuntimeManager[F]))
        _ <- EitherT.liftF(Span[F].mark("bonds-cache-validated"))
        _ <- EitherT(Validate.neglectedInvalidBlock(b, s))
        _ <- EitherT.liftF(Span[F].mark("neglected-invalid-block-validated"))

        // This validation is only to punish validator which accepted lower price deploys.
        // And this can happen if not configured correctly.
        minPhloPrice = s.onChainState.shardConf.minPhloPrice
        status <- EitherT(Validate.phloPrice(b, minPhloPrice)).recoverWith {
                   case _ =>
                     val warnToLog = EitherT.liftF[F, BlockError, Unit](
                       Log[F].warn(s"One or more deploys has phloPrice lower than $minPhloPrice")
                     )
                     val asValid = EitherT.rightT[F, BlockError](BlockStatus.valid)
                     warnToLog *> asValid
                 }
        _ <- EitherT.liftF(Span[F].mark("phlogiston-price-validated"))
      } yield status

    val blockPreState  = b.preStateHash
    val blockPostState = b.body.state.postStateHash
    val blockSender    = b.sender.toByteArray
    val indexBlock = for {
      mergeableChs <- RuntimeManager[F].loadMergeableChannels(blockPostState, blockSender, b.seqNum)

      index <- BlockIndex(
                b.blockHash,
                b.body.deploys,
                b.body.systemDeploys,
                blockPreState.toBlake2b256Hash,
                blockPostState.toBlake2b256Hash,
                RuntimeManager[F].getHistoryRepo,
                mergeableChs
              )
      _ = BlockIndex.cache.putIfAbsent(b.blockHash, index)
    } yield ()

    val validationProcessDiag = for {
      // Create block and measure duration
      r                    <- Stopwatch.duration(validationProcess.value)
      (valResult, elapsed) = r
      _ <- valResult
            .map { status =>
              val blockInfo   = PrettyPrinter.buildString(b, short = true)
              val deployCount = b.body.deploys.size
              Log[F].info(s"Block replayed: $blockInfo (${deployCount}d) ($status) [$elapsed]") <*
                indexBlock.whenA(s.onChainState.shardConf.maxNumberOfParents > 1)
            }
            .getOrElse(().pure[F])
    } yield valResult

    Log[F].info(s"Validating block ${PrettyPrinter.buildString(b, short = true)}.") *> validationProcessDiag
  }

  def lastFinalizedBlock[F[_]: Sync: BlockDagStorage: BlockStore]: F[BlockMessage] =
    for {
      dag          <- BlockDagStorage[F].getRepresentation
      blockMessage <- dag.lastFinalizedBlockUnsafe.flatMap(BlockStore[F].getUnsafe)
    } yield blockMessage

  def handleValidBlock[F[_]: Sync: BlockDagStorage: BlockStore](
      block: BlockMessage
  ): F[DagRepresentation] = BlockDagStorage[F].insert(block, invalid = false)

  def handleInvalidBlock[F[_]: Sync: BlockDagStorage: Log](
      block: BlockMessage,
      status: InvalidBlock,
      dag: DagRepresentation
  ): F[DagRepresentation] = {
    def handleInvalidBlockEffect(status: BlockError, block: BlockMessage): F[DagRepresentation] =
      for {
        _ <- Log[F].warn(
              s"Recording invalid block ${PrettyPrinter.buildString(block.blockHash)} for ${status.toString}."
            )
        r <- BlockDagStorage[F].insert(block, invalid = true)
      } yield r

    status match {
      case ib: InvalidBlock if InvalidBlock.isSlashable(ib) =>
        handleInvalidBlockEffect(ib, block)

      case ib: InvalidBlock =>
        Log[F]
          .warn(s"Recording invalid block ${PrettyPrinter.buildString(block.blockHash)} for $ib.")
          .as(dag)
    }
  }

  def deploy[F[_]: Sync: BlockDagStorage: Log](
      d: Signed[DeployData]
  ): F[Either[ParsingError, DeployId]] = {
    import coop.rchain.models.rholang.implicits._
    InterpreterUtil
      .mkTerm(d.data.term, NormalizerEnv(d))
      .bitraverse(
        err => parsingError(s"Error in parsing term: \n$err").pure[F],
        _ => addDeploy(d)
      )
  }

  private def addDeploy[F[_]: Sync: BlockDagStorage: Log](deploy: Signed[DeployData]): F[DeployId] =
    for {
      _ <- BlockDagStorage[F].addDeploy(deploy)
      _ <- Log[F].info(s"Received ${PrettyPrinter.buildString(deploy)}")
    } yield deploy.sig
}
