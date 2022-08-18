package coop.rchain.casper

import cats.Monad
import cats.data.EitherT
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.dag.DagOps
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockMetadata
import coop.rchain.shared._

// TODO: refactor all validation functions to separate logging from actual validation logic
object Validate {
  type PublicKey = Array[Byte]
  type Data      = Array[Byte]
  type Signature = Array[Byte]

  implicit private val logSource: LogSource = LogSource(this.getClass)
  val signatureVerifiers: Map[String, (Data, Signature, PublicKey) => Boolean] =
    Map(
      "secp256k1" -> Secp256k1.verify
    )

  private def ignore(b: BlockMessage, reason: String): String =
    s"Ignoring block ${PrettyPrinter.buildString(b.blockHash)} because $reason"

  /* Validation of block with logging included */

  def blockSummary[F[_]: Sync: BlockDagStorage: BlockStore: Log: Metrics: Span](
      block: BlockMessage,
      shardId: String,
      expirationThreshold: Int
  ): F[ValidBlockProcessing] = {
    def validate(f: => Boolean, errorStatus: InvalidBlock): EitherT[F, InvalidBlock, ValidBlock] =
      EitherT.fromOption(Option(BlockStatus.valid).filter(_ => f), errorStatus)

    (for {
      // First validate justifications because they are basis for all other validation
      _ <- EitherT.liftF(Span[F].mark("before-justification-regression-validation"))
      _ <- EitherT(Validate.justificationRegressions(block))
      // Validator sequence number validation
      _ <- EitherT.liftF(Span[F].mark("before-sequence-number-validation"))
      _ <- EitherT(Validate.sequenceNumber(block))
      // Block number validation
      _ <- EitherT.liftF(Span[F].mark("before-block-number-validation"))
      _ <- EitherT(Validate.blockNumber(block))
      // Deploys validation
      _ <- EitherT.liftF(Span[F].mark("before-deploys-shard-identifier-validation"))
      _ <- validate(
            BlockValidationLogic.deploysShardIdentifier(block, shardId),
            BlockStatus.invalidDeployShardId
          )
      _ <- EitherT.liftF(Span[F].mark("before-future-transaction-validation"))
      _ <- validate(BlockValidationLogic.futureTransaction(block), BlockStatus.containsFutureDeploy)
      _ <- EitherT.liftF(Span[F].mark("before-transaction-expired-validation"))
      _ <- validate(
            BlockValidationLogic.transactionExpiration(block, expirationThreshold),
            BlockStatus.containsExpiredDeploy
          )
      _ <- EitherT.liftF(Span[F].mark("before-repeat-deploy-validation"))
      s <- EitherT(Validate.repeatDeploy(block, expirationThreshold))
    } yield s).value
  }

  /**
    * Validate no deploy with the same sig has been produced in the chain
    */
  def repeatDeploy[F[_]: Sync: Log: BlockStore: BlockDagStorage: Span](
      block: BlockMessage,
      expirationThreshold: Int
  ): F[ValidBlockProcessing] = {
    val deployKeySet = block.state.deploys.map(_.deploy.sig).toSet

    for {
      _                   <- Span[F].mark("before-repeat-deploy-get-parents")
      blockMetadata       = BlockMetadata.fromBlock(block)
      initParents         <- ProtoUtil.getParentsMetadata(blockMetadata)
      maxBlockNumber      = ProtoUtil.maxBlockNumberMetadata(initParents)
      earliestBlockNumber = maxBlockNumber + 1 - expirationThreshold
      _                   <- Span[F].mark("before-repeat-deploy-duplicate-block")
      maybeDuplicatedBlockMetadata <- DagOps
                                       .bfTraverseF[F, BlockMetadata](initParents)(
                                         b =>
                                           ProtoUtil
                                             .getParentMetadatasAboveBlockNumber(
                                               b,
                                               earliestBlockNumber
                                             )
                                       )
                                       .findF { blockMetadata =>
                                         for {
                                           block        <- BlockStore[F].getUnsafe(blockMetadata.blockHash)
                                           blockDeploys = block.state.deploys.map(_.deploy)
                                         } yield blockDeploys.exists(
                                           d => deployKeySet.contains(d.sig)
                                         )
                                       }
      _ <- Span[F].mark("before-repeat-deploy-duplicate-block-log")
      maybeError <- maybeDuplicatedBlockMetadata
                     .traverse(
                       duplicatedBlockMetadata => {
                         for {
                           duplicatedBlock <- BlockStore[F].getUnsafe(
                                               duplicatedBlockMetadata.blockHash
                                             )
                           currentBlockHashString = PrettyPrinter.buildString(block.blockHash)
                           blockHashString        = PrettyPrinter.buildString(duplicatedBlock.blockHash)
                           duplicatedDeploy = duplicatedBlock.state.deploys
                             .map(_.deploy)
                             .find(d => deployKeySet.contains(d.sig))
                             .get
                           term = duplicatedDeploy.data.term
                           deployerString = PrettyPrinter.buildString(
                             ByteString.copyFrom(duplicatedDeploy.pk.bytes)
                           )
                           timestampString = duplicatedDeploy.data.timestamp.toString
                           message         = s"found deploy [$term (user $deployerString, millisecond timestamp $timestampString)] with the same sig in the block $blockHashString as current block $currentBlockHashString"
                           _               <- Log[F].warn(ignore(block, message))
                         } yield BlockStatus.invalidRepeatDeploy
                       }
                     )
    } yield maybeError.toLeft(BlockStatus.valid)
  }

  def blockNumber[F[_]: Sync: BlockDagStorage: Log](b: BlockMessage): F[ValidBlockProcessing] =
    for {
      parents <- b.justifications
                  .traverse(BlockDagStorage[F].lookupUnsafe(_))
                  .map(_.filter(!_.validationFailed))
      maxBlockNumber = parents.map(_.blockNum).maximumOption.getOrElse(-1L)
      number         = b.blockNumber
      result         = maxBlockNumber + 1 == number
      status <- if (result) {
                 BlockStatus.valid.asRight[InvalidBlock].pure[F]
               } else {
                 val logMessage =
                   if (parents.isEmpty)
                     s"block number $number is not zero, but block has no parents."
                   else
                     s"block number $number is not one more than maximum parent number $maxBlockNumber."
                 for {
                   _ <- Log[F].warn(ignore(b, logMessage))
                 } yield BlockStatus.invalidBlockNumber.asLeft[ValidBlock]
               }
    } yield status

  /**
    * Works with either efficient justifications or full explicit justifications.
    * Specifically, with efficient justifications, if a block B doesn't update its
    * creator justification, this check will fail as expected. The exception is when
    * B's creator justification is the genesis block.
    */
  def sequenceNumber[F[_]: Sync: BlockDagStorage: Log](b: BlockMessage): F[ValidBlockProcessing] =
    for {
      justifications         <- b.justifications.traverse(BlockDagStorage[F].lookupUnsafe(_))
      creatorJustifications  = justifications.filter(_.sender == b.sender)
      creatorLatestSeqNumber = creatorJustifications.map(_.seqNum).maximumOption.getOrElse(-1L)
      number                 = b.seqNum
      result                 = creatorLatestSeqNumber + 1L == number
      status <- if (result) {
                 BlockStatus.valid.asRight[InvalidBlock].pure[F]
               } else {
                 for {
                   _ <- Log[F].warn(
                         ignore(
                           b,
                           s"seq number $number is not one more than creator justification number $creatorLatestSeqNumber."
                         )
                       )
                 } yield BlockStatus.invalidSequenceNumber.asLeft[ValidBlock]
               }
    } yield status

  /**
    * Justification regression check.
    * Compares justifications that has been already used by sender and recorded in the DAG with
    * justifications used by the same sender in new block `b` and assures that there is no
    * regression.
    */
  def justificationRegressions[F[_]: Monad: BlockDagStorage](
      b: BlockMessage
  ): F[ValidBlockProcessing] =
    checkJustificationRegression(b).map { isValidOpt =>
      if (isValidOpt.getOrElse(true)) BlockStatus.valid.asRight[InvalidBlock]
      else BlockStatus.justificationRegression.asLeft
    }

  def checkJustificationRegression[F[_]: Monad: BlockDagStorage](
      b: BlockMessage
  ): F[Option[Boolean]] =
    for {
      msgMap <- BlockDagStorage[F].getRepresentation.map(_.dagMessageState.msgMap)
//      justifications = b.justifications.map(msgMap)
    } yield for {
      // TODO: temporary don't expect that all justifications are available in msgMap to satisfy the failing tests
      //  - with multi-parent finalizer all messages should be available
      justifications <- b.justifications.map(msgMap.get).sequence
      prevMsg        <- justifications.find(_.sender == b.sender)
      res = justifications.forall { just =>
        val justPrevMsgOpt = prevMsg.parents.map(msgMap).find(_.sender == just.sender)
        justPrevMsgOpt.forall { justPrevMsg =>
          // Check that previous sender's message did not seen nothing more then supplied message
          val seenByJust     = just.seen
          val seenByJustPrev = justPrevMsg.seen
          (seenByJustPrev -- seenByJust).isEmpty
        }
      }
    } yield res

  /**
    * If block contains an invalid justification block B and the creator of B is still bonded,
    * return a RejectableBlock. Otherwise return an IncludeableBlock.
    */
  def neglectedInvalidBlock[F[_]: Monad: BlockDagStorage](
      block: BlockMessage
  ): F[ValidBlockProcessing] =
    for {
      justifications <- block.justifications.flatTraverse(
                         BlockDagStorage[F].lookup(_).map(_.toList)
                       )
      invalidValidators = justifications.filter(_.validationFailed).map(b => b.sender)
      neglectedInvalidJustification = invalidValidators.exists { invalidValidator =>
        val slashedValidatorBond = block.bonds.get(invalidValidator)
        slashedValidatorBond match {
          case Some(stake) => stake > 0
          case None        => false
        }
      }
      result = if (neglectedInvalidJustification) {
        BlockStatus.neglectedInvalidBlock.asLeft[ValidBlock]
      } else {
        BlockStatus.valid.asRight[InvalidBlock]
      }
    } yield result

  def bondsCache[F[_]: Concurrent: RuntimeManager: Log](
      b: BlockMessage
  ): F[ValidBlockProcessing] = {
    val bonds          = b.bonds
    val tuplespaceHash = b.postStateHash

    RuntimeManager[F].computeBonds(tuplespaceHash).flatMap { computedBonds =>
      if (bonds.toSet == computedBonds.toSet) {
        BlockStatus.valid.asRight[InvalidBlock].pure
      } else {
        for {
          _ <- Log[F].warn("Bonds in proof of stake contract do not match block's bond cache.")
        } yield BlockStatus.invalidBondsCache.asLeft[ValidBlock]
      }
    }
  }
}
