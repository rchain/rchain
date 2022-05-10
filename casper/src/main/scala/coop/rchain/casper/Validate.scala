package coop.rchain.casper

import cats.data.EitherT
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import cats.{Applicative, Monad}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.protocol.{BlockMessage, Justification}
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.ProtoUtil.bonds
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.dag.DagOps
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockMetadata
import coop.rchain.shared._

import scala.util.{Success, Try}

object Validate {
  type PublicKey = Array[Byte]
  type Data      = Array[Byte]
  type Signature = Array[Byte]

  val DRIFT                                 = 15000 // 15 seconds
  implicit private val logSource: LogSource = LogSource(this.getClass)
  val signatureVerifiers: Map[String, (Data, Signature, PublicKey) => Boolean] =
    Map(
      "secp256k1" -> Secp256k1.verify
    )

  def ignore(b: BlockMessage, reason: String): String =
    s"Ignoring block ${PrettyPrinter.buildString(b.blockHash)} because $reason"

  /* Validation of block with logging included */

  def blockSignature[F[_]: Applicative: Log](b: BlockMessage): F[Boolean] =
    signatureVerifiers
      .get(b.sigAlgorithm)
      .map(verify => {
        Try(verify(b.blockHash.toByteArray, b.sig.toByteArray, b.sender.toByteArray)) match {
          case Success(true) => true.pure
          case _             => Log[F].warn(ignore(b, "signature is invalid.")).map(_ => false)
        }
      }) getOrElse {
      for {
        _ <- Log[F].warn(ignore(b, s"signature algorithm ${b.sigAlgorithm} is unsupported."))
      } yield false
    }

  def formatOfFields[F[_]: Monad: Log](b: BlockMessage): F[Boolean] =
    if (b.blockHash.isEmpty) {
      for {
        _ <- Log[F].warn(ignore(b, s"block hash is empty."))
      } yield false
    } else if (b.sig.isEmpty) {
      for {
        _ <- Log[F].warn(ignore(b, s"block signature is empty."))
      } yield false
    } else if (b.sigAlgorithm.isEmpty) {
      for {
        _ <- Log[F].warn(ignore(b, s"block signature algorithm is empty."))
      } yield false
    } else if (b.shardId.isEmpty) {
      for {
        _ <- Log[F].warn(ignore(b, s"block shard identifier is empty."))
      } yield false
    } else if (b.body.state.postStateHash.isEmpty) {
      for {
        _ <- Log[F].warn(ignore(b, s"block post state hash is empty."))
      } yield false
    } else {
      true.pure
    }

  def version[F[_]: Monad: Log](b: BlockMessage, version: Long): F[Boolean] = {
    val blockVersion = b.version
    if (blockVersion == version) {
      true.pure
    } else {
      Log[F]
        .warn(
          ignore(
            b,
            s"received block version $blockVersion is the expected version $version."
          )
        )
        .as(false)
    }
  }

  def blockSummary[F[_]: Sync: Log: Time: BlockStore: BlockDagStorage: Metrics: Span](
      block: BlockMessage,
      s: CasperSnapshot,
      shardId: String,
      expirationThreshold: Int
  ): F[ValidBlockProcessing] =
    (for {
      _ <- EitherT.liftF(Span[F].mark("before-timestamp-validation"))
      _ <- EitherT(Validate.timestamp(block))
      _ <- EitherT.liftF(Span[F].mark("before-deploys-shard-identifier-validation"))
      _ <- EitherT(Validate.deploysShardIdentifier(block, shardId))
      _ <- EitherT.liftF(Span[F].mark("before-repeat-deploy-validation"))
      _ <- EitherT(Validate.repeatDeploy(block, s, expirationThreshold))
      _ <- EitherT.liftF(Span[F].mark("before-block-number-validation"))
      _ <- EitherT(Validate.blockNumber(block, s))
      _ <- EitherT.liftF(Span[F].mark("before-future-transaction-validation"))
      _ <- EitherT(Validate.futureTransaction(block))
      _ <- EitherT.liftF(Span[F].mark("before-transaction-expired-validation"))
      _ <- EitherT(Validate.transactionExpiration(block, expirationThreshold))
      _ <- EitherT.liftF(Span[F].mark("before-sequence-number-validation"))
      _ <- EitherT(Validate.sequenceNumber(block))
      _ <- EitherT.liftF(Span[F].mark("before-justification-regression-validation"))
      s <- EitherT(Validate.justificationRegressions(block))
    } yield s).value

  /**
    * Validate no deploy with the same sig has been produced in the chain
    *
    * Agnostic of non-parent justifications
    */
  def repeatDeploy[F[_]: Sync: Log: BlockStore: BlockDagStorage: Span](
      block: BlockMessage,
      s: CasperSnapshot,
      expirationThreshold: Int
  ): F[ValidBlockProcessing] = {
    import cats.instances.option._

    val deployKeySet = block.body.deploys.map(_.deploy.sig).toSet

    for {
      _                   <- Span[F].mark("before-repeat-deploy-get-parents")
      blockMetadata       = BlockMetadata.fromBlock(block, invalid = false)
      initParents         <- ProtoUtil.getParentsMetadata(blockMetadata, s.dag)
      maxBlockNumber      = ProtoUtil.maxBlockNumberMetadata(initParents)
      earliestBlockNumber = maxBlockNumber + 1 - expirationThreshold
      _                   <- Span[F].mark("before-repeat-deploy-duplicate-block")
      maybeDuplicatedBlockMetadata <- DagOps
                                       .bfTraverseF[F, BlockMetadata](initParents)(
                                         b =>
                                           ProtoUtil
                                             .getParentMetadatasAboveBlockNumber(
                                               b,
                                               earliestBlockNumber,
                                               s.dag
                                             )
                                       )
                                       .findF { blockMetadata =>
                                         for {
                                           block <- BlockStore[F].getUnsafe(
                                                     blockMetadata.blockHash
                                                   )
                                           blockDeploys = ProtoUtil.deploys(block).map(_.deploy)
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
                           duplicatedDeploy = ProtoUtil
                             .deploys(duplicatedBlock)
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

  // This is not a slashable offence
  def timestamp[F[_]: Sync: Log: Time: BlockStore](
      b: BlockMessage
  ): F[ValidBlockProcessing] = {
    import cats.instances.list._

    for {
      currentTime  <- Time[F].currentMillis
      timestamp    = b.header.timestamp
      beforeFuture = currentTime + DRIFT >= timestamp
      latestParentTimestamp <- ProtoUtil.parentHashes(b).foldM(0L) {
                                case (latestTimestamp, parentHash) =>
                                  BlockStore[F]
                                    .getUnsafe(parentHash)
                                    .map(parent => {
                                      val timestamp = parent.header.timestamp
                                      math.max(latestTimestamp, timestamp)
                                    })
                              }
      afterLatestParent = timestamp >= latestParentTimestamp
      result <- if (beforeFuture && afterLatestParent) {
                 BlockStatus.valid.asRight[BlockError].pure[F]
               } else {
                 for {
                   _ <- Log[F].warn(
                         ignore(
                           b,
                           s"block timestamp $timestamp is not between latest parent block time and current time."
                         )
                       )
                 } yield BlockStatus.invalidTimestamp.asLeft[ValidBlock]
               }
    } yield result
  }

  // Agnostic of non-parent justifications
  def blockNumber[F[_]: Sync: BlockDagStorage: Log](
      b: BlockMessage,
      s: CasperSnapshot
  ): F[ValidBlockProcessing] = {
    import cats.instances.list._

    for {
      parents <- ProtoUtil.parentHashes(b).traverse { parentHash =>
                  s.dag.lookup(parentHash).flatMap {
                    case Some(p) => p.pure[F]
                    case None =>
                      Sync[F].raiseError[BlockMetadata](
                        new Exception(
                          s"Block dag store was missing ${PrettyPrinter.buildString(parentHash)}."
                        )
                      )
                  }
                }
      maxBlockNumber = parents.foldLeft(-1L) {
        case (acc, p) => math.max(acc, p.blockNum)
      }
      number = ProtoUtil.blockNumber(b)
      result = maxBlockNumber + 1 == number
      status <- if (result) {
                 BlockStatus.valid.asRight[BlockError].pure[F]
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
  }

  def futureTransaction[F[_]: Monad: Log](b: BlockMessage): F[ValidBlockProcessing] = {
    import cats.instances.option._

    val blockNumber       = ProtoUtil.blockNumber(b)
    val deploys           = ProtoUtil.deploys(b).map(_.deploy)
    val maybeFutureDeploy = deploys.find(_.data.validAfterBlockNumber >= blockNumber)
    maybeFutureDeploy
      .traverse { futureDeploy =>
        Log[F]
          .warn(
            ignore(
              b,
              s"block contains an future deploy with valid after block number of ${futureDeploy.data.validAfterBlockNumber}: ${futureDeploy.data.term}"
            )
          )
          .as(BlockStatus.containsFutureDeploy)
      }
      .map(maybeError => maybeError.toLeft(BlockStatus.valid))
  }

  def transactionExpiration[F[_]: Monad: Log](
      b: BlockMessage,
      expirationThreshold: Int
  ): F[ValidBlockProcessing] = {
    import cats.instances.option._

    val earliestAcceptableValidAfterBlockNumber = ProtoUtil.blockNumber(b) - expirationThreshold
    val deploys                                 = ProtoUtil.deploys(b).map(_.deploy)
    val maybeExpiredDeploy =
      deploys.find(_.data.validAfterBlockNumber <= earliestAcceptableValidAfterBlockNumber)
    maybeExpiredDeploy
      .traverse { expiredDeploy =>
        Log[F]
          .warn(
            ignore(
              b,
              s"block contains an expired deploy with valid after block number of ${expiredDeploy.data.validAfterBlockNumber}: ${expiredDeploy.data.term}"
            )
          )
          .as(BlockStatus.containsExpiredDeploy)
      }
      .map(maybeError => maybeError.toLeft(BlockStatus.valid))
  }

  /**
    * Works with either efficient justifications or full explicit justifications.
    * Specifically, with efficient justifications, if a block B doesn't update its
    * creator justification, this check will fail as expected. The exception is when
    * B's creator justification is the genesis block.
    */
  def sequenceNumber[F[_]: Sync: BlockDagStorage: Log](b: BlockMessage): F[ValidBlockProcessing] =
    for {
      dag                           <- BlockDagStorage[F].getRepresentation
      justifications                <- b.justifications.traverse(dag.lookupUnsafe(_))
      creatorJustificationOpt       = justifications.find(_.sender == b.sender)
      creatorJustificationSeqNumber = creatorJustificationOpt.map(_.seqNum).getOrElse(-1)
      number                        = b.seqNum
      result                        = creatorJustificationSeqNumber + 1 == number
      status <- if (result) {
                 BlockStatus.valid.asRight[BlockError].pure[F]
               } else {
                 for {
                   _ <- Log[F].warn(
                         ignore(
                           b,
                           s"seq number $number is not one more than creator justification number $creatorJustificationSeqNumber."
                         )
                       )
                 } yield BlockStatus.invalidSequenceNumber.asLeft[ValidBlock]
               }
    } yield status

  // Validator should only process deploys from its own shard
  def deploysShardIdentifier[F[_]: Monad: Log](
      b: BlockMessage,
      shardId: String
  ): F[ValidBlockProcessing] =
    if (b.body.deploys.forall(_.deploy.data.shardId == shardId)) {
      BlockStatus.valid.asRight[BlockError].pure
    } else {
      for {
        _ <- Log[F].warn(ignore(b, s"not for all deploys shard identifier is $shardId."))
      } yield BlockStatus.invalidDeployShardId.asLeft[ValidBlock]
    }

  def blockHash[F[_]: Applicative: Log](b: BlockMessage): F[Boolean] = {
    val blockHashComputed = ProtoUtil.hashBlock(b)
    if (b.blockHash == blockHashComputed)
      true.pure
    else {
      val computedHashString = PrettyPrinter.buildString(blockHashComputed)
      val hashString         = PrettyPrinter.buildString(b.blockHash)
      for {
        _ <- Log[F].warn(
              ignore(
                b,
                s"block hash $hashString does not match to computed value $computedHashString."
              )
            )
      } yield false
    }
  }

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
      if (isValidOpt.getOrElse(true)) BlockStatus.valid.asRight[BlockError]
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
  def neglectedInvalidBlock[F[_]: Applicative: BlockDagStorage](
      block: BlockMessage,
      s: CasperSnapshot
  ): F[ValidBlockProcessing] =
    for {
      justifications    <- block.justifications.flatTraverse(s.dag.lookup(_).map(_.toList))
      invalidValidators = justifications.filter(_.invalid).map(b => b.sender)
      neglectedInvalidJustification = invalidValidators.exists { invalidValidator =>
        val slashedValidatorBond = bonds(block).find(_.validator == invalidValidator)
        slashedValidatorBond match {
          case Some(bond) => bond.stake > 0
          case None       => false
        }
      }
      result = if (neglectedInvalidJustification) {
        BlockStatus.neglectedInvalidBlock.asLeft[ValidBlock]
      } else {
        BlockStatus.valid.asRight[BlockError]
      }
    } yield result

  def bondsCache[F[_]: Log: Concurrent](
      b: BlockMessage,
      runtimeManager: RuntimeManager[F]
  ): F[ValidBlockProcessing] = {
    val bonds          = ProtoUtil.bonds(b)
    val tuplespaceHash = ProtoUtil.postStateHash(b)

    runtimeManager.computeBonds(tuplespaceHash).attempt.flatMap {
      case Right(computedBonds) =>
        if (bonds.toSet == computedBonds.toSet) {
          BlockStatus.valid.asRight[BlockError].pure
        } else {
          for {
            _ <- Log[F].warn(
                  "Bonds in proof of stake contract do not match block's bond cache."
                )
          } yield BlockStatus.invalidBondsCache.asLeft[ValidBlock]
        }
      case Left(ex: Throwable) =>
        for {
          _ <- Log[F].warn(s"Failed to compute bonds from tuplespace hash ${ex.getMessage}")
        } yield BlockError.BlockException(ex).asLeft[ValidBlock]
    }
  }

  /**
    * All of deploys must have greater or equal phloPrice then minPhloPrice
    */
  def phloPrice[F[_]: Log: Concurrent](
      b: BlockMessage,
      minPhloPrice: Long
  ): F[ValidBlockProcessing] =
    if (b.body.deploys.forall(_.deploy.data.phloPrice >= minPhloPrice)) {
      BlockStatus.valid.asRight[BlockError].pure
    } else {
      BlockStatus.lowDeployCost.asLeft[ValidBlock].pure
    }
}
