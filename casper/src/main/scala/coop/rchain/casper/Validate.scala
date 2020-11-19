package coop.rchain.casper

import cats.data.EitherT
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import cats.{Applicative, Monad}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.protocol.{ApprovedBlock, BlockMessage, Justification}
import coop.rchain.casper.util.ProtoUtil.bonds
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator
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

  def signature(d: Data, sig: protocol.Signature): Boolean =
    signatureVerifiers.get(sig.algorithm).fold(false) { verify =>
      verify(d, sig.sig.toByteArray, sig.publicKey.toByteArray)
    }

  def ignore(b: BlockMessage, reason: String): String =
    s"Ignoring block ${PrettyPrinter.buildString(b.blockHash)} because $reason"

  def approvedBlock[F[_]: Sync: Log](
      approvedBlock: ApprovedBlock
  ): F[Boolean] = {
    val candidateBytesDigest = Blake2b256.hash(approvedBlock.candidate.toProto.toByteArray)

    val requiredSignatures = approvedBlock.candidate.requiredSigs

    val signatures =
      (for {
        signature <- approvedBlock.sigs
        verifySig <- signatureVerifiers.get(signature.algorithm)
        publicKey = signature.publicKey
        if verifySig(candidateBytesDigest, signature.sig.toByteArray, publicKey.toByteArray)
      } yield publicKey).toSet

    val logMsg = signatures.isEmpty match {
      case true => s"ApprovedBlock is self-signed by ceremony master."
      case false =>
        s"ApprovedBlock is signed by: ${signatures
          .map(x => "<" + Base16.encode(x.toByteArray).substring(0, 10) + "...>")
          .mkString(", ")}"
    }

    for {
      _          <- Log[F].info(logMsg)
      enoughSigs = (signatures.size >= requiredSignatures)
      _ <- Log[F]
            .warn(
              "Received invalid ApprovedBlock message not containing enough valid signatures."
            )
            .whenA(!enoughSigs)
    } yield enoughSigs
  }

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

  def blockSenderHasWeight[F[_]: Monad: Log: BlockStore](
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDagRepresentation[F]
  ): F[Boolean] =
    if (b == genesis) {
      true.pure //genesis block has a valid sender
    } else {
      for {
        weight <- ProtoUtil.weightFromSender(b)
        result <- if (weight > 0) true.pure
                 else
                   for {
                     _ <- Log[F].warn(
                           ignore(
                             b,
                             s"block creator ${PrettyPrinter.buildString(b.sender)} has 0 weight."
                           )
                         )
                   } yield false
      } yield result
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
    val blockVersion = b.header.version
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

  /*
   * TODO: Double check ordering of validity checks
   */
  def blockSummary[F[_]: Sync: Log: Time: BlockStore: Metrics: Span: Estimator](
      block: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDagRepresentation[F],
      shardId: String,
      expirationThreshold: Int
  ): F[ValidBlockProcessing] =
    (for {
      _ <- EitherT.liftF(Span[F].mark("before-block-hash-validation"))
      _ <- EitherT(Validate.blockHash(block))
      _ <- EitherT.liftF(Span[F].mark("before-timestamp-validation"))
      _ <- EitherT(Validate.timestamp(block, dag))
      _ <- EitherT.liftF(Span[F].mark("before-repeat-deploy-validation"))
      _ <- EitherT(Validate.repeatDeploy(block, dag, expirationThreshold))
      _ <- EitherT.liftF(Span[F].mark("before-block-number-validation"))
      _ <- EitherT(Validate.blockNumber(block, dag))
      _ <- EitherT.liftF(Span[F].mark("before-future-transaction-validation"))
      _ <- EitherT(Validate.futureTransaction(block))
      _ <- EitherT.liftF(Span[F].mark("before-transaction-expired-validation"))
      _ <- EitherT(Validate.transactionExpiration(block, expirationThreshold))
      _ <- EitherT.liftF(Span[F].mark("before-justification-follows-validation"))
      _ <- EitherT(Validate.justificationFollows(block, genesis, dag))
      _ <- EitherT.liftF(Span[F].mark("before-parents-validation"))
      _ <- EitherT(Validate.parents(block, genesis, dag))
      _ <- EitherT.liftF(Span[F].mark("before-sequence-number-validation"))
      _ <- EitherT(Validate.sequenceNumber(block, dag))
      _ <- EitherT.liftF(Span[F].mark("before-justification-regression-validation"))
      _ <- EitherT(Validate.justificationRegressions(block, dag))
      _ <- EitherT.liftF(Span[F].mark("before-shard-identifier-validation"))
      s <- EitherT(Validate.shardIdentifier(block, shardId))
    } yield s).value

  /**
    * Validate no deploy with the same sig has been produced in the chain
    *
    * Agnostic of non-parent justifications
    */
  def repeatDeploy[F[_]: Sync: Log: BlockStore: Span](
      block: BlockMessage,
      dag: BlockDagRepresentation[F],
      expirationThreshold: Int
  ): F[ValidBlockProcessing] = {
    import cats.instances.option._

    val deployKeySet = block.body.deploys.map(_.deploy.sig).toSet

    for {
      _                   <- Span[F].mark("before-repeat-deploy-get-parents")
      blockMetadata       = BlockMetadata.fromBlock(block, invalid = false)
      initParents         <- ProtoUtil.getParentsMetadata(blockMetadata, dag)
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
                                               dag
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
      b: BlockMessage,
      dag: BlockDagRepresentation[F]
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
  def blockNumber[F[_]: Sync: Log](
      b: BlockMessage,
      dag: BlockDagRepresentation[F]
  ): F[ValidBlockProcessing] = {
    import cats.instances.list._

    for {
      parents <- ProtoUtil.parentHashes(b).traverse { parentHash =>
                  dag.lookup(parentHash).flatMap {
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
  @SuppressWarnings(Array("org.wartremover.warts.Throw")) // TODO remove throw
  def sequenceNumber[F[_]: Monad: Log](
      b: BlockMessage,
      dag: BlockDagRepresentation[F]
  ): F[ValidBlockProcessing] = {
    import cats.instances.option._

    for {
      creatorJustificationSeqNumber <- ProtoUtil.creatorJustification(b).foldM(-1) {
                                        case (_, Justification(_, latestBlockHash)) =>
                                          dag.lookup(latestBlockHash).map {
                                            case Some(block) =>
                                              block.seqNum
                                            case None =>
                                              throw new Exception(
                                                s"Latest block hash ${PrettyPrinter.buildString(latestBlockHash)} is missing from block dag store."
                                              )
                                          }
                                      }
      number = b.seqNum
      result = creatorJustificationSeqNumber + 1 == number
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
  }

  // Agnostic of justifications
  def shardIdentifier[F[_]: Monad: Log: BlockStore](
      b: BlockMessage,
      shardId: String
  ): F[ValidBlockProcessing] =
    if (b.shardId == shardId) {
      BlockStatus.valid.asRight[BlockError].pure
    } else {
      for {
        _ <- Log[F].warn(
              ignore(b, s"got shard identifier ${b.shardId} while $shardId was expected.")
            )
      } yield BlockStatus.invalidShardId.asLeft[ValidBlock]
    }

  // TODO: Double check this validation isn't shadowed by the blockSignature validation
  def blockHash[F[_]: Applicative: Log](b: BlockMessage): F[ValidBlockProcessing] = {
    val blockHashComputed = ProtoUtil.hashSignedBlock(b)
    if (b.blockHash == blockHashComputed)
      BlockStatus.valid.asRight[BlockError].pure
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
      } yield BlockStatus.invalidBlockHash.asLeft[ValidBlock]
    }
  }

  /**
    * Works only with fully explicit justifications.
    */
  def parents[F[_]: Sync: Log: BlockStore: Metrics: Span: Estimator](
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDagRepresentation[F]
  ): F[ValidBlockProcessing] = {
    val maybeParentHashes = ProtoUtil.parentHashes(b)
    val parentHashes = maybeParentHashes match {
      case hashes if hashes.isEmpty => Seq(genesis.blockHash)
      case hashes                   => hashes
    }

    for {
      latestMessagesHashes <- ProtoUtil.toLatestMessageHashes(b.justifications).pure
      tipHashes            <- Estimator[F].tips(dag, genesis, latestMessagesHashes)
      computedParents      <- EstimatorHelper.chooseNonConflicting(tipHashes, dag)
      computedParentHashes = computedParents.map(_.blockHash)
      status <- if (parentHashes == computedParentHashes) {
                 BlockStatus.valid.asRight[BlockError].pure
               } else {
                 val parentsString =
                   parentHashes.map(hash => PrettyPrinter.buildString(hash)).mkString(",")
                 val estimateString =
                   computedParentHashes.map(hash => PrettyPrinter.buildString(hash)).mkString(",")
                 val justificationString = latestMessagesHashes.values
                   .map(hash => PrettyPrinter.buildString(hash))
                   .mkString(",")
                 val message =
                   s"block parents ${parentsString} did not match estimate ${estimateString} based on justification ${justificationString}."
                 for {
                   _ <- Log[F].warn(
                         ignore(b, message)
                       )
                 } yield BlockStatus.invalidParents.asLeft[ValidBlock]
               }
    } yield status
  }

  /*
   * This check must come before Validate.parents
   */
  def justificationFollows[F[_]: Sync: Log: BlockStore](
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDagRepresentation[F]
  ): F[ValidBlockProcessing] = {
    val justifiedValidators = b.justifications.map(_.validator).toSet
    val mainParentHash      = ProtoUtil.parentHashes(b).head
    for {
      mainParent       <- BlockStore[F].getUnsafe(mainParentHash)
      bondedValidators = ProtoUtil.bonds(mainParent).map(_.validator).toSet
      status <- if (bondedValidators == justifiedValidators) {
                 BlockStatus.valid.asRight[BlockError].pure
               } else {
                 val justifiedValidatorsPP = justifiedValidators.map(PrettyPrinter.buildString)
                 val bondedValidatorsPP    = bondedValidators.map(PrettyPrinter.buildString)
                 for {
                   _ <- Log[F].warn(
                         ignore(
                           b,
                           s"the justified validators, ${justifiedValidatorsPP}, do not match the bonded validators, ${bondedValidatorsPP}."
                         )
                       )
                 } yield BlockStatus.invalidFollows.asLeft[ValidBlock]
               }
    } yield status
  }

  /**
    * Justification regression check.
    * Compares justifications that has been already used by sender and recorded in the DAG with
    * justifications used by the same sender in new block `b` and assures that there is no
    * regression.
    *
    * When we switch between equivocation forks for a slashed validator, we will potentially get a
    * justification regression that is valid. We cannot ignore this as the creator only drops the
    * justification block created by the equivocator on the following block.
    * Hence, we ignore justification regressions involving the block's sender and
    * let checkEquivocations handle it instead.
    */
  def justificationRegressions[F[_]: Sync: Log](
      b: BlockMessage,
      dag: BlockDagRepresentation[F]
  ): F[ValidBlockProcessing] =
    dag.latestMessage(b.sender).flatMap {
      // `b` is first message from sender of `b`, so regression is not possible
      case None =>
        BlockStatus.valid.asRight[BlockError].pure
      // Latest Message from sender of `b` is present in the DAG
      case Some(curSendersBlock) =>
        // Here we comparing view on the network by sender from the standpoint of
        // his previous block created (current Latest Message of sender)
        // and new block `b` (potential new Latest Message of sender)
        val newSendersBlock = b
        val newLMs          = ProtoUtil.toLatestMessageHashes(newSendersBlock.justifications)
        val curLMs          = ProtoUtil.toLatestMessageHashes(curSendersBlock.justifications)
        // We let checkEquivocations handle when sender uses old self-justification
        val newLMsNoSelf = newLMs.filterNot(_._1 == b.sender)

        // Check each Latest Message for regression (block seq num goes backwards)
        newLMsNoSelf.toList.tailRecM {
          // No more Latest Messages to check
          case Nil =>
            Applicative[F].pure(BlockStatus.valid.asRight.asRight)
          // Check if sender of LatestMessage does justification regression
          case newLM :: tail =>
            val (sender, newJustificationHash) = newLM
            val noSenderInCurLMs               = !curLMs.contains(sender)
            if (noSenderInCurLMs) {
              // If there is no justification to compare with - regression is not possible
              Applicative[F].pure(Left(tail))
            } else {
              val curJustificationHash = curLMs(sender)
              def logWarn(currentHash: BlockHash, regressiveHash: BlockHash): F[Unit] = {
                val msg = s"block ${PrettyPrinter.buildString(regressiveHash)} by ${PrettyPrinter
                  .buildString(sender)} has a lower sequence number than ${PrettyPrinter
                  .buildString(currentHash)}."
                Log[F].warn(ignore(b, msg))
              }
              // Compare and check for regression
              val regressionDetected = for {
                newJustification <- dag.lookupUnsafe(newJustificationHash)
                curJustification <- dag.lookupUnsafe(curJustificationHash)
                regression       = (!newJustification.invalid && newJustification.seqNum < curJustification.seqNum)
                _                <- logWarn(curJustificationHash, newJustificationHash).whenA(regression)
              } yield regression
              // Exit tailRecM when regression detected, or continue to check remaining Latest Messages.
              regressionDetected.ifM(
                BlockStatus.justificationRegression.asLeft.asRight.pure,
                Applicative[F].pure(Left(tail))
              )
            }
        }
    }

  /**
    * If block contains an invalid justification block B and the creator of B is still bonded,
    * return a RejectableBlock. Otherwise return an IncludeableBlock.
    */
  def neglectedInvalidBlock[F[_]: Applicative](
      block: BlockMessage,
      dag: BlockDagRepresentation[F]
  ): F[ValidBlockProcessing] = {
    import cats.instances.list._

    for {
      invalidJustifications <- block.justifications.filterA { justification =>
                                for {
                                  latestBlockOpt <- dag.lookup(justification.latestBlockHash)
                                } yield latestBlockOpt.exists(_.invalid)
                              }
      neglectedInvalidJustification = invalidJustifications.exists { justification =>
        val slashedValidatorBond = bonds(block).find(_.validator == justification.validator)
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
  }

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
}
