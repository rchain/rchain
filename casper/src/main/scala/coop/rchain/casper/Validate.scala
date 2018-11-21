package coop.rchain.casper

import cats.effect.Sync
import cats.{Applicative, Monad}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol.Event.EventInstance
import coop.rchain.casper.protocol.{ApprovedBlock, BlockMessage, Justification}
import coop.rchain.casper.util.{DagOperations, ProtoUtil}
import coop.rchain.casper.util.ProtoUtil.bonds
import coop.rchain.casper.util.rholang.{InterpreterUtil, RuntimeManager}
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.catscontrib.Capture
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.shared._
import monix.execution.Scheduler

import scala.util.{Failure, Success, Try}

object Validate {
  type PublicKey = Array[Byte]
  type Data      = Array[Byte]
  type Signature = Array[Byte]

  val DRIFT                                 = 15000 // 15 seconds
  private implicit val logSource: LogSource = LogSource(this.getClass)
  val signatureVerifiers: Map[String, (Data, Signature, PublicKey) => Boolean] =
    Map(
      "ed25519" -> Ed25519.verify
    )

  def signature(d: Data, sig: protocol.Signature): Boolean =
    signatureVerifiers.get(sig.algorithm).fold(false) { verify =>
      verify(d, sig.sig.toByteArray, sig.publicKey.toByteArray)
    }

  def ignore(b: BlockMessage, reason: String): String =
    s"Ignoring block ${PrettyPrinter.buildString(b.blockHash)} because $reason"

  def approvedBlock[F[_]: Applicative: Log](
      a: ApprovedBlock,
      requiredValidators: Set[ByteString]
  ): F[Boolean] = {
    val maybeSigData = for {
      c     <- a.candidate
      bytes = c.toByteArray
    } yield Blake2b256.hash(bytes)

    val requiredSigs = a.candidate.map(_.requiredSigs).getOrElse(0)

    maybeSigData match {
      case Some(sigData) =>
        val validatedSigs =
          (for {
            s      <- a.sigs
            verify <- signatureVerifiers.get(s.algorithm)
            pk     = s.publicKey
            if verify(sigData, s.sig.toByteArray, pk.toByteArray)
          } yield pk).toSet

        if (validatedSigs.size >= requiredSigs && requiredValidators.forall(validatedSigs.contains))
          true.pure[F]
        else
          Log[F]
            .warn("Received invalid ApprovedBlock message not containing enough valid signatures.")
            .map(_ => false)

      case None =>
        Log[F]
          .warn("Received invalid ApprovedBlock message not containing any candidate.")
          .map(_ => false)
    }
  }

  def blockSignature[F[_]: Applicative: Log](b: BlockMessage): F[Boolean] =
    signatureVerifiers
      .get(b.sigAlgorithm)
      .map(verify => {
        Try(verify(b.blockHash.toByteArray, b.sig.toByteArray, b.sender.toByteArray)) match {
          case Success(true) => true.pure[F]
          case _             => Log[F].warn(ignore(b, "signature is invalid.")).map(_ => false)
        }
      }) getOrElse {
      for {
        _ <- Log[F].warn(ignore(b, s"signature algorithm ${b.sigAlgorithm} is unsupported."))
      } yield false
    }

  def blockSender[F[_]: Monad: Log: BlockStore](
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag
  ): F[Boolean] =
    if (b == genesis) {
      true.pure[F] //genesis block has a valid sender
    } else {
      for {
        weight <- ProtoUtil.weightFromSender[F](b)
        result <- if (weight > 0) true.pure[F]
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
    } else if (b.header.isEmpty) {
      for {
        _ <- Log[F].warn(ignore(b, s"block header is missing."))
      } yield false
    } else if (b.body.isEmpty) {
      for {
        _ <- Log[F].warn(ignore(b, s"block body is missing."))
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
    } else if (b.header.get.postStateHash.isEmpty) {
      for {
        _ <- Log[F].warn(ignore(b, s"block post state hash is empty."))
      } yield false
    } else if (b.header.get.deploysHash.isEmpty) {
      for {
        _ <- Log[F].warn(ignore(b, s"block new code hash is empty."))
      } yield false
    } else if (b.body.get.state.isEmpty) {
      for {
        _ <- Log[F].warn(ignore(b, s"block post state is missing."))
      } yield false
    } else if (b.body.get.deploys.flatMap(_.log).exists(_.eventInstance == EventInstance.Empty)) {
      for {
        _ <- Log[F].warn(ignore(b, s"one of block comm reduction events is empty."))
      } yield false
    } else {
      true.pure[F]
    }

  def version[F[_]: Applicative: Log](b: BlockMessage, version: Long): F[Boolean] = {
    val blockVersion = b.header.get.version
    if (blockVersion == version) {
      true.pure[F]
    } else {
      Log[F].warn(
        ignore(
          b,
          s"received block version $blockVersion is the expected version $version."
        )
      ) *> false.pure[F]
    }
  }

  /*
   * TODO: Double check ordering of validity checks
   * TODO: Add check for missing fields
   * TODO: Check that justifications follow from bonds of creator justification
   */
  def blockSummary[F[_]: Monad: Log: Time: BlockStore](
      block: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag,
      shardId: String
  ): F[Either[BlockStatus, ValidBlock]] =
    for {
      blockHashStatus   <- Validate.blockHash[F](block)
      deployCountStatus <- blockHashStatus.traverse(_ => Validate.deployCount[F](block))
      missingBlockStatus <- deployCountStatus.joinRight.traverse(
                             _ => Validate.missingBlocks[F](block, dag)
                           )
      timestampStatus <- missingBlockStatus.joinRight.traverse(
                          _ => Validate.timestamp[F](block, dag)
                        )
      repeatedDeployStatus <- timestampStatus.joinRight.traverse(
                               _ => Validate.repeatDeploy[F](block, dag)
                             )
      blockNumberStatus <- repeatedDeployStatus.joinRight.traverse(
                            _ => Validate.blockNumber[F](block)
                          )
      followsStatus <- blockNumberStatus.joinRight.traverse(
                        _ => Validate.justificationFollows[F](block, genesis, dag)
                      )
      parentsStatus <- followsStatus.joinRight.traverse(
                        _ => Validate.parents[F](block, genesis, dag)
                      )
      sequenceNumberStatus <- parentsStatus.joinRight.traverse(
                               _ => Validate.sequenceNumber[F](block, dag)
                             )
      justificationRegressionsStatus <- sequenceNumberStatus.joinRight.traverse(
                                         _ =>
                                           Validate.justificationRegressions[F](block, genesis, dag)
                                       )
      shardIdentifierStatus <- justificationRegressionsStatus.joinRight.traverse(
                                _ => Validate.shardIdentifier[F](block, shardId)
                              )
    } yield shardIdentifierStatus.joinRight

  /**
    * Works with either efficient justifications or full explicit justifications
    */
  def missingBlocks[F[_]: Monad: Log: BlockStore](
      block: BlockMessage,
      dag: BlockDag
  ): F[Either[InvalidBlock, ValidBlock]] =
    for {
      parentsPresent <- ProtoUtil.parentHashes(block).toList.forallM(p => BlockStore[F].contains(p))
      justificationsPresent <- block.justifications.toList
                                .forallM(j => BlockStore[F].contains(j.latestBlockHash))
      result <- if (parentsPresent && justificationsPresent) {
                 Applicative[F].pure(Right(Valid))
               } else {
                 for {
                   _ <- Log[F].debug(
                         s"Fetching missing dependencies for ${PrettyPrinter.buildString(block.blockHash)}."
                       )
                 } yield Left(MissingBlocks)
               }
    } yield result

  /**
    * Validate no deploy by the same (user, millisecond timestamp)
    * has been produced in the chain
    *
    * Agnostic of non-parent justifications
    */
  def repeatDeploy[F[_]: Monad: Log: BlockStore](
      block: BlockMessage,
      dag: BlockDag
  ): F[Either[InvalidBlock, ValidBlock]] = {
    val deployKeySet = (for {
      bd <- block.body.toList
      d  <- bd.deploys.flatMap(_.deploy)
      r  <- d.raw.toList
    } yield (r.user, r.timestamp)).toSet

    for {
      initParents <- ProtoUtil.unsafeGetParents[F](block)
      duplicatedBlock <- DagOperations
                          .bfTraverseF[F, BlockMessage](initParents)(ProtoUtil.unsafeGetParents[F])
                          .find(
                            _.body.exists(
                              _.deploys
                                .flatMap(_.deploy)
                                .exists(
                                  _.raw.exists(p => deployKeySet.contains((p.user, p.timestamp)))
                                )
                            )
                          )
      result <- duplicatedBlock match {
                 case Some(b) =>
                   for {
                     _ <- Log[F].warn(
                           ignore(
                             block,
                             s"found deploy by the same (user, millisecond timestamp) produced in the block(${b.blockHash})"
                           )
                         )
                   } yield Left(InvalidRepeatDeploy)
                 case None => Applicative[F].pure(Right(Valid))
               }
    } yield result
  }

  // This is not a slashable offence
  def timestamp[F[_]: Monad: Log: Time: BlockStore](
      b: BlockMessage,
      dag: BlockDag
  ): F[Either[InvalidBlock, ValidBlock]] =
    for {
      currentTime  <- Time[F].currentMillis
      timestamp    = b.header.get.timestamp
      beforeFuture = currentTime + DRIFT >= timestamp
      latestParentTimestamp <- ProtoUtil.parentHashes(b).toList.foldM(0L) {
                                case (latestTimestamp, parentHash) =>
                                  ProtoUtil
                                    .unsafeGetBlock[F](parentHash)
                                    .map(parent => {
                                      val timestamp =
                                        parent.header.fold(latestTimestamp)(_.timestamp)
                                      math.max(latestTimestamp, timestamp)
                                    })
                              }
      afterLatestParent = timestamp >= latestParentTimestamp
      result <- if (beforeFuture && afterLatestParent) {
                 Applicative[F].pure(Right(Valid))
               } else {
                 for {
                   _ <- Log[F].warn(
                         ignore(
                           b,
                           s"block timestamp $timestamp is not between latest parent block time and current time."
                         )
                       )
                 } yield Left(InvalidUnslashableBlock)
               }
    } yield result

  // Agnostic of non-parent justifications
  def blockNumber[F[_]: Monad: Log: BlockStore](
      b: BlockMessage
  ): F[Either[InvalidBlock, ValidBlock]] =
    for {
      parents <- ProtoUtil.unsafeGetParents[F](b)
      maxBlockNumber = parents.foldLeft(-1L) {
        case (acc, p) => math.max(acc, ProtoUtil.blockNumber(p))
      }
      number = ProtoUtil.blockNumber(b)
      result = maxBlockNumber + 1 == number
      status <- if (result) {
                 Applicative[F].pure(Right(Valid))
               } else {
                 val logMessage =
                   if (parents.isEmpty)
                     s"block number $number is not zero, but block has no parents."
                   else
                     s"block number $number is not one more than maximum parent number $maxBlockNumber."
                 for {
                   _ <- Log[F].warn(ignore(b, logMessage))
                 } yield Left(InvalidBlockNumber)
               }
    } yield status

  /**
    * Works with either efficient justifications or full explicit justifications.
    * Specifically, with efficient justifications, if a block B doesn't update its
    * creator justification, this check will fail as expected. The exception is when
    * B's creator justification is the genesis block.
    */
  def sequenceNumber[F[_]: Monad: Log: BlockStore](
      b: BlockMessage,
      dag: BlockDag
  ): F[Either[InvalidBlock, ValidBlock]] =
    for {
      creatorJustificationSeqNumber <- ProtoUtil.creatorJustification(b).foldM(-1) {
                                        case (_, Justification(_, latestBlockHash)) =>
                                          for {
                                            latestBlock <- ProtoUtil
                                                            .unsafeGetBlock[F](latestBlockHash)
                                          } yield latestBlock.seqNum
                                      }
      number = b.seqNum
      result = creatorJustificationSeqNumber + 1 == number
      status <- if (result) {
                 Applicative[F].pure(Right(Valid))
               } else {
                 for {
                   _ <- Log[F].warn(
                         ignore(
                           b,
                           s"seq number $number is not one more than creator justification number $creatorJustificationSeqNumber."
                         )
                       )
                 } yield Left(InvalidSequenceNumber)
               }
    } yield status

  // Agnostic of justifications
  def shardIdentifier[F[_]: Monad: Log: BlockStore](
      b: BlockMessage,
      shardId: String
  ): F[Either[InvalidBlock, ValidBlock]] =
    if (b.shardId == shardId) {
      Applicative[F].pure(Right(Valid))
    } else {
      for {
        _ <- Log[F].warn(
              ignore(b, s"got shard identifier ${b.shardId} while $shardId was expected.")
            )
      } yield Left(InvalidShardId)
    }

  def blockHash[F[_]: Applicative: Log](b: BlockMessage): F[Either[InvalidBlock, ValidBlock]] = {
    val blockHashComputed = ProtoUtil.hashSignedBlock(
      b.header.get,
      b.sender,
      b.sigAlgorithm,
      b.seqNum,
      b.shardId,
      b.extraBytes
    )
    val deployHashComputed    = ProtoUtil.protoSeqHash(b.body.get.deploys)
    val postStateHashComputed = ProtoUtil.protoHash(b.body.get.state.get)
    if (b.blockHash == blockHashComputed &&
        b.header.get.deploysHash == deployHashComputed &&
        b.header.get.postStateHash == postStateHashComputed) {
      Applicative[F].pure(Right(Valid))
    } else {
      for {
        _ <- Log[F].warn(ignore(b, s"block hash does not match to computed value."))
      } yield Left(InvalidBlockHash)
    }
  }

  def deployCount[F[_]: Applicative: Log](b: BlockMessage): F[Either[InvalidBlock, ValidBlock]] =
    if (b.header.get.deployCount == b.body.get.deploys.length) {
      Applicative[F].pure(Right(Valid))
    } else {
      for {
        _ <- Log[F].warn(ignore(b, s"block deploy count does not match to the amount of deploys."))
      } yield Left(InvalidDeployCount)
    }

  /**
    * Works only with fully explicit justifications.
    */
  def parents[F[_]: Monad: Log: BlockStore](
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag
  ): F[Either[InvalidBlock, ValidBlock]] = {
    val maybeParentHashes = ProtoUtil.parentHashes(b)
    val parentHashes = maybeParentHashes match {
      case hashes if hashes.isEmpty => Seq(genesis.blockHash)
      case hashes                   => hashes
    }

    for {
      latestMessagesHashes <- ProtoUtil.toLatestMessageHashes(b.justifications).pure[F]
      estimate             <- Estimator.tips[F](dag, genesis.blockHash, latestMessagesHashes)
      computedParents      <- ProtoUtil.chooseNonConflicting[F](estimate, genesis, dag)
      computedParentHashes = computedParents.map(_.blockHash)
      status <- if (parentHashes == computedParentHashes)
                 Applicative[F].pure(Right(Valid))
               else
                 for {
                   _ <- Log[F].warn(
                         ignore(b, "block parents did not match estimate based on justification.")
                       )
                 } yield Left(InvalidParents)
    } yield status
  }

  /*
   * This check must come before Validate.parents
   */
  def justificationFollows[F[_]: Monad: Log: BlockStore](
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag
  ): F[Either[InvalidBlock, ValidBlock]] = {
    val justifiedValidators = b.justifications.map(_.validator).toSet
    val mainParentHash      = ProtoUtil.parentHashes(b).head
    for {
      mainParent       <- ProtoUtil.unsafeGetBlock[F](mainParentHash)
      bondedValidators = ProtoUtil.bonds(mainParent).map(_.validator).toSet
      status <- if (bondedValidators == justifiedValidators) {
                 Applicative[F].pure(Right(Valid))
               } else {
                 val justifiedValidatorsPP = justifiedValidators.map(PrettyPrinter.buildString(_))
                 val bondedValidatorsPP    = bondedValidators.map(PrettyPrinter.buildString(_))
                 for {
                   _ <- Log[F].warn(
                         ignore(
                           b,
                           s"the justified validators, ${justifiedValidatorsPP}, do not match the bonded validators, ${bondedValidatorsPP}."
                         )
                       )
                 } yield Left(InvalidFollows)
               }
    } yield status
  }

  /*
   * When we switch between equivocation forks for a slashed validator, we will potentially get a
   * justification regression that is valid. We cannot ignore this as the creator only drops the
   * justification block created by the equivocator on the following block.
   * Hence, we ignore justification regressions involving the block's sender and
   * let checkEquivocations handle it instead.
   */
  def justificationRegressions[F[_]: Monad: Log: BlockStore](
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag
  ): F[Either[InvalidBlock, ValidBlock]] = {
    val latestMessagesOfBlock             = ProtoUtil.toLatestMessageHashes(b.justifications)
    val maybeLatestMessagesFromSenderView = dag.latestMessagesOfLatestMessages.get(b.sender)
    maybeLatestMessagesFromSenderView match {
      case Some(latestMessagesFromSenderView) =>
        justificationRegressionsAux[F](
          b,
          latestMessagesOfBlock,
          latestMessagesFromSenderView,
          genesis
        )
      case None =>
        // We cannot have a justification regression if we don't have a previous latest message from sender
        Applicative[F].pure(Right(Valid))
    }
  }

  private def justificationRegressionsAux[F[_]: Monad: Log: BlockStore](
      b: BlockMessage,
      latestMessagesOfBlock: Map[Validator, BlockHash],
      latestMessagesFromSenderView: Map[Validator, BlockHash],
      genesis: BlockMessage
  ): F[Either[InvalidBlock, ValidBlock]] =
    for {
      containsJustificationRegression <- latestMessagesOfBlock.toList.existsM {
                                          case (validator, currentBlockJustificationHash) =>
                                            if (validator == b.sender) {
                                              // We let checkEquivocations handle this case
                                              false.pure[F]
                                            } else {
                                              val previousBlockJustificationHash =
                                                latestMessagesFromSenderView.getOrElse(
                                                  validator,
                                                  genesis.blockHash
                                                )
                                              isJustificationRegression[F](
                                                currentBlockJustificationHash,
                                                previousBlockJustificationHash
                                              )
                                            }
                                        }
      status <- if (containsJustificationRegression) {
                 for {
                   _ <- Log[F].warn(ignore(b, "block contains justification regressions."))
                 } yield Left(JustificationRegression)
               } else {
                 Applicative[F].pure(Right(Valid))
               }
    } yield status

  private def isJustificationRegression[F[_]: Monad: Log: BlockStore](
      currentBlockJustificationHash: BlockHash,
      previousBlockJustificationHash: BlockHash
  ): F[Boolean] =
    for {
      currentBlockJustification <- ProtoUtil
                                    .unsafeGetBlock[F](currentBlockJustificationHash)
      previousBlockJustification <- ProtoUtil
                                     .unsafeGetBlock[F](previousBlockJustificationHash)
    } yield
      if (currentBlockJustification.seqNum < previousBlockJustification.seqNum) {
        true
      } else {
        false
      }

  def transactions[F[_]: Sync: Log: BlockStore](
      block: BlockMessage,
      dag: BlockDag,
      emptyStateHash: StateHash,
      runtimeManager: RuntimeManager
  )(implicit scheduler: Scheduler): F[Either[BlockStatus, ValidBlock]] =
    for {
      maybeStateHash <- InterpreterUtil
                         .validateBlockCheckpoint[F](
                           block,
                           dag,
                           runtimeManager
                         )
    } yield
      maybeStateHash match {
        case Left(ex)       => Left(ex)
        case Right(Some(_)) => Right(Valid)
        case Right(None)    => Left(InvalidTransaction)
      }

  /**
    * If block contains an invalid justification block B and the creator of B is still bonded,
    * return a RejectableBlock. Otherwise return an IncludeableBlock.
    */
  def neglectedInvalidBlock[F[_]: Applicative](
      block: BlockMessage,
      invalidBlockTracker: Set[BlockHash]
  ): F[Either[InvalidBlock, ValidBlock]] = {
    val invalidJustifications = block.justifications.filter(
      justification => invalidBlockTracker.contains(justification.latestBlockHash)
    )
    val neglectedInvalidJustification = invalidJustifications.exists { justification =>
      val slashedValidatorBond = bonds(block).find(_.validator == justification.validator)
      slashedValidatorBond match {
        case Some(bond) => bond.stake > 0
        case None       => false
      }
    }
    if (neglectedInvalidJustification) {
      Applicative[F].pure(Left(NeglectedInvalidBlock))
    } else {
      Applicative[F].pure(Right(Valid))
    }
  }

  def bondsCache[F[_]: Applicative: Log](b: BlockMessage, runtimeManager: RuntimeManager)(
      implicit scheduler: Scheduler
  ): F[Either[InvalidBlock, ValidBlock]] = {
    val bonds = ProtoUtil.bonds(b)
    ProtoUtil.tuplespace(b) match {
      case Some(tuplespaceHash) =>
        Try(runtimeManager.computeBonds(tuplespaceHash)) match {
          case Success(computedBonds) =>
            if (bonds.toSet == computedBonds.toSet) {
              Applicative[F].pure(Right(Valid))
            } else {
              for {
                _ <- Log[F].warn(
                      "Bonds in proof of stake contract do not match block's bond cache."
                    )
              } yield Left(InvalidBondsCache)
            }
          case Failure(ex: Throwable) =>
            for {
              _ <- Log[F].warn(s"Failed to compute bonds from tuplespace hash ${ex.getMessage}")
            } yield Left(InvalidBondsCache)
        }
      case None =>
        for {
          _ <- Log[F].warn(s"Block ${PrettyPrinter.buildString(b)} is missing a tuplespace hash.")
        } yield Left(InvalidBondsCache)
    }
  }
}
