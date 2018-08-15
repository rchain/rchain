package coop.rchain.casper

import cats.{Applicative, Monad}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol.{ApprovedBlock, BlockMessage, Justification}
import coop.rchain.casper.util.DagOperations.bfTraverse
import coop.rchain.casper.util.{DagOperations, ProtoUtil}
import coop.rchain.casper.util.ProtoUtil.bonds
import coop.rchain.casper.util.rholang.{InterpreterUtil, RuntimeManager}
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.catscontrib.Capture
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.shared.{AtomicSyncVar, Log, LogSource, Time}
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
    s"CASPER: Ignoring block ${PrettyPrinter.buildString(b.blockHash)} because $reason"

  def approvedBlock[F[_]: Applicative: Log](a: ApprovedBlock,
                                            requiredValidators: Set[ByteString]): F[Boolean] = {
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
            .warn(
              "CASPER: Received invalid ApprovedBlock message not containing enough valid signatures.")
            .map(_ => false)

      case None =>
        Log[F]
          .warn("CASPER: Received invalid ApprovedBlock message not containing any candidate.")
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

  def blockSender[F[_]: Monad: Log: BlockStore](b: BlockMessage,
                                                genesis: BlockMessage,
                                                dag: BlockDag): F[Boolean] =
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
                             s"block creator ${PrettyPrinter.buildString(b.sender)} has 0 weight."))
                   } yield false
      } yield result
    }

  /*
   * TODO: Double check ordering of validity checks
   * TODO: Add check for missing fields
   * TODO: Check that justifications follow from bonds (especially beware of arbitrary droppings of bonded validators)
   * Justification regressions validation depends on sequence numbers being valid
   */
  def blockSummary[F[_]: Monad: Log: Time: BlockStore](
      block: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag): F[Either[InvalidBlock, ValidBlock]] =
    for {
      missingBlockStatus <- Validate.missingBlocks[F](block, dag)
      timestampStatus    <- missingBlockStatus.traverse(_ => Validate.timestamp[F](block, dag))
      repeatedDeployStatus <- timestampStatus.joinRight.traverse(_ =>
                               Validate.repeatDeploy[F](block, dag))
      blockNumberStatus <- repeatedDeployStatus.joinRight.traverse(_ =>
                            Validate.blockNumber[F](block, dag))
      parentsStatus <- blockNumberStatus.joinRight.traverse(_ =>
                        Validate.parents[F](block, genesis, dag))
      sequenceNumberStatus <- parentsStatus.joinRight.traverse(_ =>
                               Validate.sequenceNumber[F](block, dag))
    } yield sequenceNumberStatus.joinRight

  def missingBlocks[F[_]: Monad: Log: BlockStore](
      block: BlockMessage,
      dag: BlockDag): F[Either[InvalidBlock, ValidBlock]] =
    for {
      parentsPresent <- ProtoUtil.parentHashes(block).toList.forallM(p => BlockStore[F].contains(p))
      justificationsPresent <- block.justifications.toList.forallM(j =>
                                BlockStore[F].contains(j.latestBlockHash))
      result <- if (parentsPresent && justificationsPresent) {
                 Applicative[F].pure(Right(Valid))
               } else {
                 for {
                   _ <- Log[F].debug(
                         s"Fetching missing dependencies for ${PrettyPrinter.buildString(block.blockHash)}.")
                 } yield Left(MissingBlocks)
               }
    } yield result

  /**
    * Validate no deploy by the same (user, millisecond timestamp)
    * has been produced in the chain
    */
  def repeatDeploy[F[_]: Monad: Log: BlockStore](
      block: BlockMessage,
      dag: BlockDag): F[Either[InvalidBlock, ValidBlock]] = {
    val deployKeySet = (for {
      bd <- block.body.toList
      d  <- bd.newCode.flatMap(_.deploy)
      r  <- d.raw.toList
    } yield (r.user, r.timestamp)).toSet

    for {
      initParents <- ProtoUtil.unsafeGetParents[F](block)
      parentBlocksAncestors <- DagOperations
                                .bfTraverseF[F, BlockMessage](initParents)(
                                  ProtoUtil.unsafeGetParents[F])
                                .toList
      duplicatedBlock = parentBlocksAncestors.find(
        _.body.exists(
          _.newCode
            .flatMap(_.deploy)
            .exists(
              _.raw.exists(p => deployKeySet.contains((p.user, p.timestamp)))
            )
        )
      )
      result <- duplicatedBlock match {
                 case Some(b) =>
                   for {
                     _ <- Log[F].warn(ignore(
                           block,
                           s"found deploy by the same (user, millisecond timestamp) produced in the block(${b.blockHash})"))
                   } yield Left(InvalidRepeatDeploy)
                 case None => Applicative[F].pure(Right(Valid))
               }
    } yield result
  }

  // This is not a slashable offence
  def timestamp[F[_]: Monad: Log: Time: BlockStore](
      b: BlockMessage,
      dag: BlockDag): F[Either[InvalidBlock, ValidBlock]] =
    for {
      currentTime  <- Time[F].currentMillis
      timestamp    = b.header.get.timestamp
      beforeFuture = currentTime + DRIFT >= timestamp
      latestParentTimestamp <- ProtoUtil.parentHashes(b).toList.foldM(0L) {
                                case (latestTimestamp, parentHash) =>
                                  for {
                                    parent    <- ProtoUtil.unsafeGetBlock[F](parentHash)
                                    timestamp = parent.header.get.timestamp
                                    updatedLatestTimestamp <- if (latestTimestamp > timestamp) {
                                                               latestTimestamp.pure[F]
                                                             } else {
                                                               timestamp.pure[F]
                                                             }
                                  } yield updatedLatestTimestamp
                              }
      afterLatestParent = timestamp >= latestParentTimestamp
      result <- if (beforeFuture && afterLatestParent) {
                 Applicative[F].pure(Right(Valid))
               } else {
                 for {
                   _ <- Log[F].warn(
                         ignore(
                           b,
                           s"block timestamp $timestamp is not between latest parent block time and current time.")
                       )
                 } yield Left(InvalidUnslashableBlock)
               }
    } yield result

  def blockNumber[F[_]: Monad: Log: BlockStore](
      b: BlockMessage,
      dag: BlockDag): F[Either[InvalidBlock, ValidBlock]] =
    for {
      maybeMainParent       <- ProtoUtil.parentHashes(b).headOption.traverse(ProtoUtil.unsafeGetBlock[F])
      maybeMainParentNumber = maybeMainParent.map(ProtoUtil.blockNumber)
      number                = ProtoUtil.blockNumber(b)
      result                = maybeMainParentNumber.fold(number == 0)(_ + 1 == number)
      status <- if (result) {
                 Applicative[F].pure(Right(Valid))
               } else {
                 val log = maybeMainParentNumber.fold(
                   Log[F].warn(
                     ignore(b, s"block number $number is not zero, but block has no parents.")
                   )
                 )(n => {
                   Log[F].warn(
                     ignore(b, s"block number $number is not one more than parent number $n.")
                   )
                 })
                 for {
                   _ <- log
                 } yield Left(InvalidBlockNumber)
               }
    } yield status

  def sequenceNumber[F[_]: Monad: Log: BlockStore](
      b: BlockMessage,
      dag: BlockDag): F[Either[InvalidBlock, ValidBlock]] =
    for {
      creatorJustificationSeqNumber <- b.justifications
                                        .find {
                                          case Justification(validator, _) => validator == b.sender
                                        }
                                        .fold((-1).pure[F]) {
                                          case Justification(_, latestBlockHash) =>
                                            for {
                                              latestBlock <- ProtoUtil.unsafeGetBlock[F](
                                                              latestBlockHash)
                                              latestBlockSeqNum = latestBlock.seqNum
                                            } yield latestBlockSeqNum
                                        }
      number = b.seqNum
      result = creatorJustificationSeqNumber + 1 == number
      status <- if (result) {
                 Applicative[F].pure(Right(Valid))
               } else {
                 for {
                   _ <- Log[F].warn(ignore(
                         b,
                         s"seq number $number is not one more than creator justification number $creatorJustificationSeqNumber."))
                 } yield Left(InvalidSequenceNumber)
               }
    } yield status

  def parents[F[_]: Monad: Log: BlockStore](b: BlockMessage,
                                            genesis: BlockMessage,
                                            dag: BlockDag): F[Either[InvalidBlock, ValidBlock]] = {
    val bParents = b.header.fold(Seq.empty[ByteString])(_.parentsHashList)

    if (b.justifications.isEmpty) {
      if (bParents.exists(_ != genesis.blockHash))
        for {
          _ <- Log[F].warn(ignore(b, "justification is empty, but block has non-genesis parents."))
        } yield Left(InvalidParents)
      else
        Applicative[F].pure(Right(Valid))
    } else {
      val latestMessages = b.justifications
        .foldLeft(Map.empty[Validator, BlockHash]) {
          case (map, Justification(v, hash)) => map.updated(v, hash)
        }
      // TODO: Double check logic here
      val viewDag = dag.copy(latestMessages = latestMessages)
      for {
        estimate         <- Estimator.tips[F](viewDag, genesis)
        trueParents      <- ProtoUtil.chooseNonConflicting[F](estimate, genesis, dag)
        trueParentHashes = trueParents.map(_.blockHash)
        status <- if (bParents == trueParentHashes)
                   Applicative[F].pure(Right(Valid))
                 else
                   for {
                     _ <- Log[F].warn(
                           ignore(b,
                                  "block parents did not match estimate based on justification."))
                   } yield Left(InvalidParents)
      } yield status
    }
  }

  /*
   * When we switch between equivocation forks for a slashed validator, we will potentially get a
   * justification regression that is valid but since that validator is dropped from the
   * justifications, we don't have to check for it.
   */
  def justificationRegressions[F[_]: Monad: Log: BlockStore](
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag): F[Either[InvalidBlock, ValidBlock]] = {
    val latestMessagesOfBlock = ProtoUtil.toLatestMessages(b.justifications)
    val latestMessagesOfLatestMessagesForSender =
      dag.latestMessagesOfLatestMessages.getOrElse(b.sender, latestMessagesOfBlock)
    for {
      containsJustificationRegression <- latestMessagesOfBlock.toList.existsM {
                                          case (validator, currentBlockJustificationHash) =>
                                            val previousBlockJustificationHash =
                                              latestMessagesOfLatestMessagesForSender.getOrElse(
                                                validator,
                                                genesis.blockHash)
                                            for {
                                              currentBlockJustification <- ProtoUtil
                                                                            .unsafeGetBlock[F](
                                                                              currentBlockJustificationHash)
                                              previousBlockJustification <- ProtoUtil
                                                                             .unsafeGetBlock[F](
                                                                               previousBlockJustificationHash)
                                            } yield
                                              if (currentBlockJustification.seqNum < previousBlockJustification.seqNum) {
                                                true
                                              } else {
                                                false
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
  }

  def transactions[F[_]: Monad: Log: Capture: BlockStore](
      block: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag,
      emptyStateHash: StateHash,
      runtimeManager: RuntimeManager,
      knownStateHashesContainer: AtomicSyncVar[Set[StateHash]])(
      implicit scheduler: Scheduler): F[Either[InvalidBlock, ValidBlock]] =
    for {
      internalMap <- BlockStore[F].asMap()
      maybeCheckPoint <- Capture[F].capture {
                          val Right((maybeCheckPoint, _)) =
                            knownStateHashesContainer
                              .mapAndUpdate[(Option[StateHash], Set[StateHash])](
                                //invalid blocks return None and don't update the checkpoints
                                InterpreterUtil.validateBlockCheckpoint(
                                  block,
                                  genesis,
                                  dag,
                                  internalMap,
                                  emptyStateHash,
                                  _,
                                  runtimeManager
                                ),
                                _._2
                              )
                          maybeCheckPoint
                        }
    } yield
      maybeCheckPoint match {
        case Some(_) => Right(Valid)
        case None    => Left(InvalidTransaction)
      }

  /**
    * If block contains an invalid justification block B and the creator of B is still bonded,
    * return a RejectableBlock. Otherwise return an IncludeableBlock.
    */
  def neglectedInvalidBlockCheck[F[_]: Applicative](
      block: BlockMessage,
      invalidBlockTracker: Set[BlockHash]): F[Either[InvalidBlock, ValidBlock]] = {
    val invalidJustifications = block.justifications.filter(justification =>
      invalidBlockTracker.contains(justification.latestBlockHash))
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

  def bondsCache[F[_]: Applicative: Log](
      b: BlockMessage,
      runtimeManager: RuntimeManager): F[Either[InvalidBlock, ValidBlock]] = {
    val bonds = ProtoUtil.bonds(b)
    ProtoUtil.tuplespace(b) match {
      case Some(tuplespaceHash) =>
        Try(runtimeManager.computeBonds(tuplespaceHash)) match {
          case Success(computedBonds) =>
            if (bonds == computedBonds) {
              Applicative[F].pure(Right(Valid))
            } else {
              for {
                _ <- Log[F].warn(
                      "Bonds in proof of stake contract do not match block's bond cache.")
              } yield Left(InvalidBondsCache)
            }
          case Failure(_) =>
            for {
              _ <- Log[F].warn("Failed to compute bonds from tuplespace hash.")
            } yield Left(InvalidBondsCache)
        }
      case None =>
        for {
          _ <- Log[F].warn("Block is missing a tuplespace hash.")
        } yield Left(InvalidBondsCache)
    }
  }
}
