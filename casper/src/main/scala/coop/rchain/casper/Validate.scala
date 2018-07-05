package coop.rchain.casper

import cats.{Applicative, Monad}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol.{ApprovedBlock, BlockMessage, Justification}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.{InterpreterUtil, RuntimeManager}
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.catscontrib.Capture
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.shared.{AtomicSyncVar, Log, LogSource, Time}
import monix.execution.Scheduler

import scala.util.{Success, Try}

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

  def ignore(b: BlockMessage, reason: String): String =
    s"CASPER: Ignoring block ${PrettyPrinter.buildString(b.blockHash)} because $reason"

  def approvedBlock[F[_]: Applicative: Log](a: ApprovedBlock,
                                            requiredValidators: Set[ByteString]): F[Boolean] = {
    val maybeSigData = a.block.map(b => ProtoUtil.add(b.blockHash.toByteArray, a.requiredSigs))

    maybeSigData match {
      case Some(sigData) =>
        val validatedSigs =
          (for {
            s      <- a.sigs
            verify <- signatureVerifiers.get(s.algorithm)
            pk     = s.publicKey
            if verify(sigData, s.sig.toByteArray, pk.toByteArray)
          } yield pk).toSet

        if (validatedSigs.size >= a.requiredSigs && requiredValidators.forall(
              validatedSigs.contains))
          true.pure[F]
        else
          Log[F]
            .warn(
              "CASPER: Received invalid ApprovedBlock message not containing enough valid signatures.")
            .map(_ => false)

      case None =>
        Log[F]
          .warn("CASPER: Received invalid ApprovedBlock message not containing any block.")
          .map(_ => false)
    }
  }

  def blockSignature[F[_]: Applicative: Log](b: BlockMessage): F[Boolean] =
    signatureVerifiers
      .get(b.sigAlgorithm)
      .map(verify => {
        val justificationHash = ProtoUtil.protoSeqHash(b.justifications)
        val sigData =
          Blake2b256.hash(justificationHash.toByteArray ++ b.blockHash.toByteArray)

        Try(verify(sigData, b.sig.toByteArray, b.sender.toByteArray)) match {
          case Success(true) => true.pure[F]
          case _             => Log[F].warn(ignore(b, "signature is invalid.")).map(_ => false)
        }
      }) getOrElse {
      for {
        _ <- Log[F].warn(ignore(b, s"signature algorithm ${b.sigAlgorithm} is unsupported."))
      } yield false
    }

  def blockSender[F[_]: Applicative: Log](b: BlockMessage,
                                          genesis: BlockMessage,
                                          dag: BlockDag): F[Boolean] =
    if (b == genesis) {
      true.pure[F] //genesis block has a valid sender
    } else {
      val weight = ProtoUtil.weightFromSender(b, dag.blockLookup)
      if (weight > 0)
        true.pure[F]
      else
        for {
          _ <- Log[F].warn(
                ignore(b, s"block creator ${PrettyPrinter.buildString(b.sender)} has 0 weight."))
        } yield false
    }

  /*
   * TODO: Double check ordering of validity checks
   * TODO: Add check for missing fields
   * TODO: Check that justifications follow from bonds (especially beware of arbitrary droppings of bonded validators)
   * Justification regressions validation depends on sequence numbers being valid
   */
  def blockSummary[F[_]: Monad: Log: Time](
      block: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag): F[Either[RejectableBlock, IncludeableBlock]] =
    for {
      missingBlockStatus <- Validate.missingBlocks[F](block, dag)
      timestampStatus    <- missingBlockStatus.traverse(_ => Validate.timestamp[F](block, dag))
      blockNumberStatus  <- timestampStatus.traverse(_ => Validate.blockNumber[F](block, dag))
      parentsStatus <- blockNumberStatus.joinRight.traverse(_ =>
                        Validate.parents[F](block, genesis, dag))
      sequenceNumberStatus <- parentsStatus.joinRight.traverse(_ =>
                               Validate.sequenceNumber[F](block, dag))
    } yield sequenceNumberStatus.joinRight

  def missingBlocks[F[_]: Applicative: Log](
      block: BlockMessage,
      dag: BlockDag): F[Either[RejectableBlock, IncludeableBlock]] = {
    val parentsPresent = ProtoUtil.parents(block).forall(p => dag.blockLookup.contains(p))
    val justificationsPresent =
      block.justifications.forall(j => dag.blockLookup.contains(j.latestBlockHash))
    if (parentsPresent && justificationsPresent) {
      Applicative[F].pure(Right(Valid))
    } else {
      for {
        _ <- Log[F].debug(
              s"Fetching missing dependencies for ${PrettyPrinter.buildString(block.blockHash)}.")
      } yield Left(MissingBlocks)
    }
  }

  // This is not a slashable offence
  def timestamp[F[_]: Monad: Log: Time](
      b: BlockMessage,
      dag: BlockDag): F[Either[RejectableBlock, IncludeableBlock]] =
    for {
      currentTime  <- Time[F].currentMillis
      timestamp    = b.header.get.timestamp
      beforeFuture = currentTime + DRIFT >= timestamp
      latestParentTimestamp = ProtoUtil
        .parents(b)
        .foldLeft(0L) {
          case (latestTimestamp, parentHash) =>
            val parent    = dag.blockLookup(parentHash)
            val timestamp = parent.header.get.timestamp
            if (latestTimestamp > timestamp) {
              latestTimestamp
            } else {
              timestamp
            }
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

  def blockNumber[F[_]: Applicative: Log](
      b: BlockMessage,
      dag: BlockDag): F[Either[RejectableBlock, IncludeableBlock]] = {
    val parentNumber = ProtoUtil
      .parents(b)
      .headOption
      .map(dag.blockLookup andThen ProtoUtil.blockNumber)
    val number = ProtoUtil.blockNumber(b)
    val result = parentNumber.fold(number == 0)(_ + 1 == number)

    if (result) {
      Applicative[F].pure(Right(Valid))
    } else {
      val log = parentNumber.fold(
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
  }

  def sequenceNumber[F[_]: Applicative: Log](
      b: BlockMessage,
      dag: BlockDag): F[Either[RejectableBlock, IncludeableBlock]] = {
    val creatorJustificationSeqNumber = b.justifications
      .find {
        case Justification(validator, _) => validator == b.sender
      }
      .fold(-1) {
        case Justification(_, latestBlockHash) => dag.blockLookup(latestBlockHash).seqNum
      }
    val number = b.seqNum
    val result = creatorJustificationSeqNumber + 1 == number

    if (result) {
      Applicative[F].pure(Right(Valid))
    } else {
      for {
        _ <- Log[F].warn(ignore(
              b,
              s"seq number $number is not one more than creator justification number $creatorJustificationSeqNumber."))
      } yield Left(InvalidSequenceNumber)
    }
  }

  def parents[F[_]: Applicative: Log](
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag): F[Either[RejectableBlock, IncludeableBlock]] = {
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
      val viewDag     = dag.copy(latestMessages = latestMessages)
      val estimate    = Estimator.tips(viewDag, genesis)
      val trueParents = ProtoUtil.chooseNonConflicting(estimate, genesis, dag).map(_.blockHash)

      if (bParents == trueParents)
        Applicative[F].pure(Right(Valid))
      else
        for {
          _ <- Log[F].warn(
                ignore(b, "block parents did not match estimate based on justification."))
        } yield Left(InvalidParents)
    }
  }

  /*
   * When we switch between equivocation forks for a slashed validator, we will potentially get a
   * justification regression that is valid but since that validator is dropped from the
   * justifications, we don't have to check for it.
   */
  def justificationRegressions[F[_]: Applicative: Log](
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag): F[Either[RejectableBlock, IncludeableBlock]] = {
    val latestMessagesOfBlock = ProtoUtil.toLatestMessages(b.justifications)
    val latestMessagesOfLatestMessagesForSender =
      dag.latestMessagesOfLatestMessages.getOrElse(b.sender, latestMessagesOfBlock)
    val containsJustificationRegression =
      latestMessagesOfBlock.exists {
        case (validator, currentBlockJustificationHash) =>
          val previousBlockJustificationHash =
            latestMessagesOfLatestMessagesForSender.getOrElse(validator, genesis.blockHash)
          val currentBlockJustification  = dag.blockLookup(currentBlockJustificationHash)
          val previousBlockJustification = dag.blockLookup(previousBlockJustificationHash)
          if (currentBlockJustification.seqNum < previousBlockJustification.seqNum) {
            true
          } else {
            false
          }
      }
    if (containsJustificationRegression) {
      for {
        _ <- Log[F].warn(ignore(b, "block contains justification regressions."))
      } yield Left(JustificationRegression)
    } else {
      Applicative[F].pure(Right(Valid))
    }
  }

  def transactions[F[_]: Applicative: Log: Capture](
      block: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag,
      initStateHash: StateHash,
      runtimeManager: RuntimeManager,
      knownStateHashesContainer: AtomicSyncVar[Set[StateHash]])(
      implicit scheduler: Scheduler): F[Either[RejectableBlock, IncludeableBlock]] =
    for {
      maybeCheckPoint <- Capture[F].capture {
                          val Right((maybeCheckPoint, _)) =
                            knownStateHashesContainer
                              .mapAndUpdate[(Option[StateHash], Set[StateHash])](
                                //invalid blocks return None and don't update the checkpoints
                                InterpreterUtil.validateBlockCheckpoint(
                                  block,
                                  genesis,
                                  dag,
                                  initStateHash,
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
}
