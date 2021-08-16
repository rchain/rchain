package coop.rchain.node.benchmark.utils

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper.merging.DeployIndex.sysCloseBlockId
import coop.rchain.casper.merging.{BlockIndex, DagMerger, DeployIndex}
import coop.rchain.casper.protocol.{
  BlockMessage,
  Body,
  Bond,
  CommEvent,
  Header,
  ProcessedDeploy,
  ProcessedSystemDeploy,
  RChainState,
  RejectedDeploy
}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.Metrics
import coop.rchain.models.block.StateHash.StateHash
import coop.rchain.node.benchmark.utils.Payment.{
  conflictsPresent,
  verifyBalances,
  BalanceSheet,
  BlockWithPayments
}
import coop.rchain.node.benchmark.utils.StateTransition.StateTransitionResult
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.syntax._
import coop.rchain.shared.{Log, Stopwatch, Time}
import fs2.Stream

import scala.collection.Seq
import scala.concurrent.duration.{FiniteDuration, NANOSECONDS}

object LeaderfulSimulation {

  def mkBlocks[F[_]: Concurrent: Time](
      validatorsWithPayments: List[ValidatorWithPayments],
      preStateHash: StateHash,
      seqNum: Int,
      blockNum: Long,
      deploysToReject: Seq[ByteString] = List.empty
  )(
      implicit runtimeManager: RuntimeManager[F]
  ): List[Stream[F, (BlockMessage, Seq[Charged[PaymentDeploy]])]] = {
    def packBlock(
        sender: PublicKey,
        postStateHash: StateHash,
        processed: Seq[ProcessedDeploy],
        processedSystem: Seq[ProcessedSystemDeploy]
    ): BlockMessage = BlockMessage(
      blockHash = ByteString.copyFrom(Array.fill(32)((scala.util.Random.nextInt(256) - 128).toByte)),
      header = Header(
        parentsHashList = List.empty,
        timestamp = processed.head.deploy.data.timestamp,
        version = 1
      ),
      body = Body(
        state = RChainState(
          preStateHash = preStateHash,
          postStateHash = postStateHash,
          bonds = validatorsWithPayments
            .map(_.validator)
            .map(pk => Bond(ByteString.copyFrom(pk.bytes), 1)),
          blockNumber = blockNum
        ),
        deploys = processed.toList,
        systemDeploys = processedSystem.toList,
        rejectedDeploys = deploysToReject.map(RejectedDeploy(_)).toList
      ),
      justifications = List.empty,
      sender = ByteString.copyFrom(sender.bytes),
      seqNum = seqNum,
      sig = ByteString.copyFrom(Array.fill(32)((scala.util.Random.nextInt(256) - 128).toByte)),
      sigAlgorithm = "",
      shardId = "shardId"
    )

    validatorsWithPayments.map {
      case ValidatorWithPayments(validatorPk, payments) =>
        Stream
          .eval(
            StateTransition
              .make(
                preStateHash,
                validatorPk,
                seqNum,
                blockNum,
                payments.toList
              )
          )
          .map {
            case StateTransitionResult(
                postStateHash,
                chargedDeploysWithMeta,
                processedDeploys,
                processedSystem
                ) =>
              (
                packBlock(validatorPk, postStateHash, processedDeploys, processedSystem),
                chargedDeploysWithMeta
              )
          }
    }
  }

  final case class LayerResult(
      mergeBlock: BlockWithPayments,
      mergedBlocks: List[BlockWithPayments],
      rejected: List[RejectedDeploy],
      commsAccepted: Long,
      commsrejected: Long,
      mergeTime: String
  )
  def mkLayer[F[_]: Concurrent: Time: Log](
      validatorsWithPayments: List[ValidatorWithPayments],
      baseBlock: BlockMessage,
      dagStore: BlockDagStorage[F]
  )(
      implicit runtimeManager: RuntimeManager[F]
  ): F[LayerResult] = {

    val baseState        = baseBlock.body.state.postStateHash
    val seqNum           = baseBlock.seqNum + 1
    val mergingBlocksNum = (baseBlock.body.state.blockNumber + 1).toLong
    val mergerBlockNum   = (baseBlock.body.state.blockNumber + 2).toLong
    val validatorsNum    = validatorsWithPayments.size

    val mkBlocksToMerge =
      Log[F].info(s"${validatorsNum - 1} validators create blocks concurrently.") *>
        Stream
          .emits(
            mkBlocks[F](validatorsWithPayments, baseState, seqNum, mergingBlocksNum).dropRight(1)
          )
          .parJoinProcBounded
          .map {
            case (b, payment) =>
              (
                b.copy(header = b.header.copy(parentsHashList = List(baseBlock.blockHash))),
                payment
              )
          }
          .evalTap { case (b, _) => dagStore.insert(b, false) }
          .compile
          .toList

    for {
      // create children blocks
      v                            <- Stopwatch.duration(mkBlocksToMerge)
      (t, tailStateTransitionTime) = v

      _                         <- Log[F].info(s"Done in ${tailStateTransitionTime}")
      (toMerge, mergedPayments) = t.unzip

      _ <- Log[F].info("Indexing blocks...")
      // merge children blocks
      indices <- (baseBlock +: toMerge)
                  .traverse(
                    b =>
                      BlockIndex(
                        b.blockHash,
                        b.body.deploys,
                        b.body.systemDeploys,
                        Blake2b256Hash.fromByteString(b.body.state.preStateHash),
                        Blake2b256Hash.fromByteString(b.body.state.postStateHash),
                        runtimeManager.getHistoryRepo
                      ).map(b.blockHash -> _)
                  )
                  .map(_.toMap)
      dag <- dagStore.getRepresentation

      _ <- Log[F].info("Preparing merged state...")
      v <- Stopwatch.duration(
            DagMerger.merge[F](
              dag,
              baseBlock.blockHash,
              Blake2b256Hash.fromByteString(baseState),
              indices(_).deployChains.pure,
              runtimeManager.getHistoryRepo,
              DagMerger.costOptimalRejectionAlg
            )
          )
      ((postState, rejectedDeploys), mergeTime) = v
      mergedState                               = ByteString.copyFrom(postState.bytes.toArray)

      // create next base block (merge block)
      _ <- Log[F].info("Creating merge block...")
      r <- mkBlocks[F](
            validatorsWithPayments,
            mergedState,
            seqNum,
            mergerBlockNum,
            rejectedDeploys
          ).last
            .map {
              case (b, balancesDiff) =>
                (
                  b.copy(header = b.header.copy(parentsHashList = toMerge.map(_.blockHash))),
                  balancesDiff
                )
            }
            .compile
            .lastOrError
      (nextBaseBlock, leaderPayments) = r
      _                               <- dagStore.insert(nextBaseBlock, false)
      _                               <- dagStore.recordDirectlyFinalized(nextBaseBlock.blockHash, _ => ().pure[F])
      (rejectedLogs, acceptedLogs) = (nextBaseBlock +: toMerge)
        .flatMap(_.body.deploys)
        .map(d => (d, rejectedDeploys.contains(d.deploy.sig)))
        .partition { case (_, rejected) => rejected }
      commsAccepted = acceptedLogs
        .flatMap(_._1.deployLog)
        .collect { case c: CommEvent => c }
        .size
        .toLong
      commsRejected = rejectedLogs
        .flatMap(_._1.deployLog)
        .collect { case c: CommEvent => c }
        .size
        .toLong
    } yield LayerResult(
      BlockWithPayments(nextBaseBlock, leaderPayments),
      toMerge.zip(mergedPayments).map(BlockWithPayments.tupled),
      rejectedDeploys.map(RejectedDeploy(_)).toList,
      commsAccepted,
      commsRejected,
      mergeTime
    )
  }

  final case class ValidatorWithPayments(validator: PublicKey, payments: Seq[Payment])
  def go[F[_]: Concurrent: Time: RuntimeManager: BlockDagStorage: Log: Metrics](
      genesis: BlockMessage,
      layers: Iterator[Seq[ValidatorWithPayments]],
      initBalances: BalanceSheet,
      epochLength: Int,
      mergesNum: Int = 1
  ): Stream[F, (List[BlockMessage], BlockMessage)] = {
    val usersToTrack = initBalances
      .filterNot {
        // do not check per validator vaults
        case (User(_, pk, _), _) =>
          layers.next().map(_.validator).contains(pk)
      }

    def newLayer(
        validatorsWithPayments: Seq[ValidatorWithPayments],
        baseBlock: BlockMessage,
        balanceSheetAcc: BalanceSheet,
        acceptedCommsAcc: Long,
        rejectedCommsAcc: Long,
        durAcc: Long,
        layerNum: Int
    ): F[(BlockMessage, List[BlockMessage], Int, BalanceSheet, Long, Long, Long)] =
      for {
        v <- Stopwatch.durationNano(
              mkLayer(validatorsWithPayments.toList, baseBlock, BlockDagStorage[F])
            )
        (
          LayerResult(
            mergeBlockWithPayments,
            mergedBlocksWithPayments,
            rejectedOnMerge,
            commsAccepted,
            commsRejected,
            mergeTime
          ),
          dur
        ) = v

        // 1. initial check
        _ <- new Exception("State is not changed.").raiseError.whenA(
              baseBlock.body.state.postStateHash == mergeBlockWithPayments.b.body.state.postStateHash
            )
        isEpochMerge           = mergedBlocksWithPayments.head.b.body.state.blockNumber % epochLength == 0
        paymentsOfferedToMerge = mergedBlocksWithPayments.map(_.payments)
        conflictingPaymentsPresent = conflictsPresent(
          paymentsOfferedToMerge.map(_.map(_.v.payment))
        )
        closeBlocksOfferedToMerge = mergedBlocksWithPayments
          .map(_.b)
          .map(DeployIndex.sysCloseBlockId)
        rejections = rejectedOnMerge.map(_.sig)
        // a. when merging epoch all but one closeBlocks should be rejected
        badEpochMerge = isEpochMerge && (closeBlocksOfferedToMerge diff rejections).size != 1
        _ <- new Exception(
              "More then one close block left unrejected when merging blocks with epoch change"
            ).raiseError.whenA(badEpochMerge)
        // b. if conflicting payments present - there have to be rejected deploys
        // this check for epoch merge is problematic because close block on epoch depends on all payments
        // so all deploys are rejected
        wrongPaymentRejection = !isEpochMerge && rejectedOnMerge.nonEmpty != conflictingPaymentsPresent
        _ <- new Exception(
              "Transfers are conflicting but no rejections or vice versa."
            ).raiseError.whenA(wrongPaymentRejection)

        // 2. balances check
        paymentsMerged = paymentsOfferedToMerge.flatten.map { dwp =>
          if (rejectedOnMerge.map(_.sig).contains(dwp.v.d.sig)) {
            dwp.copy(
              charge = dwp.charge.copy(rejected = true),
              v = dwp.v.copy(payment = dwp.v.payment.copy(rejected = true))
            )
          } else dwp
        }
        paymentsInMerge = mergeBlockWithPayments.payments
        newBalanceSheet = (paymentsInMerge ++ paymentsMerged)
          .flatMap { dwp =>
            val v = List(dwp.charge, dwp.v.payment)
            assert(
              dwp.charge.rejected == dwp.v.payment.rejected,
              s"charge rejected ${dwp.charge.rejected}, payment rejected: ${dwp.v.payment.rejected}"
            )
            v
          }
          .foldLeft(balanceSheetAcc) {
            case (acc, p) =>
              acc +
                (p.source -> {
                  val (currB, currPs) =
                    acc.getOrElse(p.source, (0L, List.empty[Payment]))
                  (
                    if (p.rejected || p.source == p.dest) currB
                    else currB - p.amt,
                    p +: currPs
                  )
                }) +
                (p.dest -> {
                  val (currB, currPs) =
                    acc.getOrElse(p.dest, (0L, List.empty[Payment]))
                  (
                    if (p.rejected || p.source == p.dest) currB
                    else currB + p.amt,
                    p +: currPs
                  )
                })
          }
        // a. verify new balance sheet - each tx has charge so number of TX should be even
        paymentsRejected = newBalanceSheet.flatMap(_._2._2).toSet.count(_.rejected)
        paymentsIn       = newBalanceSheet.flatMap(_._2._2).toSet.count(v => !v.rejected)
        _ <- new Exception(
              s"Rejected payments number is odd ($paymentsRejected) (are you including charge?)"
            ).raiseError.whenA(paymentsRejected % 2 != 0)
        _ <- new Exception(
              s"Accepted payments number is odd ($paymentsIn) (are you including charge?)"
            ).raiseError.whenA(paymentsIn % 2 != 0)
        // b. verify state balances
        toVerify    = newBalanceSheet.filterKeys(usersToTrack.contains)
        usersMadeTx = toVerify.filter { case (_, (_, txs)) => txs.nonEmpty }
        _ <- Log[F].info(s"Verifying ${toVerify.size} vaults balances at state ${Base16
              .encode(mergeBlockWithPayments.b.body.state.postStateHash.toByteArray)}")
        _ <- verifyBalances(
              toVerify.iterator,
              mergeBlockWithPayments.b.body.state.postStateHash
            )

        // Log
        txRejected          = paymentsRejected / 2
        txIn                = paymentsIn / 2
        txTotal             = txIn + txRejected
        newAcceptedCommsAcc = acceptedCommsAcc + commsAccepted
        newRejectedCommsAcc = rejectedCommsAcc + commsRejected
        newDurAcc           = durAcc + dur
        nextLayerNum        = layerNum + 1

        _ <- Log[F].info(
              s"""
           |Layer ${nextLayerNum} accomplished. Layer stats:
           | Time spent: ${Stopwatch.showTime(FiniteDuration(dur, NANOSECONDS))}
           | Time spent on merge: $mergeTime
           | COMM events accepted: ${commsAccepted}
           | COMM events rejected: ${commsRejected}
           |Network stats:
           | Validators num: ${validatorsWithPayments.size}
           | Users total: ${newBalanceSheet.size - validatorsWithPayments.size}
           | Users involved in TX: ${usersMadeTx.size}
           | Payments accepted: ${txIn} (${txIn.toFloat / txTotal * 100} %)
           | Payments rejected: ${txRejected} (${txRejected.toFloat / txTotal * 100} %)
           | Avg payments per block: ${txTotal.toFloat / nextLayerNum / validatorsWithPayments.size}
           | COMMs accepted total: $newAcceptedCommsAcc
           | COMMs rejected total: $newRejectedCommsAcc
           | Time spent: ${Stopwatch
                   .showTime(FiniteDuration(newDurAcc, NANOSECONDS))} $newDurAcc
           | COMM EVENTS PER SEC (include rejected): ${(newAcceptedCommsAcc + newRejectedCommsAcc)
                   .floatValue() / (newDurAcc.floatValue() / 1e9)}
           | COMM EVENTS PER SEC (real):  ${newAcceptedCommsAcc
                   .floatValue() / (newDurAcc.floatValue() / 1e9)}
           |""".stripMargin
            )
      } yield (
        mergeBlockWithPayments.b,
        mergedBlocksWithPayments.map(_.b),
        nextLayerNum,
        newBalanceSheet,
        newAcceptedCommsAcc,
        newRejectedCommsAcc,
        newDurAcc
      )

    Stream
      .unfoldLoopEval((genesis, 0, initBalances, 0L, 0L, 0L)) {
        case (
            baseBlock,
            layerNum,
            balanceSheetAcc,
            acceptedCommsAcc,
            rejectedCommsAcc,
            durAcc
            ) =>
          val validatorsWithPayments = layers.next()

          for {
            _ <- Log[F].info("Verifying vaults balances at genesis...")
            _ <- verifyBalances(
                  initBalances.filterKeys(usersToTrack.contains).iterator,
                  genesis.body.state.postStateHash
                ).whenA(layerNum == 0)
            _ <- Log[F].info(s"Done. OK for ${usersToTrack.size} vaults at state ${Base16
                  .encode(genesis.body.state.postStateHash.toByteArray)}.")
            r <- newLayer(
                  validatorsWithPayments,
                  baseBlock,
                  balanceSheetAcc,
                  acceptedCommsAcc,
                  rejectedCommsAcc,
                  durAcc,
                  layerNum
                )
            (
              b,
              merged,
              nextLayerNum,
              newBalanceSheet,
              newAcceptedCommsAcc,
              newRejectedCommsAcc,
              newDurAcc
            ) = r

            out = (merged, b)
            next = (
              b,
              nextLayerNum,
              newBalanceSheet,
              newAcceptedCommsAcc,
              newRejectedCommsAcc,
              newDurAcc
            )
          } yield (out, (nextLayerNum < mergesNum).guard[Option].as(next))
      }
  }
}
