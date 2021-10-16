package coop.rchain.node.benchmark

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.node.benchmark.Payment.{conflictsPresent, verifyBalances, BalanceSheet}
import coop.rchain.blockstorage.dag.{BlockDagKeyValueStorage, BlockDagStorage}
import coop.rchain.casper.merging.{BlockIndex, DagMerger}
import coop.rchain.casper.protocol.{
  BlockMessage,
  Body,
  Bond,
  CommEvent,
  DeployData,
  Header,
  ProcessedDeploy,
  ProcessedSystemDeploy,
  RChainState,
  RejectedDeploy
}
import coop.rchain.casper.util.rholang.costacc.CloseBlockDeploy
import coop.rchain.casper.util.rholang.{RuntimeManager, SystemDeployUtil}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Signed
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.block.StateHash.StateHash
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.syntax._
import coop.rchain.shared.{Log, Stopwatch, Time}
import coop.rchain.store.InMemoryStoreManager
import fs2.Stream

import scala.collection.Seq
import scala.concurrent.duration.{FiniteDuration, NANOSECONDS}

object DagBuilder {

  def mkStateTransition[F[_]: Concurrent: Time](
      runtimeManager: RuntimeManager[F],
      baseState: StateHash,
      validator: PublicKey,
      seqNum: Int,
      blockNum: Long,
      payments: Seq[Payment]
  ): Stream[
    F,
    (StateHash, Seq[Charged[PaymentDeploy]], Seq[ProcessedDeploy], Seq[ProcessedSystemDeploy])
  ] = {
    val perValidatorVault =
      User(PrivateKey(ByteString.EMPTY), validator, Base16.encode(validator.bytes))

    def computeState(
        userDeploys: Seq[Signed[DeployData]]
    ): F[(StateHash, Seq[ProcessedDeploy], Seq[ProcessedSystemDeploy])] = {
      val cbRandomSeed = SystemDeployUtil.generateCloseDeployRandomSeed(validator, seqNum)
      assert(userDeploys.nonEmpty, "Attempting to compute state without user deploys.")
      runtimeManager
        .computeState(baseState)(
          terms = userDeploys.distinct,
          systemDeploys = CloseBlockDeploy(cbRandomSeed) :: Nil,
          blockData = BlockData(userDeploys.head.data.timestamp, blockNum, validator, seqNum),
          invalidBlocks = Map.empty[BlockHash, Validator]
        )
    }

    Stream(payments)
      .evalMap(_.toList.traverse(Payment.mkTxDeploy(_)))
      .evalMap { deploysWithMeta =>
        computeState(deploysWithMeta.map(_.d)).map {
          case (s, processedDeploys, sp) =>
            assert(
              !processedDeploys.exists(_.isFailed),
              "Failed deploys found. Check if you users have enough REV to continue payments."
            )
            val charged =
              processedDeploys.map { d =>
                val payerAddr = RevAddress.fromPublicKey(d.deploy.pk).get.address.toBase58
                val charge = Payment(
                  // TODO key not avail here, but not needed actually
                  User(PrivateKey(ByteString.EMPTY), d.deploy.pk, payerAddr),
                  perValidatorVault,
                  d.cost.cost
                )
                Charged(deploysWithMeta.find(_.d.sig == d.deploy.sig).get, charge)
              }
            (s, charged, processedDeploys, sp)
        }
      }
  }

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
        mkStateTransition(
          runtimeManager,
          preStateHash,
          validatorPk,
          seqNum,
          blockNum,
          payments
        ).map {
          case (postStateHash, chargedDeploysWithMeta, processedDeploys, processedSystem) =>
            (
              packBlock(validatorPk, postStateHash, processedDeploys, processedSystem),
              chargedDeploysWithMeta
            )
        }
    }
  }

  def mkLayer[F[_]: Concurrent: Time: Log](
      validatorsWithPayments: List[ValidatorWithPayments],
      baseBlock: BlockMessage,
      dagStore: BlockDagStorage[F]
  )(
      implicit runtimeManager: RuntimeManager[F]
  ): F[
    (
        BlockMessage,
        Seq[Charged[PaymentDeploy]],
        List[Seq[Charged[PaymentDeploy]]],
        List[RejectedDeploy],
        Long,
        Long,
        String
    )
  ] = {

    val baseState        = baseBlock.body.state.postStateHash
    val seqNum           = baseBlock.seqNum + 1
    val mergingBlocksNum = (baseBlock.seqNum * 2 + 1).toLong
    val mergerBlockNum   = (baseBlock.seqNum * 2 + 2).toLong
    val validatorsNum    = validatorsWithPayments.size

    val mkBlocksToMerge =
      Log[F].info(s"Creating ${validatorsNum - 1} concurrent blocks.") *>
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
      t                         <- mkBlocksToMerge
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
    } yield (
      nextBaseBlock,
      leaderPayments,
      mergedPayments,
      rejectedDeploys.map(RejectedDeploy(_)).toList,
      commsAccepted,
      commsRejected,
      mergeTime
    )
  }

  final case class ValidatorWithPayments(validator: PublicKey, payments: Seq[Payment])

  def leaderful[F[_]: Concurrent: Time: RuntimeManager: BlockDagStorage: Log: Metrics](
      genesis: BlockMessage,
      layers: Iterator[Seq[ValidatorWithPayments]],
      initBalances: BalanceSheet,
      mergesNum: Int = 1
  ): F[Unit] = {
    val usersToTrack = initBalances
      .filterNot {
        // do not check per validator vaults
        case (User(_, pk, _), _) =>
          layers.next().map(_.validator).contains(pk)
      }
    for {
      _ <- Log[F].info("Verifying vaults balances at genesis...")
      _ <- verifyBalances(
            initBalances.filterKeys(usersToTrack.contains).iterator,
            genesis.body.state.postStateHash
          )
      _ <- Log[F].info(s"Done. OK for ${usersToTrack.size} vaults at state ${Base16
            .encode(genesis.body.state.postStateHash.toByteArray)}.")
      _ <- (genesis, 0, initBalances, 0L, 0L, 0L).tailRecM[F, (BlockMessage, BalanceSheet)] {
            case (
                baseBlock,
                layerNum,
                balanceSheetAcc,
                acceptedCommsAcc,
                rejectedCommsAcc,
                durAcc
                ) =>
              val validatorsWithPayments = layers.next()
              val newLayer = for {
                v <- Stopwatch.durationNano(
                      mkLayer(validatorsWithPayments.toList, baseBlock, BlockDagStorage[F])
                    )
                (
                  (
                    b,
                    basePayment,
                    deploysToMerge,
                    rejectedOnMerge,
                    commsAccepted,
                    commsRejected,
                    mergeTime
                  ),
                  dur
                ) = v

                // check state
                rejectionMissed = rejectedOnMerge.nonEmpty !=
                  conflictsPresent(deploysToMerge.map(_.map(_.v.payment)))
                _ <- new Exception("Transfers are conflicting but no rejections on merge.").raiseError
                      .whenA(rejectionMissed)

                _ <- new Exception("State is not changed.").raiseError
                      .whenA(baseBlock.body.state.postStateHash == b.body.state.postStateHash)

                paymentsMerged = deploysToMerge.flatten.map { dwp =>
                  if (rejectedOnMerge.map(_.sig).contains(dwp.v.d.sig)) {
                    dwp.copy(
                      charge = dwp.charge.copy(rejected = true),
                      v = dwp.v.copy(payment = dwp.v.payment.copy(rejected = true))
                    )
                  } else dwp
                }
                newBalanceSheet = (basePayment ++ paymentsMerged)
                  .flatMap(dwp => List(dwp.charge, dwp.v.payment))
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
                nextLayerNum  = layerNum + 1
                toVerify      = newBalanceSheet.filterKeys(usersToTrack.contains)
                usersInvolved = toVerify.filter { case (_, (_, txs)) => txs.nonEmpty }
                _ <- Log[F].info(s"Verifying ${toVerify.size} vaults balances at state ${Base16
                      .encode(b.body.state.postStateHash.toByteArray)}")
                _ <- verifyBalances(
                      toVerify.iterator,
                      b.body.state.postStateHash
                    )

                // each tx has charge
                paymentsRejected = newBalanceSheet.flatMap(_._2._2).toSet.count(_.rejected)
                paymentsIn       = newBalanceSheet.flatMap(_._2._2).toSet.count(v => !v.rejected)
                _ <- new Exception("Rejected payments number is even (are you including charge?)").raiseError
                      .whenA(paymentsRejected % 2 != 0)
                _ <- new Exception("Accepted payments number is even (are you including charge?)").raiseError
                      .whenA(paymentsIn % 2 != 0)

                txRejected          = paymentsRejected / 2
                txIn                = paymentsIn / 2
                txTotal             = txIn + txRejected
                newAcceptedCommsAcc = acceptedCommsAcc + commsAccepted
                newRejectedCommsAcc = rejectedCommsAcc + commsRejected
                newDurAcc           = durAcc + dur

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
                       | Users involved in TX: ${usersInvolved.size}
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
                b,
                nextLayerNum,
                newBalanceSheet,
                newAcceptedCommsAcc,
                newRejectedCommsAcc,
                newDurAcc
              ).asLeft[(BlockMessage, BalanceSheet)]

              if (layerNum == mergesNum)
                (baseBlock, balanceSheetAcc)
                  .asRight[(BlockMessage, Int, BalanceSheet, Long, Long, Long)]
                  .pure[F]
              else newLayer
          }
    } yield ()
  }
}
//trait DagBuilder[D[_], V, M, S] {
//  // new row
//  def < : S => D[S]
//  // epoch change
//  def <! : S => D[S]
//  // push message to DAG, use next validator in line
//  def b(msg: M)(ps: M*): S => D[S]
//  // do not create deploy on validator
//  def * : S => D[S]
//  // push message to DAG, choose validator
//  def bv(msg: M)(ps: M*)(v: V): S => D[S]
//  // push multiple messages having the same parents
//  def bs(msgs: (M, V)*)(ps: M*): S => D[S]
//  // empty DAG
//  def empty(genesis: M): D[S]
//  // repeat from the start
//  def repeat(times: Int): S => D[S]
//}
//
//final class DagFlatMapOps[F[_], A](private val fa: F[A]) extends AnyVal {
//  def |[B](f: A => F[B])(implicit F: FlatMap[F]): F[B] = F.flatMap(fa)(f)
//}
//
//case class DagsLibrary[Dag[_]: Concurrent, S: DagBuilder[Dag[S], Int, Int, S]: DagFlatMapOps](
//    builder: DagBuilder[Dag[S], Int, Int, S]
//) {
//  implicit final def catsSyntaxFlatMapOps[F[_]: FlatMap, A](fa: F[A]): DagFlatMapOps[F, A] =
//    new DagFlatMapOps[F, A](fa)
//
//  def leaderful(validatorsNum: Int): Dag[S] = {
//    import builder._
//    val tail = (2 until validatorsNum).map(v => (v,v))
//
//    empty(0)              |
//      < |     b(1)(0)     |
//      < |  bs(tail:_*)(1) |
//    repeat(100)
//  }
//
//  def random(g: Int, validatorsNum: Int): Dag[S] = {
//    import builder._
//
//    empty(0) |
//      < |  b(1)(0)  | b(2)(0) | b(3)(0) |
//      < |     *     | b(4)(2) | b(5)(1) |
//      < | b(6)(4,5) |    *    |    *    |
//      < | bs((7,1),(8,2),(9,3))(6,5,4)  |
//      < |       bv(10)(7,8,9)(3)        |
//      repeat(100)
//  }
//}
//
//trait Market[F[_], U, S] {
//  def tx(from: U, to: U): S => F[S]
//  def balance(t: U): F[Long]
//  def allBalances: F[Map[U, Long]]
//}
//
//trait StateChanger[F[_], S, V, D] {
//  def mkStateTransitions(initState: S, transitions: Seq[(V, Seq[D], D)]): F[Seq[(V, S)]]
//}
//
//trait DagBuilder[F[_], S, V, T] {
//  def empty(genesis: (T, S))
//  def createNodes(nodes: Seq[(T, V, Long)])
//}
