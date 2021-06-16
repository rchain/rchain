package coop.rchain.node.revvaultexport.reporting

import cats.Parallel
import cats.effect.{Blocker, Concurrent, ContextShift, Sync}
import cats.implicits._
import cats.mtl.FunctorRaise
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.{BlockDagKeyValueStorage, BlockDagRepresentation}
import coop.rchain.blockstorage.util.io.IOError
import coop.rchain.blockstorage.{BlockStore, KeyValueBlockStore}
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.genesis.contracts.StandardDeploys
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.casper.storage.RNodeKeyValueStoreManager.legacyRSpacePathPrefix
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.{BondsParser, VaultParser}
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.node.revvaultexport.mainnet1.reporting.SpecialCase.getSpecialTransfer
import coop.rchain.node.revvaultexport.RhoTrieTraverser
import coop.rchain.node.revvaultexport.mainnet1.reporting.PerValidatorVaults
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.rspace.syntax._
import coop.rchain.rspace.{Match, RSpace}
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import fs2._
import monix.execution.Scheduler

import java.nio.file.{Files, Path}
import scala.concurrent.ExecutionContext

object TransactionBalances {

  final case class Transfer(amount: Long, fromAddr: String, toAddr: String, blockNumber: Long)

  val initialPosStakingVault: RevAccount = RevAccount(
    RevAddress
      .fromPublicKey(
        Secp256k1.toPublic(PrivateKey(Base16.unsafeDecode(StandardDeploys.poSGeneratorPk)))
      )
      .get,
    0,
    PosStakingVault
  ) // 1111gW5kkGxHg7xDg6dRkZx2f7qxTizJzaCH9VEM1oJKWRvSX9Sk5
  val CoopVaultAddr = "11112q61nMYJKnJhQmqz7xKBNupyosG4Cy9rVupBPmpwcyT6s2SAoF"

  sealed trait AccountType
  object NormalVault          extends AccountType
  object PerValidatorVault    extends AccountType
  object PosStakingVault      extends AccountType
  object CoopPosMultiSigVault extends AccountType

  final case class RevAccount(address: RevAddress, amount: Long, accountType: AccountType) {
    def receiveRev(receiveAmount: Long): RevAccount = this.copy(amount = amount + receiveAmount)
    def sendRev(sendAmount: Long): RevAccount       = this.copy(amount = amount - sendAmount)

    def keccakHashedAddress: String =
      Base16.encode(RhoTrieTraverser.keccakParString(address.toBase58).drop(2))
    def typeString: String = accountType match {
      case NormalVault          => "NormalVault"
      case PerValidatorVault    => "PerValidatorVault"
      case PosStakingVault      => "PosStakingVault"
      case CoopPosMultiSigVault => "CoopPosMultiSigVault"
    }
  }

  type RevAddr = String

  /**
    * @param vaultMaps contains all the revVault account including posVaultAddress
    * `posVaultAddress`, `coopPosMultiSigVault`, `perValidatorVaults` are just a marker for special addresses.
    */
  final case class GlobalVaultsInfo(
      vaultMaps: Map[RevAddr, RevAccount],
      posVaultAddress: RevAddr,
      coopPosMultiSigVault: RevAddr,
      perValidatorVaults: Seq[RevAddr]
  )

  def getPerValidatorVaults[F[_]: Sync: Span: Log](
      runtime: RhoRuntime[F],
      block: BlockMessage
  ): F[Seq[RevAddr]] = {
    val contract = """new return, rl(`rho:registry:lookup`),
                    |  poSCh
                    |in {
                    |  rl!(`rho:rchain:pos`, *poSCh) |
                    |  for(@(_, PoS) <- poSCh) {
                    |    @PoS!("getActiveValidatorVaults", *return)
                    |  }
                    |}""".stripMargin
    for {
      perValidatorVaults <- runtime.playExploratoryDeploy(
                             contract,
                             block.body.state.postStateHash
                           )
      perValidatorVaultAddr = perValidatorVaults.head.exprs.head.getEListBody.ps
        .map(p => p.exprs.head.getETupleBody.ps(1).exprs.head.getGString)
    } yield perValidatorVaultAddr
  }

  def generateRevAccountFromWalletAndBond[F[_]: Sync: Log](
      walletPath: Path,
      bondsPath: Path
  ): F[Map[String, RevAccount]] = {
    implicit val raiseIOError: FunctorRaise[F, IOError] = IOError.raiseIOErrorThroughSync[F]
    for {
      vaults   <- VaultParser.parse(walletPath)
      bondsMap <- BondsParser.parse(bondsPath)
      accountMap = vaults
        .map(v => (v.revAddress.toBase58, RevAccount(v.revAddress, v.initialBalance, NormalVault)))
        .toMap
      revAccountMap = bondsMap.foldLeft(accountMap) {
        case (vaultMap, (_, bondAmount)) =>
          val posVault =
            vaultMap.getOrElse(initialPosStakingVault.address.toBase58, initialPosStakingVault)
          val newPosVault = posVault.receiveRev(bondAmount)
          vaultMap.updated(initialPosStakingVault.address.toBase58, newPosVault)
      }
    } yield revAccountMap
  }

  def updateGenesisFromTransfer(
      genesisVault: GlobalVaultsInfo,
      transfers: List[Transfer]
  ): GlobalVaultsInfo = {
    val resultMap = transfers.foldLeft(genesisVault.vaultMaps) {
      case (m, transfer) =>
        val fromVault = m.getOrElse(
          transfer.fromAddr,
          RevAccount(
            address = RevAddress.parse(transfer.fromAddr).right.get,
            amount = 0L,
            accountType = NormalVault
          )
        )
        val newVaultMap = m.updated(transfer.fromAddr, fromVault.sendRev(transfer.amount))
        val toVault = newVaultMap.getOrElse(
          transfer.toAddr,
          RevAccount(
            address = RevAddress.parse(transfer.toAddr).right.get,
            amount = 0L,
            accountType = NormalVault
          )
        )
        newVaultMap.updated(transfer.toAddr, toVault.receiveRev(transfer.amount))
    }
    genesisVault.copy(vaultMaps = resultMap)
  }

  def getGenesisVaultMap[F[_]: Sync: Span: Log](
      walletPath: Path,
      bondsPath: Path,
      runtime: RhoRuntime[F],
      block: BlockMessage
  ): F[GlobalVaultsInfo] =
    for {
      vaultMap  <- generateRevAccountFromWalletAndBond(walletPath, bondsPath)
      coopVault = RevAccount(RevAddress.parse(CoopVaultAddr).right.get, 0, CoopPosMultiSigVault)
      perValidatorVaults <- getPerValidatorVaults(runtime, block).map(
                             addrs =>
                               addrs.map(
                                 addr =>
                                   RevAccount(
                                     RevAddress.parse(addr).right.get,
                                     0,
                                     PerValidatorVault
                                   )
                               )
                           )
      genesisAccountMap = (coopVault +: perValidatorVaults).foldLeft(vaultMap) {
        case (accountMap, account) => accountMap.updated(account.address.toBase58, account)
      }
      globalVaults = GlobalVaultsInfo(
        genesisAccountMap,
        initialPosStakingVault.address.toBase58,
        coopVault.address.toBase58,
        perValidatorVaults.map(_.address.toBase58)
      )
    } yield globalVaults

  def putTransfer[F[_]: Sync: Log](
      blockNumber: Long,
      transactionStore: Transaction.TransactionStore[F],
      blockStore: BlockStore[F],
      dag: BlockDagRepresentation[F]
  ): F[Vector[Transfer]] =
    for {
      blocks <- dag.topoSort(blockNumber, Some(blockNumber))
      blocksTransfer <- blocks.flatten.foldLeftM(Vector.empty[Transfer]) {
                         case (t, blockHash) =>
                           for {
                             isFinalized <- dag.isFinalized(blockHash)
                             result <- if (isFinalized) for {
                                        blockOpt <- blockStore.get(blockHash)
                                        block    = blockOpt.get
                                        _ <- Log[F].info(
                                              s"Current ${blockOpt.isDefined} ${PrettyPrinter
                                                .buildString(blockHash)} ${block.body.state.blockNumber}"
                                            )
                                        deployCost = block.body.deploys
                                          .foldLeft(Vector.empty[Transfer]) {
                                            case (transfers, pd) =>
                                              val deployerRevAddr =
                                                RevAddress.fromPublicKey(pd.deploy.pk).get.toBase58
                                              val blockPerValidator =
                                                PerValidatorVaults.senderPerValidator(block)
                                              val addedTransfers = {
                                                // between 166708 and 184203 blocks , there were a bug in the mainnet
                                                // that after a new validator is slashed, this validator would be slashed
                                                // in every block which means that there were a
                                                // slashing deploy in every block and all the fund in the per validator
                                                // vault would go to pos vault immediately.
                                                if (blockNumber > 166708L && blockNumber <= 184203L)
                                                  Vector(
                                                    Transfer(
                                                      pd.cost.cost,
                                                      deployerRevAddr,
                                                      blockPerValidator,
                                                      block.body.state.blockNumber
                                                    ),
                                                    Transfer(
                                                      pd.cost.cost,
                                                      blockPerValidator,
                                                      initialPosStakingVault.address.toBase58,
                                                      block.body.state.blockNumber
                                                    )
                                                  )
                                                else
                                                  Vector(
                                                    Transfer(
                                                      pd.cost.cost,
                                                      deployerRevAddr,
                                                      blockPerValidator,
                                                      block.body.state.blockNumber
                                                    )
                                                  )
                                              }
                                              transfers ++ addedTransfers
                                          }
                                        transactions <- transactionStore.get(
                                                         Base16.encode(blockHash.toByteArray)
                                                       )
                                        res = transactions.get.flatten
                                          .foldLeft(Vector.empty[Transfer]) {
                                            case (th, transaction) =>
                                              if (transaction.success) {
                                                th :+ Transfer(
                                                  transaction.amount,
                                                  transaction.fromAddr,
                                                  transaction.toAddr,
                                                  block.body.state.blockNumber
                                                )

                                              } else th
                                          }
                                      } yield res ++ deployCost
                                      else Vector.empty[Transfer].pure[F]
                           } yield result ++ t
                       }
    } yield blocksTransfer

  def getAllTransfer[F[_]: Concurrent: Log](
      targetBlock: BlockMessage,
      transactionStore: Transaction.TransactionStore[F],
      blockStore: BlockStore[F],
      dag: BlockDagRepresentation[F]
  ): Stream[F, Vector[Transfer]] =
    Stream
      .range(1, targetBlock.body.state.blockNumber.toInt + 1)
      .parEvalMapUnorderedProcBounded(
        i =>
          putTransfer(
            i.toLong,
            transactionStore,
            blockStore,
            dag
          )
      )

  def getBlockHashByHeight[F[_]: Sync](
      blockNumber: Long,
      dag: BlockDagRepresentation[F],
      blockStore: BlockStore[F]
  ): F[BlockMessage] =
    for {
      blocks    <- dag.topoSort(blockNumber.toLong, Some(blockNumber.toLong))
      blockHash = blocks.flatten.head
      block     <- blockStore.get(blockHash)
      blockMes  = block.get
    } yield blockMes

  def main[F[_]: Concurrent: Parallel: ContextShift](
      dataDir: Path,
      walletPath: Path,
      bondPath: Path,
      transactionDir: Path,
      targetBlockHash: String
  )(implicit scheduler: ExecutionContext): F[(GlobalVaultsInfo, Map[String, List[Transfer]])] = {
    val ioScheduler = Scheduler.io()
    val blocker     = Blocker.liftExecutionContext(ioScheduler)

    val oldRSpacePath                           = dataDir.resolve(s"$legacyRSpacePathPrefix/history/data.mdb")
    val legacyRSpaceDirSupport                  = Files.exists(oldRSpacePath)
    implicit val metrics: Metrics.MetricsNOP[F] = new Metrics.MetricsNOP[F]()
    import coop.rchain.rholang.interpreter.storage._
    implicit val span: NoopSpan[F]                           = NoopSpan[F]()
    implicit val log: Log[F]                                 = Log.log
    implicit val m: Match[F, BindPattern, ListParWithRandom] = matchListPar[F]
    for {
      rnodeStoreManager <- RNodeKeyValueStoreManager[F](dataDir, legacyRSpaceDirSupport, blocker)
      blockStore        <- KeyValueBlockStore(rnodeStoreManager)
      blockDagStorage   <- BlockDagKeyValueStorage.create[F](rnodeStoreManager)
      dagRepresantation <- blockDagStorage.getRepresentation
      store             <- rnodeStoreManager.rSpaceStores
      spaces <- RSpace
                 .createWithReplay[F, Par, BindPattern, ListParWithRandom, TaggedContinuation](
                   store
                 )
      (rSpacePlay, rSpaceReplay, _) = spaces
      runtimes <- RhoRuntime
                   .createRuntimes[F](rSpacePlay, rSpaceReplay, initRegistry = true, Seq.empty)
      (rhoRuntime, _)  = runtimes
      targetBlockOpt   <- blockStore.get(ByteString.copyFrom(Base16.unsafeDecode(targetBlockHash)))
      targetBlock      = targetBlockOpt.get
      _                <- log.info(s"Getting balance from $targetBlock")
      genesisVaultMap  <- getGenesisVaultMap(walletPath, bondPath, rhoRuntime, targetBlock)
      transactionStore <- Transaction.store(transactionDir, blocker)
      tasks = getAllTransfer(
        targetBlock,
        transactionStore,
        blockStore,
        dagRepresantation
      )
      transfers   <- tasks.compile.toList
      allTransfer = transfers.flatten ++ getSpecialTransfer(targetBlock.body.state.blockNumber)
      _ <- log.info(
            s"After getting transfer history total ${allTransfer.length} account make transfer."
          )
      sortedAllTransfer = allTransfer.sortBy(t => t.blockNumber)
      toAddrTransfers   = sortedAllTransfer.groupBy(t => t.toAddr)
      fromAddrTransfers = sortedAllTransfer.groupBy(t => t.fromAddr)
      mappedTransfer = toAddrTransfers.toList.foldLeft(Map.empty[String, List[Transfer]]) {
        case (m, (addr, transfers)) =>
          val fromTransfers = fromAddrTransfers.getOrElse(addr, List.empty)
          m.updated(addr, transfers ++ fromTransfers)
      }
      afterTransferMap = updateGenesisFromTransfer(genesisVaultMap, sortedAllTransfer)
    } yield (afterTransferMap, mappedTransfer)
  }
}
