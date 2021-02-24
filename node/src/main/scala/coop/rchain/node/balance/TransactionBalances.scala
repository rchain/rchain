package coop.rchain.node.balance

import cats.Parallel
import cats.implicits._
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.mtl.FunctorRaise
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockStore, KeyValueBlockStore}
import coop.rchain.blockstorage.dag.{
  BlockDagKeyValueStorage,
  BlockDagRepresentation,
  BlockDagStorage
}
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.casper.storage.RNodeKeyValueStoreManager.legacyRSpacePathPrefix
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rspace.{Match, RSpace}
import coop.rchain.rspace.syntax._
import coop.rchain.casper.syntax._
import coop.rchain.blockstorage.syntax._
import coop.rchain.shared.syntax._
import coop.rchain.blockstorage.util.io.IOError
import coop.rchain.casper.genesis.contracts.StandardDeploys
import coop.rchain.casper.protocol.{BlockMessage, ProcessedDeploy}
import coop.rchain.casper.util.rholang.RhoTrieTraverser
import coop.rchain.casper.util.{BondsParser, VaultParser}
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.node.balance.SpecialCases.{getSpecialCases, SpecialCase}
import coop.rchain.shared.Log
import coop.rchain.shared.Log.NOPLog

import java.nio.file.{Files, Path}
import scala.concurrent.ExecutionContext

object TransactionBalances {

  val initialPosStakingVault: RevAccount = RevAccount(
    RevAddress
      .fromPublicKey(
        Secp256k1.toPublic(PrivateKey(Base16.unsafeDecode(StandardDeploys.poSGeneratorPk)))
      )
      .get,
    0,
    PosStakingVault
  )

  val convinientPerValidatorAddr = ""

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

  final case class GlobalVaultsInfo(
      vaultMaps: Map[String, RevAccount],
      posVaultAddress: String,
      coopPosMultiSigVault: String,
      perValidatorVaults: Seq[String]
  )

  def getPerValidatorVault[F[_]: Sync: Span: Log](
      runtime: RhoRuntime[F],
      block: BlockMessage
  ): F[Seq[String]] = {
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

  def getCoopVault[F[_]: Sync: Span: Log](
      runtime: RhoRuntime[F],
      block: BlockMessage
  ): F[String] = {
    val contract = """new return, rl(`rho:registry:lookup`),
                     |  poSCh
                     |in {
                     |  rl!(`rho:rchain:pos`, *poSCh) |
                     |  for(@(_, PoS) <- poSCh) {
                     |    @PoS!("getCoopVault", *return)
                     |  }
                     |}""".stripMargin
    for {
      coopVault <- runtime.playExploratoryDeploy(
                    contract,
                    block.body.state.postStateHash
                  )
      coopVaultAddr = coopVault.head.exprs.head.getETupleBody.ps(1).exprs.head.getGString
    } yield coopVaultAddr
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
        case (vaultMap, (_, bondAmount)) => {
          val posVault =
            vaultMap.getOrElse(initialPosStakingVault.address.toBase58, initialPosStakingVault)
          val newPosVault = posVault.receiveRev(bondAmount)
          vaultMap.updated(initialPosStakingVault.address.toBase58, newPosVault)
        }
      }
    } yield revAccountMap
  }

  def getGenesisVaultMap[F[_]: Sync: Span: Log](
      walletPath: Path,
      bondsPath: Path,
      runtime: RhoRuntime[F],
      block: BlockMessage
  ): F[GlobalVaultsInfo] =
    for {
      vaultMap <- generateRevAccountFromWalletAndBond(walletPath, bondsPath)
      coopVault <- getCoopVault(runtime, block).map(
                    addr => RevAccount(RevAddress.parse(addr).right.get, 0, CoopPosMultiSigVault)
                  )
      perValidatorVaults <- getPerValidatorVault(runtime, block).map(
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
        case (accountMap, account) => {
          accountMap.updated(account.address.toBase58, account)
        }
      }
      globalVaults = GlobalVaultsInfo(
        genesisAccountMap,
        initialPosStakingVault.address.toBase58,
        coopVault.address.toBase58,
        perValidatorVaults.map(_.address.toBase58)
      )
    } yield globalVaults

  def updateAccountMapFromDeploy(
      vaultMaps: Map[String, RevAccount],
      processedDeploy: ProcessedDeploy,
      specificPerValidatorAddr: String
  ): Map[String, RevAccount] = {
    val deployerRevAddr =
      RevAddress.fromPublicKey(processedDeploy.deploy.pk).get
    val deployerVault = vaultMaps.getOrElse(
      deployerRevAddr.toBase58,
      RevAccount(deployerRevAddr, 0, NormalVault)
    )
    val perValidatorVault =
      vaultMaps(specificPerValidatorAddr)
    val removeDeployerVault =
      deployerVault.sendRev(processedDeploy.cost.cost)
    val addPerValidatorVault =
      perValidatorVault.receiveRev(processedDeploy.cost.cost)
    vaultMaps
      .updated(
        removeDeployerVault.address.toBase58,
        removeDeployerVault
      )
      .updated(
        addPerValidatorVault.address.toBase58,
        addPerValidatorVault
      )
  }

  def updateAccountMapFromTransaction(
      vaultMap: Map[String, RevAccount],
      transaction: Transaction.TransactionInfo,
      perValidatorVault: Seq[String]
  ): Map[String, RevAccount] =
    if (transaction.success) {
      val fromVault =
        vaultMap(transaction.fromAddr)

      val toVault =
        if (perValidatorVault.contains(transaction.toAddr))
          vaultMap(perValidatorVault.head)
        else
          vaultMap.getOrElse(
            transaction.toAddr,
            RevAccount(
              RevAddress
                .parse(transaction.toAddr)
                .right
                .get,
              0,
              NormalVault
            )
          )
      val removeFromVault =
        fromVault.sendRev(transaction.amount)
      val addFromVault =
        toVault.receiveRev(transaction.amount)
      vaultMap
        .updated(
          removeFromVault.address.toBase58,
          removeFromVault
        )
        .updated(
          addFromVault.address.toBase58,
          addFromVault
        )
    } else vaultMap

  def traverseHistory[F[_]: Sync: Span: Log](
      genesisVaultMap: GlobalVaultsInfo,
      transactionStore: Transaction.TransactionStore[F],
      blockStore: BlockStore[F],
      dag: BlockDagRepresentation[F],
      specialCases: Map[Int, SpecialCase],
      targetBlock: BlockMessage
  ): F[GlobalVaultsInfo] =
    (1L to targetBlock.body.state.blockNumber).toList.foldLeftM(genesisVaultMap) {
      case (vaultsInfo, blockNumber) =>
        for {
          blocks <- dag.topoSort(blockNumber, Some(blockNumber))
          resultVaultMap <- blocks.flatten.foldLeftM(vaultsInfo.vaultMaps) {
                             case (vm, blockHash) =>
                               for {
                                 isFinalized <- dag.isFinalized(blockHash)
                                 result <- if (isFinalized) for {
                                            block <- blockStore.get(blockHash)
                                            afterCostDeployMap = block.get.body.deploys
                                              .foldLeft(vm) {
                                                case (vaultMaps, pd) =>
                                                  updateAccountMapFromDeploy(
                                                    vaultMaps,
                                                    pd,
                                                    vaultsInfo.perValidatorVaults.head
                                                  )
                                              }
                                            transactions <- transactionStore.get(
                                                             Base16.encode(blockHash.toByteArray)
                                                           )
                                            res = transactions.get.flatten
                                              .foldLeft(afterCostDeployMap) {
                                                case (vaultMap, transaction) =>
                                                  updateAccountMapFromTransaction(
                                                    vaultMap,
                                                    transaction,
                                                    vaultsInfo.perValidatorVaults
                                                  )
                                              }
                                            g = vaultsInfo.copy(vaultMaps = res)
                                            result = specialCases.get(
                                              block.get.body.state.blockNumber.toInt
                                            ) match {
                                              case Some(special) => special.handle(g)
                                              case None          => g
                                            }
                                          } yield result.vaultMaps
                                          else vm.pure[F]
                               } yield result
                           }
          newGlobalVaultInfo = vaultsInfo.copy(vaultMaps = resultVaultMap)
        } yield newGlobalVaultInfo
    }

  def getBlockHashByHeight[F[_]: Sync](
      blockNumber: Long,
      dag: BlockDagRepresentation[F],
      blockStore: BlockStore[F]
  ) =
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
  )(implicit scheduler: ExecutionContext): F[GlobalVaultsInfo] = {
    val oldRSpacePath                           = dataDir.resolve(s"$legacyRSpacePathPrefix/history/data.mdb")
    val legacyRSpaceDirSupport                  = Files.exists(oldRSpacePath)
    implicit val metrics: Metrics.MetricsNOP[F] = new Metrics.MetricsNOP[F]()
    import coop.rchain.rholang.interpreter.storage._
    implicit val span: NoopSpan[F]                           = NoopSpan[F]()
    implicit val log: Log[F]                                 = Log.log
    implicit val m: Match[F, BindPattern, ListParWithRandom] = matchListPar[F]
    for {
      rnodeStoreManager <- RNodeKeyValueStoreManager[F](dataDir, legacyRSpaceDirSupport)
      blockStore        <- KeyValueBlockStore(rnodeStoreManager)
      blockDagStorage   <- BlockDagKeyValueStorage.create[F](rnodeStoreManager)
      dagRepresantation <- blockDagStorage.getRepresentation
      store             <- rnodeStoreManager.rSpaceStores
      spaces <- RSpace
                 .createWithReplay[F, Par, BindPattern, ListParWithRandom, TaggedContinuation](
                   store
                 )
      (rSpacePlay, rSpaceReplay, _) = spaces
      runtimes                      <- RhoRuntime.createRuntimes[F](rSpacePlay, rSpaceReplay, true, Seq.empty)
      (rhoRuntime, _)               = runtimes
      targetBlock                   <- blockStore.get(ByteString.copyFrom(Base16.unsafeDecode(targetBlockHash)))
      genesisVaultMap               <- getGenesisVaultMap(walletPath, bondPath, rhoRuntime, targetBlock.get)
      transactionStore              <- Transaction.store(transactionDir)
      epoch250000                   <- getBlockHashByHeight(250000, dagRepresantation, blockStore)
      epoch500000                   <- getBlockHashByHeight(500000, dagRepresantation, blockStore)
      specialCases <- getSpecialCases(
                       rhoRuntime,
                       genesisVaultMap.posVaultAddress,
                       epoch250000,
                       epoch500000,
                       genesisVaultMap.perValidatorVaults.head
                     )
      result <- traverseHistory(
                 genesisVaultMap,
                 transactionStore,
                 blockStore,
                 dagRepresantation,
                 specialCases.toMap,
                 targetBlock.get
               )

    } yield result
  }
}
