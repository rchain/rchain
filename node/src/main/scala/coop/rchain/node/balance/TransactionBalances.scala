package coop.rchain.node.balance

import cats.Parallel
import cats.effect.concurrent.Semaphore
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
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.genesis.contracts.StandardDeploys
import coop.rchain.casper.protocol.{BlockMessage, ProcessedDeploy}
import coop.rchain.casper.util.rholang.RhoTrieTraverser
import coop.rchain.casper.util.{BondsParser, VaultParser}
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.node.balance.SpecialCases.{getSpecialCases, SpecialCase}
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import fs2._

import java.nio.file.{Files, Path}
import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext

object TransactionBalances {

  final case class Transfer(amount: Long, address: String, blockNumber: Long, in: Boolean)

  val initialPosStakingVault: RevAccount = RevAccount(
    RevAddress
      .fromPublicKey(
        Secp256k1.toPublic(PrivateKey(Base16.unsafeDecode(StandardDeploys.poSGeneratorPk)))
      )
      .get,
    0,
    PosStakingVault
  )

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

  def updateGenesisFromTransfer(genesisVault: GlobalVaultsInfo, transfers: List[Transfer]) = {
    val resultMap = transfers.foldLeft(genesisVault.vaultMaps) {
      case (m, transfer) => {
        val targetVault = m.getOrElse(
          transfer.address,
          RevAccount(
            address = RevAddress.parse(transfer.address).right.get,
            amount = 0L,
            accountType = NormalVault
          )
        )

        val resultVault =
          if (transfer.in) targetVault.receiveRev(transfer.amount)
          else targetVault.sendRev(transfer.amount)
        m.updated(transfer.address, resultVault)
      }
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

  def putTransfer[F[_]: Sync: Log](
      blockNumber: Long,
      transactionStore: Transaction.TransactionStore[F],
      blockStore: BlockStore[F],
      dag: BlockDagRepresentation[F],
      perValidatorVaultAddr: String
  ) =
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
                                            case (transfers, pd) => {
                                              val deployerRevAddr =
                                                RevAddress.fromPublicKey(pd.deploy.pk).get.toBase58

                                              transfers ++ Vector(
                                                Transfer(
                                                  pd.cost.cost,
                                                  deployerRevAddr,
                                                  block.body.state.blockNumber,
                                                  false
                                                ),
                                                Transfer(
                                                  pd.cost.cost,
                                                  perValidatorVaultAddr,
                                                  block.body.state.blockNumber,
                                                  true
                                                )
                                              )
                                            }
                                          }
                                        transactions <- transactionStore.get(
                                                         Base16.encode(blockHash.toByteArray)
                                                       )
                                        res = transactions.get.flatten
                                          .foldLeft(Vector.empty[Transfer]) {
                                            case (th, transaction) => {
                                              if (transaction.success) {
                                                th ++ Vector(
                                                  Transfer(
                                                    transaction.amount,
                                                    transaction.toAddr,
                                                    block.body.state.blockNumber,
                                                    true
                                                  ),
                                                  Transfer(
                                                    transaction.amount,
                                                    transaction.fromAddr,
                                                    block.body.state.blockNumber,
                                                    false
                                                  )
                                                )
                                              } else th
                                            }
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
      dag: BlockDagRepresentation[F],
      perValidatorVaultAddr: String
  ) =
    Stream
      .range(1, targetBlock.body.state.blockNumber.toInt)
      .parEvalMapUnorderedProcBounded(
        i =>
          putTransfer(
            i.toLong,
            transactionStore,
            blockStore,
            dag,
            perValidatorVaultAddr
          )
      )

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
  )(implicit scheduler: ExecutionContext) = {
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
      targetBlockOpt                <- blockStore.get(ByteString.copyFrom(Base16.unsafeDecode(targetBlockHash)))
      targetBlock                   = targetBlockOpt.get
      _                             <- log.info(s"Getting balance from ${targetBlock}")
      genesisVaultMap               <- getGenesisVaultMap(walletPath, bondPath, rhoRuntime, targetBlock)
      transactionStore              <- Transaction.store(transactionDir)
      tasks = getAllTransfer(
        targetBlock,
        transactionStore,
        blockStore,
        dagRepresantation,
        genesisVaultMap.perValidatorVaults.head
      )
      allTransfers <- tasks.compile.toList
      allTransfer  = allTransfers.flatten
      _ <- log.info(
            s"After getting transfer history total ${allTransfer.length} account make transfer."
          )
      afterTransferMap = updateGenesisFromTransfer(genesisVaultMap, allTransfer)
      epoch250000      <- getBlockHashByHeight(250000, dagRepresantation, blockStore)
      epoch500000      <- getBlockHashByHeight(500000, dagRepresantation, blockStore)
      specialCases <- getSpecialCases(
                       rhoRuntime,
                       afterTransferMap.posVaultAddress,
                       epoch250000,
                       epoch500000,
                       afterTransferMap.perValidatorVaults.head
                     )
      resultMap = specialCases.foldLeft(afterTransferMap) {
        case (v, (_, special)) => special.handle(v)
      }
    } yield resultMap
  }
}
