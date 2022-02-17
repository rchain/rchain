package coop.rchain.node.revvaultexport.reporting

import cats.Parallel
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.{BlockDagKeyValueStorage, BlockDagRepresentation}
import coop.rchain.blockstorage.{BlockStore, KeyValueBlockStore}
import coop.rchain.casper.genesis.contracts.StandardDeploys
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.casper.storage.RNodeKeyValueStoreManager.legacyRSpacePathPrefix
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.{BondsParser, VaultParser}
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.node.revvaultexport.RhoTrieTraverser
import coop.rchain.node.web.{
  CloseBlock,
  PreCharge,
  Refund,
  SlashingDeploy,
  Transaction,
  TransactionInfo,
  UserDeploy
}
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.rspace.syntax._
import coop.rchain.rspace.{Match, RSpace}
import coop.rchain.models.syntax._
import coop.rchain.shared.{Base16, Log}

import java.nio.file.{Files, Path}
import scala.concurrent.ExecutionContext

object TransactionBalances {
  final case class TransactionBlockInfo(
      transaction: TransactionInfo,
      blockNumber: Long,
      isFinalized: Boolean
  )

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

  final case class DeployNotFound(transaction: TransactionInfo) extends Exception

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

  def generateRevAccountFromWalletAndBond[F[_]: Sync: ContextShift: Log](
      walletPath: Path,
      bondsPath: Path
  ): F[Map[String, RevAccount]] =
    for {
      bondsMap <- BondsParser.parse(bondsPath)
      vaults   <- VaultParser.parse(walletPath)
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

  def updateGenesisFromTransfer(
      genesisVault: GlobalVaultsInfo,
      transfers: List[TransactionBlockInfo]
  ): GlobalVaultsInfo = {
    val resultMap = transfers.foldLeft(genesisVault.vaultMaps) {
      case (m, transfer) =>
        if (transfer.isFinalized && transfer.transaction.transaction.failReason.isEmpty) {
          val fromAddr = transfer.transaction.transaction.fromAddr
          val toAddr   = transfer.transaction.transaction.toAddr
          val amount   = transfer.transaction.transaction.amount
          val fromVault = m.getOrElse(
            fromAddr,
            RevAccount(
              address = RevAddress.parse(fromAddr).right.get,
              amount = 0L,
              accountType = NormalVault
            )
          )
          val newVaultMap = m.updated(fromAddr, fromVault.sendRev(amount))
          val toVault = newVaultMap.getOrElse(
            toAddr,
            RevAccount(
              address = RevAddress.parse(toAddr).right.get,
              amount = 0L,
              accountType = NormalVault
            )
          )
          newVaultMap.updated(toAddr, toVault.receiveRev(amount))
        } else m

    }
    genesisVault.copy(vaultMaps = resultMap)
  }

  def getGenesisVaultMap[F[_]: Sync: ContextShift: Span: Log](
      walletPath: Path,
      bondsPath: Path,
      runtime: RhoRuntime[F],
      block: BlockMessage
  ): F[GlobalVaultsInfo] =
    for {
      vaultMap <- generateRevAccountFromWalletAndBond(walletPath, bondsPath)
      coopVault = vaultMap.getOrElse(
        CoopVaultAddr,
        RevAccount(RevAddress.parse(CoopVaultAddr).right.get, 0, CoopPosMultiSigVault)
      )
      perValidatorVaults <- getPerValidatorVaults(runtime, block).map(
                             addrs =>
                               addrs.map(
                                 addr =>
                                   vaultMap.getOrElse(
                                     addr,
                                     RevAccount(
                                       RevAddress.parse(addr).right.get,
                                       0,
                                       PerValidatorVault
                                     )
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
      targetBlockHash: String
  )(implicit scheduler: ExecutionContext): F[(GlobalVaultsInfo, List[TransactionBlockInfo])] = {
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
      store             <- rnodeStoreManager.rSpaceStores
      spaces <- RSpace
                 .createWithReplay[F, Par, BindPattern, ListParWithRandom, TaggedContinuation](
                   store
                 )
      (rSpacePlay, rSpaceReplay) = spaces
      runtimes <- RhoRuntime
                   .createRuntimes[F](rSpacePlay, rSpaceReplay, initRegistry = true, Seq.empty)
      (rhoRuntime, _)    = runtimes
      targetBlockOpt     <- blockStore.get(targetBlockHash.unsafeHexToByteString)
      targetBlock        = targetBlockOpt.get
      _                  <- log.info(s"Getting balance from $targetBlock")
      genesisVaultMap    <- getGenesisVaultMap(walletPath, bondPath, rhoRuntime, targetBlock)
      transactionStore   <- Transaction.store(rnodeStoreManager)
      allTransactionsMap <- transactionStore.toMap
      allTransactions    = allTransactionsMap.values.flatMap(_.data)
      blockDagStorage    <- BlockDagKeyValueStorage.create[F](rnodeStoreManager)
      dagRepresantation  <- blockDagStorage.getRepresentation
      allWrappedTransactions <- {
        def findTransaction(transaction: TransactionInfo): F[ByteString] =
          transaction.transactionType match {
            case PreCharge(deployId) =>
              dagRepresantation
                .lookupByDeployId(deployId.unsafeHexToByteString)
                .flatMap(_.liftTo(DeployNotFound(transaction)))
            case Refund(deployId) =>
              dagRepresantation
                .lookupByDeployId(deployId.unsafeHexToByteString)
                .flatMap(_.liftTo(DeployNotFound(transaction)))
            case UserDeploy(deployId) =>
              dagRepresantation
                .lookupByDeployId(deployId.unsafeHexToByteString)
                .flatMap(_.liftTo(DeployNotFound(transaction)))
            case CloseBlock(blockHash) =>
              blockHash.unsafeHexToByteString.pure[F]
            case SlashingDeploy(blockHash) =>
              blockHash.unsafeHexToByteString.pure[F]
          }
        allTransactions.toList.traverse { t =>
          for {
            blockHash    <- findTransaction(t)
            blockMetaOpt <- dagRepresantation.lookup(blockHash)
            blockMeta <- blockMetaOpt.liftTo(
                          new Exception(s"Block ${blockHash.toHexString} not found in dag")
                        )
            isFinalized         <- dagRepresantation.isFinalized(blockHash)
            isBeforeTargetBlock = blockMeta.blockNum <= targetBlock.body.state.blockNumber
          } yield TransactionBlockInfo(t, blockMeta.blockNum, isFinalized && isBeforeTargetBlock)
        }
      }
      allSortedTransactions = allWrappedTransactions.sortBy(_.blockNumber)
      _ <- log.info(
            s"Transaction history from ${allSortedTransactions.head} to ${allSortedTransactions.tail}"
          )
      afterTransferMap = updateGenesisFromTransfer(genesisVaultMap, allSortedTransactions)
    } yield (afterTransferMap, allSortedTransactions)
  }
}
