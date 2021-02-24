package coop.rchain.node.balance

import cats.effect.Sync
import cats.implicits._
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.node.balance.TransactionBalances.{GlobalVaultsInfo, RevAccount}
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.shared.Log
import coop.rchain.casper.syntax._
import coop.rchain.metrics.Span

object SpecialCases {
  final case class SpecialCase(
      blockNumber: Long,
      handle: GlobalVaultsInfo => GlobalVaultsInfo
  )

  def getBalanceContract(revAddress: String) =
    s"""new return, rl(`rho:registry:lookup`), RevVaultCh, vaultCh, balanceCh in {
      |  rl!(`rho:rchain:revVault`, *RevVaultCh) |
      |  for (@(_, RevVault) <- RevVaultCh) {
      |    @RevVault!("findOrCreate", "${revAddress}", *vaultCh) |
      |    for (@(true, vault) <- vaultCh) {
      |      @vault!("balance", *balanceCh) |
      |      for (@balance <- balanceCh) {
      |        return!(balance)
      |      }
      |    }
      |  }
      |}
      |""".stripMargin

  val slashAt463304 = (
    463304,
    SpecialCase(
      463304,
      v => {
        val slashAmount     = 150000000000004L
        val posVault        = v.vaultMaps(v.posVaultAddress)
        val coopVault       = v.vaultMaps(v.coopPosMultiSigVault)
        val removePostVault = posVault.sendRev(slashAmount)
        val addCoopVault    = coopVault.receiveRev(slashAmount)
        val newVaults = v.vaultMaps
          .updated(removePostVault.address.toBase58, removePostVault)
          .updated(addCoopVault.address.toBase58, addCoopVault)
        v.copy(vaultMaps = newVaults)
      }
    )
  )

  def bondAt250000(toPosVaultAmount: Long, perValidatorVaultAddr: String) =
    (
      250000,
      SpecialCase(
        250000,
        v => {
          val posVault                = v.vaultMaps(v.posVaultAddress)
          val perValidatorVault       = v.vaultMaps(perValidatorVaultAddr)
          val removePerValidatorVault = perValidatorVault.sendRev(toPosVaultAmount)
          val addPosVault             = posVault.receiveRev(toPosVaultAmount)
          val newVaults = v.vaultMaps
            .updated(removePerValidatorVault.address.toBase58, removePerValidatorVault)
            .updated(addPosVault.address.toBase58, addPosVault)
          v.copy(vaultMaps = newVaults)
        }
      )
    )

  val slashAt166708 = (
    166708,
    SpecialCase(
      166708,
      v => {
        val slashAmount     = 81703424000000L
        val posVault        = v.vaultMaps(v.posVaultAddress)
        val coopVault       = v.vaultMaps(v.coopPosMultiSigVault)
        val removePostVault = posVault.sendRev(slashAmount)
        val addCoopVault    = coopVault.receiveRev(slashAmount)
        val newVaults = v.vaultMaps
          .updated(removePostVault.address.toBase58, removePostVault)
          .updated(addCoopVault.address.toBase58, addCoopVault)
        v.copy(vaultMaps = newVaults)
      }
    )
  )

  def bondAt500000(toPosVaultAmount: Long, perValidatorVaultAddr: String) =
    (
      500000,
      SpecialCase(
        500000,
        v => {
          val posVault                = v.vaultMaps(v.posVaultAddress)
          val perValidatorVault       = v.vaultMaps(perValidatorVaultAddr)
          val removePerValidatorVault = perValidatorVault.sendRev(toPosVaultAmount)
          val addPosVault             = posVault.receiveRev(toPosVaultAmount)
          val newVaults = v.vaultMaps
            .updated(removePerValidatorVault.address.toBase58, removePerValidatorVault)
            .updated(addPosVault.address.toBase58, addPosVault)
          v.copy(vaultMaps = newVaults)
        }
      )
    )

  def getVaultChangesInBlock[F[_]: Sync: Span: Log](
      runtime: RhoRuntime[F],
      posVaultAddr: String,
      block: BlockMessage
  ) =
    for {
      preAmount <- runtime
                    .playExploratoryDeploy(
                      getBalanceContract(posVaultAddr),
                      block.body.state.preStateHash
                    )
                    .map(p => p.head.exprs.head.getGInt)
      postAmount <- runtime
                     .playExploratoryDeploy(
                       getBalanceContract(posVaultAddr),
                       block.body.state.postStateHash
                     )
                     .map(p => p.head.exprs.head.getGInt)

    } yield postAmount - preAmount

  def getSpecialCases[F[_]: Sync: Span: Log](
      runtime: RhoRuntime[F],
      posVaultAddr: String,
      epoch250000: BlockMessage,
      epoch500000: BlockMessage,
      perValidatorVaultAddr: String
  ) =
    for {
      epoch25PosChanges <- getVaultChangesInBlock(runtime, posVaultAddr, epoch250000)
      epoch50PosChanges <- getVaultChangesInBlock(runtime, posVaultAddr, epoch500000)

    } yield Seq(
      slashAt166708,
      bondAt250000(epoch25PosChanges, perValidatorVaultAddr),
      slashAt463304,
      bondAt500000(epoch50PosChanges, perValidatorVaultAddr)
    )
}
