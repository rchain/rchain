package coop.rchain.casper.genesis.contracts

import cats.implicits._
import coop.rchain.crypto.PublicKey
import coop.rchain.models.Par
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.{NormalizerEnv, ParBuilder}
import coop.rchain.rholang.interpreter.util.RevAddress
import monix.eval.Coeval

final case class RevGenerator(
    genesisPk: PublicKey,
    supply: Long
) extends CompiledRholangSource {

  val path: String = "<synthetic in Rev.scala>"

  val code: String =
    s""" new rl(`rho:registry:lookup`), revVaultCh in {
       #   rl!(`rho:rchain:revVault`, *revVaultCh)
       #   | for (@(_, RevVault) <- revVaultCh) {
       #     new genesisVaultCh in {
       #       @RevVault!(
       #         "findOrCreateGenesisVault",
       #         "${RevAddress.fromPublicKey(genesisPk).get.toBase58}",
       #         $supply,
       #         *genesisVaultCh
       #       )
       #     }
       #   }
       # }
     """.stripMargin('#')

  val normalizerEnv: NormalizerEnv = NormalizerEnv(deployId = none, deployerPk = genesisPk.some)

  val term: Par = ParBuilder[Coeval]
    .buildNormalizedTerm(code, normalizerEnv)
    .value()

}

final case class VaultGenerator(genesisPk: PublicKey, userVaults: Seq[Vault])
    extends CompiledRholangSource {

  val path: String = "<synthetic in Rev.scala>"

  val code: String =
    s""" new rl(`rho:registry:lookup`),
       #     deployerId(`rho:rchain:deployerId`),
       #     revVaultCh,
       #     genesisVaultCh,
       #     genesisVaultAuthKeyCh,
       #     ack in {
       #     rl!(`rho:rchain:revVault`, *revVaultCh)
       #     | for (@(_, RevVault) <- revVaultCh) {
       #       @RevVault!("findOrCreate", "${RevAddress
         .fromPublicKey(genesisPk)
         .get
         .toBase58}", *genesisVaultCh)
       #       | @RevVault!("deployerAuthKey", *deployerId, *genesisVaultAuthKeyCh)
       #       | for (@(true, genesisVault) <- genesisVaultCh; genesisVaultAuthKey <- genesisVaultAuthKeyCh){
       #         ${concatenate(findOrCreate)} |
       #         ${concatenate(transfer)}
       #       }
       #     }
       # }
     """.stripMargin('#')

  val normalizerEnv: NormalizerEnv = NormalizerEnv(deployId = none, deployerPk = genesisPk.some)

  val term: Par = ParBuilder[Coeval]
    .buildNormalizedTerm(code, normalizerEnv)
    .value()

  private def findOrCreate(userVault: Vault): String =
    s"""
       # @RevVault!(
       #   "findOrCreate",
       #   "${userVault.revAddress.toBase58}",
       #   *ack
       # )
     """.stripMargin('#')

  private def transfer(userVault: Vault): String =
    s"""
       # @genesisVault!(
       #   "transfer",
       #   "${userVault.revAddress.toBase58}",
       #   ${userVault.initialBalance},
       #   *genesisVaultAuthKey,
       #   *ack
       # )
     """.stripMargin('#')

  private def concatenate(f: Vault => String): String =
    if (userVaults.nonEmpty) userVaults.map(f).mkString(" |\n\n")
    else "Nil"

}
