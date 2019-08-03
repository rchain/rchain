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
    genesisAddress: RevAddress,
    userVaults: Seq[Vault],
    supply: Long
) extends CompiledRholangSource {

  val path: String = "<synthetic in Rev.scala>"

  val code: String =
    s""" new rl(`rho:registry:lookup`), revVaultCh in {
       #   rl!(`rho:rchain:revVault`, *revVaultCh) |
       #   for (@(_, RevVault) <- revVaultCh) {
       #
       #     new genesisVaultCh in {
       #       @RevVault!(
       #         "findOrCreateGenesisVault",
       #         "${genesisAddress.toBase58}",
       #         $supply,
       #         *genesisVaultCh
       #       )
       #     } |
       #
       #     new ret in {
       #       @RevVault!("init", *ret) |
       #       for (vaultMapStore, initVault <- ret) {
       #         ${vaultInitCode()}
       #       }
       #     }
       #
       #   }
       # }
     """.stripMargin('#')

  val normalizerEnv: NormalizerEnv = NormalizerEnv(deployId = none, deployerPk = genesisPk.some)

  val term: Par =
    ParBuilder[Coeval]
      .buildNormalizedTerm(code, normalizerEnv)
      .value()

  private def vaultInitCode() =
    if (userVaults.isEmpty) {
      "vaultMapStore!({})"
    } else {
      s"""
        #new ${userVaults.indices.map("x" + _).mkString(", ")} in {
        #  vaultMapStore!(${vaultsMap()}) |
        #  ${concatenate(initVault)}
        #}
        #""".stripMargin('#')
    }

  private def vaultsMap(): String =
    s"""
       |{
       |${concatenate(mapEntry, separator = ",\n")}
       |}
       |""".stripMargin

  private def mapEntry(userVault: Vault, index: Int): String =
    s"""  "${userVault.revAddress.toBase58}" : *x$index"""

  private def initVault(userVault: Vault, index: Int): String =
    s"""initVault!(*x$index, "${userVault.revAddress.toBase58}", ${userVault.initialBalance})"""

  private def concatenate(f: (Vault, Int) => String, separator: String = " |\n\n"): String =
    if (userVaults.nonEmpty) userVaults.zipWithIndex.map(Function.tupled(f)).mkString(separator)
    else "Nil"

}
