package coop.rchain.casper.genesis.contracts

import coop.rchain.models.NormalizerEnv.NormalizerEnv
import coop.rchain.models.{NormalizerEnv, Par}
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.ParBuilder
import monix.eval.Coeval

final case class RevGenerator(userVaults: Seq[Vault], supply: Long) extends CompiledRholangSource {

  val path: String = "<synthetic in Rev.scala>"

  val code: String =
    s""" new rl(`rho:registry:lookup`), revVaultCh in {
       #   rl!(`rho:rchain:revVault`, *revVaultCh) |
       #   for (@(_, RevVault) <- revVaultCh) {
       #     new ret in {
       #       @RevVault!("init", *ret) |
       #       for (vaultMapStore, initVault <- ret) {
       #         ${vaultInitCode()}
       #       }
       #     }
       #   }
       # }
     """.stripMargin('#')

  val normalizerEnv: NormalizerEnv = NormalizerEnv.Empty

  val term: Par =
    ParBuilder[Coeval]
      .buildNormalizedTerm(code, normalizerEnv)
      .value()

  private def vaultInitCode(): String =
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
       #{
       #${concatenate(mapEntry, separator = ",\n")}
       #}
       #""".stripMargin('#')

  private def mapEntry(userVault: Vault, index: Int): String =
    s"""  "${userVault.revAddress.toBase58}" : *x$index"""

  private def initVault(userVault: Vault, index: Int): String =
    s"""initVault!(*x$index, "${userVault.revAddress.toBase58}", ${userVault.initialBalance})"""

  private def concatenate(f: (Vault, Int) => String, separator: String = " |\n\n"): String =
    if (userVaults.nonEmpty) userVaults.zipWithIndex.map(Function.tupled(f)).mkString(separator)
    else "Nil"

}
