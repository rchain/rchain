package coop.rchain.casper.genesis.contracts

import coop.rchain.models.NormalizerEnv
import coop.rchain.rholang.build.CompiledRholangSource

final class RevGenerator private (supply: Long, code: String)
    extends CompiledRholangSource(code, NormalizerEnv.Empty) {
  val path: String = "<synthetic in Rev.scala>"
}

object RevGenerator {
  private def concatenate(
      userVaults: Seq[Vault]
  )(f: (Vault, Int) => String, separator: String = " |\n\n"): String =
    if (userVaults.nonEmpty) userVaults.zipWithIndex.map(Function.tupled(f)).mkString(separator)
    else "Nil"

  private def vaultInitCode(userVaults: Seq[Vault]): String = {
    def initVault(userVault: Vault, index: Int): String =
      s"""initVault!(*x$index, "${userVault.revAddress.toBase58}", ${userVault.initialBalance})"""
    def mapEntry(userVault: Vault, index: Int): String =
      s"""  "${userVault.revAddress.toBase58}" : *x$index"""

    if (userVaults.isEmpty) {
      "vaultMapStore!({})"
    } else {
      val vaultsMap: String =
        s"""
           #{
           #${concatenate(userVaults)(mapEntry, separator = ",\n")}
           #}
           #""".stripMargin('#')
      s"""
         #new ${userVaults.indices.map("x" + _).mkString(", ")} in {
         #  vaultMapStore!($vaultsMap) |
         #  ${concatenate(userVaults)(initVault)}
         #}
         #""".stripMargin('#')
    }
  }

  def apply(userVaults: Seq[Vault], supply: Long): RevGenerator = {
    val code: String =
      s""" new rl(`rho:registry:lookup`), revVaultCh in {
         #   rl!(`rho:rchain:revVault`, *revVaultCh) |
         #   for (@(_, RevVault) <- revVaultCh) {
         #     new ret in {
         #       @RevVault!("init", *ret) |
         #       for (vaultMapStore, initVault <- ret) {
         #         ${vaultInitCode(userVaults)}
         #       }
         #     }
         #   }
         # }
     """.stripMargin('#')
    new RevGenerator(supply, code)
  }
}
