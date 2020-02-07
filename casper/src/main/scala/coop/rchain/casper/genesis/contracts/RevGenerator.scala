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
    def treeHashMapEntry(userVault: Vault, index: Int): String =
      s"""TreeHashMap!("set", vaultMap, "${userVault.revAddress.toBase58}", *x$index, *ack$index)"""

    if (userVaults.isEmpty) {
      "Nil"
    } else {
      val vaultsMap: String =
        s"""${concatenate(userVaults)(treeHashMapEntry, separator = "| \n")}"""

      // Sets all vaults in the TreeHashMap and catches the acknowledgements from each set.
      s"""
         #new ${userVaults.indices.map(i => s"x$i, ack$i").mkString(", ")} in {
         #  $vaultsMap |
         #  for (${userVaults.indices.map("_ <- ack" + _).mkString("; ")}) {
         #    ${concatenate(userVaults)(initVault)}
         #  }
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
         #       for (TreeHashMap, @vaultMap, initVault <- ret) {
         #         ${vaultInitCode(userVaults)}
         #       }
         #     }
         #   }
         # }
     """.stripMargin('#')
    new RevGenerator(supply, code)
  }
}
