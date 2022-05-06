package coop.rchain.casper.genesis.contracts

import coop.rchain.models.NormalizerEnv
import coop.rchain.rholang.build.CompiledRholangSource

final class RevGenerator private (code: String)
    extends CompiledRholangSource(code, NormalizerEnv.Empty) {
  val path: String = "<synthetic in Rev.scala>"
}

object RevGenerator {

  // REV vault initialization in genesis is done in batches.
  // In the last batch `initContinue` channel will not receive
  // anything so further access to `RevVault(@"init", _)` is impossible.

  def apply(userVaults: Seq[Vault], isLastBatch: Boolean): RevGenerator = {
    val vaultBalanceList =
      userVaults.map(v => s"""("${v.revAddress.toBase58}", ${v.initialBalance})""").mkString(", ")

    val code: String =
      s""" new rl(`rho:registry:lookup`), revVaultCh in {
         #   rl!(`rho:rchain:revVault`, *revVaultCh) |
         #   for (@(_, RevVault) <- revVaultCh) {
         #     new revVaultInitCh in {
         #       @RevVault!("init", *revVaultInitCh) |
         #       for (TreeHashMap, @vaultMap, initVault, initContinue <- revVaultInitCh) {
         #         match [$vaultBalanceList] {
         #           vaults => {
         #             new iter in {
         #               contract iter(@[(addr, initialBalance) ... tail]) = {
         #                  iter!(tail) |
         #                  new vault, setDoneCh, logCh in {
         #                    initVault!(*vault, addr, initialBalance) |
         #                    TreeHashMap!("set", vaultMap, addr, (addr, *vault, *logCh), *setDoneCh) |
         #                    for (_ <- setDoneCh) { Nil }
         #                  }
         #               } |
         #               iter!(vaults) ${if (!isLastBatch) "| initContinue!()" else ""}
         #             }
         #           }
         #         }
         #       }
         #     }
         #   }
         # }
     """.stripMargin('#')

    new RevGenerator(code)
  }
}
