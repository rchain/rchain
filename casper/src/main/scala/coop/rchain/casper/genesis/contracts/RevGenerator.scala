package coop.rchain.casper.genesis.contracts

import coop.rchain.models.Par
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.ParBuilder
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.rholang.interpreter.util.codec.Base58
import monix.eval.Coeval

final case class RevGenerator(genesisAddress: RevAddress, userVaults: Seq[Vault], supply: Long)
    extends CompiledRholangSource {

  val path: String = "<synthetic in Rev.scala>"

  val code: String =
    s""" new rl(`rho:registry:lookup`), revVaultCh in {
       #   rl!(`rho:id:1o93uitkrjfubh43jt19owanuezhntag5wh74c6ur5feuotpi73q8z`, *revVaultCh)
       #   | for (@(_, RevVault) <- revVaultCh) {
       #     new genesisVaultCh in {
       #       @RevVault!(
       #         "findOrCreateGenesisVault",
       #         "${genesisAddress.toBase58}",
       #         $supply,
       #         *genesisVaultCh
       #       )
       #       | for (@(true, genesisVault) <- genesisVaultCh) {
       #         new genesisAuthKeyCh in {
       #           @RevVault!("deployerAuthKey", *genesisAuthKeyCh)
       #           | for (genesisVaultAuthKey <- genesisAuthKeyCh) {
       #             ${concatenate(transfer)} |
       #             ${concatenate(_.code)}
       #           }
       #         }
       #       }
       #     }
       #   }
       # }
     """.stripMargin('#')

  val term: Par = ParBuilder[Coeval].buildNormalizedTerm(code).value()

  private def transfer(userVault: Vault): String =
    s"""
       # @genesisVault!(
       #   "transfer",
       #   "${userVault.revAddress.toBase58}",
       #   ${userVault.initialBalance},
       #   *genesisVaultAuthKey,
       #   Nil
       # )
     """.stripMargin('#')

  private def concatenate(f: Vault => String): String =
    if (userVaults.nonEmpty) userVaults.map(f).mkString(" |\n\n")
    else "Nil"

}
