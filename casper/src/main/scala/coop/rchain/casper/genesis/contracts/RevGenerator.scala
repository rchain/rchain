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
       #   rl!(`rho:rchain:revVault`, *revVaultCh)
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
       #             ${concatenate(findOrCreate)} |
       #             ${concatenate(transfer)}
       #           }
       #         }
       #       }
       #     }
       #   }
       # }
     """.stripMargin('#')

  val term: Par = ParBuilder[Coeval].buildNormalizedTerm(code, None).value()

  private def findOrCreate(userVault: Vault): String =
    s""" 
       # @RevVault!(
       #   "findOrCreate",
       #   "${userVault.revAddress.toBase58}",
       #   Nil
       # )
     """.stripMargin('#')

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
