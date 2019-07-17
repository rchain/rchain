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
       #         new genesisAuthKeyCh, deployerId(`rho:rchain:deployerId`) in {
       #           @RevVault!("deployerAuthKey", *deployerId, *genesisAuthKeyCh)
       #           | for (genesisVaultAuthKey <- genesisAuthKeyCh) {
       #             new loop, ack in {
       #               contract loop(@vaultList) = {
       #                 match vaultList {
       #                   [(revAddress, initialBalance) ...tail] => {
       #                     @RevVault!("findOrCreate", revAddress, *ack)
       #                     | for(_ <- ack){
       #                       @genesisVault!("transfer", revAddress, initialBalance, *genesisVaultAuthKey, *ack)
       #                       | for(_ <- ack){
       #                         loop!(tail)
       #                       }
       #                     }
       #                   }
       #                   _ => Nil
       #                 }
       #               } | loop!(
       #               ${concatenate {
         case Vault(revAddress, initialBalance) =>
           s"""("${revAddress.toBase58}", $initialBalance)"""
       }}
       #               )
       #             }
       #           }
       #         }
       #       }
       #     }
       #   }
       # }
     """.stripMargin('#')

  val normalizerEnv: NormalizerEnv = NormalizerEnv(deployId = none, deployerPk = genesisPk.some)

  val term: Par = ParBuilder[Coeval]
    .buildNormalizedTerm(code, normalizerEnv)
    .value()

  private def concatenate(f: Vault => String): String =
    userVaults.map(f).mkString("[", ", ", "]")

}
