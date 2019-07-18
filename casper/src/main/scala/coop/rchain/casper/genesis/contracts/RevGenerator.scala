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

final case class VaultGenerator(genesisPk: PublicKey, userVault: Vault)
    extends CompiledRholangSource {

  val path: String = "<synthetic in Rev.scala>"

  val code: String =
    s""" new rl(`rho:registry:lookup`), stdout(`rho:io:stdout`), revVaultCh in {
       #   rl!(`rho:rchain:revVault`, *revVaultCh)
       #   | for (@(_, RevVault) <- revVaultCh) {
       #     new ack, genesisVaultCh in {
       #       @RevVault!("findOrCreate", "${userVault.revAddress.toBase58}", *ack)
       #       | @RevVault!("findOrCreate", "${RevAddress
         .fromPublicKey(genesisPk)
         .get
         .toBase58}", *genesisVaultCh)
       #       | for(@(true, _) <- ack; @(true, genesisVault) <- genesisVaultCh){
       #         new genesisAuthKeyCh, deployerId(`rho:rchain:deployerId`) in {
       #           @RevVault!("deployerAuthKey", *deployerId, *genesisAuthKeyCh)
       #           | for (genesisVaultAuthKey <- genesisAuthKeyCh) {
       #             @genesisVault!(
       #               "transfer",
       #               "${userVault.revAddress.toBase58}",
       #               ${userVault.initialBalance},
       #               *genesisVaultAuthKey,
       #               *ack
       #             )
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

}
