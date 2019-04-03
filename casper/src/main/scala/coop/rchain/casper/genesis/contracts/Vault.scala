package coop.rchain.casper.genesis.contracts
import coop.rchain.models.Par
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.ParBuilder
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.rholang.interpreter.util.codec.Base58
import monix.eval.Coeval

final case class Vault(revAddress: RevAddress, initialBalance: Long) extends CompiledRholangSource {

  val path: String = "<synthetic in Vault.scala>"

  val code: String =
    s""" new rl(`rho:registry:lookup`), revVaultCh in {
       |   rl!(`rho:id:1o93uitkrjfubh43jt19owanuezhntag5wh74c6ur5feuotpi73q8z`, *revVaultCh)
       |   | for (@(_, RevVault) <- revVaultCh) {
       |     @RevVault!(
       |       "findOrCreate",
       |       "${revAddress.toBase58}",
       |       Nil
       |     )
       |   }
       | }
     """.stripMargin

  val term: Par = ParBuilder[Coeval].buildNormalizedTerm(code).value()

}
