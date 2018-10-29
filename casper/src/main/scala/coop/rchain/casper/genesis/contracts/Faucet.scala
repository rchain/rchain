package coop.rchain.casper.genesis.contracts

import coop.rchain.rholang.mint.BasicWalletFaucet

/**
  * The purpose of a "Faucet" is to give a place where users
  * can obtain REV for testing their contracts for free on the
  * testnet. Since this is only applicable to testnet, the
  * `noopFaucet` will be used in main net, where there is no
  * way to obtain REV for free.
  */
object Faucet {
  //TODO: use registry instead of public names
  def basicWalletFaucet(mintName: String): String =
    s"""new rl(`rho:registry:lookup`), BasicWalletFaucetCh, faucetCh in {
       |  rl!(`rho:id:r3pfwhwyzfg3n3yhcndwuszkszr11rjdbksizz4eqbqnwg5w49kfo7`, *BasicWalletFaucetCh) |
       |  for(@(_, BasicWalletFaucet) <- BasicWalletFaucetCh) {
       |    @BasicWalletFaucet!($mintName, *faucetCh) |
       |    for(@faucet <- faucetCh){
       |      @SystemInstancesRegistry!("register", "faucet", faucet)
       |    }
       |  }
       |}""".stripMargin

  val noopFaucet: String => String = (mintName: String) =>
    """@SystemInstancesRegistry!("register", "faucet", Nil)"""

}
