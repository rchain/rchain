package coop.rchain.casper.genesis.contracts

import coop.rchain.rholang.mint.BasicWalletFaucet

object Faucet {
  //TODO: use registry instead of public names
  def basicWalletFaucet(mintName: String): String =
    s"""new faucetCh in {
       |  @"BasicWalletFaucet"!($mintName, *faucetCh) |
       |  for(@faucet <- faucetCh){
       |    @"faucet"!!(faucet)
       |  }
       |}""".stripMargin

  val noopFaucet: String => String = (mintName: String) => "Nil"

}
