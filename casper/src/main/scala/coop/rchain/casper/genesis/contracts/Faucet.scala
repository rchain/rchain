package coop.rchain.casper.genesis.contracts

import coop.rchain.rholang.mint.BasicWalletFaucet

object Faucet {
  //TODO: use registry instead of public names
  def basicWalletFaucet(mintName: String): String =
    s"""@"BasicWalletFaucet"!($mintName, "faucet")"""

  val noopFaucet: String => String = (mintName: String) => "Nil"

}
