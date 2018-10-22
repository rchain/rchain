package coop.rchain.casper.genesis.contracts

import scala.util.{Failure, Success, Try}

case class PreWallet(ethAddress: String, initRevBalance: BigInt)

object PreWallet {

  /**
    * Produces Rholang code which adds a wallet to the blockchain based on the
    * given Wallet case class.
    * @param w the PreWallet object containing the information which will go
    *          on the blockchain.
    * @return  Rholang code to add the pre-wallet to the blockchain.
    */
  def rhoCode(w: PreWallet): String = s"""
    |new purseCh in {
    |  @revMint!("makePurse", ${w.initRevBalance}, *purseCh) |
    |  for(@purse <- purseCh) {
    |    @WalletCheck!("create", "${w.ethAddress}", purse)
    |  }
    |}""".stripMargin

  def fromLine(line: String): Either[String, PreWallet] = line.split(",").filter(_.nonEmpty) match {
    case Array(ethAddress, initRevBalanceStr, _) =>
      Try(BigInt(initRevBalanceStr)) match {
        case Success(initRevBalance) => Right(PreWallet(ethAddress, initRevBalance))
        case Failure(_) =>
          Left(s"Failed to parse given initial balance $initRevBalanceStr as int.")
      }

    case _ => Left(s"Invalid pre-wallet specification:\n$line")
  }
}
