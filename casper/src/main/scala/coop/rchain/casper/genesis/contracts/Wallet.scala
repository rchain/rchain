package coop.rchain.casper.genesis.contracts

import scala.util.{Failure, Success, Try}

case class Wallet(algorithm: String, hash: String, initRevBalance: Int)

object Wallet {

  /**
    * Produces Rholang code which adds a wallet to the blockchain based on the
    * given Wallet case class.
    * @param w the Wallet object containing the information which will go
    *          on the blockchain.
    * @return  Rholang code to add the wallet to the blockchain.
    */
  def rhoCode(w: Wallet): String = s"""
    |new purseCh, walletCh in {
    |  @[revMint, "makePurse"]!(${w.initRevBalance}, *purseCh) |
    |  for(@purse <- purseCh) {
    |    @["WalletCheck", "create"]!(purse, "${w.algorithm}", "${w.hash}")
    |  }
    |}""".stripMargin

  def fromLine(line: String): Either[String, Wallet] = line.split(" ").filter(_.nonEmpty) match {
    case Array(algorithm, hash, initRevBalanceStr) =>
      Try(initRevBalanceStr.toInt) match {
        case Success(initRevBalance) => Right(Wallet(algorithm, hash, initRevBalance))
        case Failure(_) =>
          Left(s"Failed to parse given initial balance $initRevBalanceStr as int.")
      }

    case _ => Left(s"Invalid wallet specification:\n$line")
  }
}
