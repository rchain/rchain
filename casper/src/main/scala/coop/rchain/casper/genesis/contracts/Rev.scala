package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.models.Par

class Rev[A](rhoCode: A => String, wallets: Seq[A]) extends CompiledRholangSource {
  final val code = s"""
    |//requires MakeMint, BasicWallet
    |new revMintCh in {
    |  @"MakeMint"!(*revMintCh) | for(@revMint <- revMintCh) {
    |    //TODO: How should the revMint unforgeable name be exposed (if at all)?
    |
    |    //public contract for making empty rev purses
    |    contract @("Rev", "makePurse")(return) = {
    |       @(revMint, "makePurse")!(0, *return)
    |    } |
    |
    |    //basic wallets which exist from genesis
    |    $walletCode
    |  }
    |}
  """.stripMargin

  private[this] def walletCode: String =
    if (wallets.isEmpty) {
      "Nil"
    } else {
      wallets.map(rhoCode).mkString(" |\n")
    }

  final override val term: Par = InterpreterUtil.mkTerm(code).right.get
}

class PreWalletRev(wallets: Seq[PreWallet]) extends Rev[PreWallet](PreWallet.rhoCode, wallets)
