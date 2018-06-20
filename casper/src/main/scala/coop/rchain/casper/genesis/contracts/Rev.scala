package coop.rchain.rholang.genesis.contracts

import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.models.Par

final class Rev(wallets: Seq[Wallet]) extends CompiledRholangSource {
  val code = s"""
    |//requires MakeMint, BasicWallet
    |new revMintCh in {
    |  @"MakeMint"!(*revMintCh) | for(@revMint <- revMintCh) {
    |    //TODO: How should the revMint unforgeable name be exposed (if at all)?
    |
    |    //public contract for making empty rev purses
    |    contract @["Rev", "makePurse"](return) = {
    |       @[revMint, "makePurse"]!(0, *return)
    |    } |
    |
    |    //basic wallets which exist from genesis
    |    ${wallets.map(Wallet.rhoCode).mkString(" |\n")}
    |  }
    |}
  """.stripMargin

  override val term: Par = InterpreterUtil.mkTerm(code).right.get
}
