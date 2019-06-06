package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.models.Par
import coop.rchain.rholang.build.CompiledRholangSource

class Rev[A](
    rhoCode: A => String,
    wallets: Seq[A],
    faucetCode: String => String,
    posParams: ProofOfStake
) extends CompiledRholangSource {
  final val path = "<synthetic in Rev.scala>"

  final val code = s"""
    |//requires MakeMint, BasicWallet, WalletCheck
    |new
    |  rl(`rho:registry:lookup`), MakeMintCh, WalletCheckCh, BasicWalletCh,
    |  SystemInstancesCh, revMintCh, posPurseCh, rev, posCh
    |in {
    |  rl!(`rho:rchain:makeMint`, *MakeMintCh) |
    |  rl!(`rho:rchain:walletCheck`, *WalletCheckCh) |
    |  rl!(`rho:rchain:basicWallet`, *BasicWalletCh) |
    |  rl!(`rho:rchain:systemInstancesRegistry`, *SystemInstancesCh) |
    |  for(
    |    @(_, MakeMint) <- MakeMintCh; @(_, WalletCheck) <- WalletCheckCh;
    |    @(_, BasicWallet) <- BasicWalletCh;
    |    @(_, SystemInstancesRegistry) <- SystemInstancesCh
    |  ) {
    |    @MakeMint!(*revMintCh) | for(@revMint <- revMintCh) {
    |      //TODO: How should the revMint unforgeable name be exposed (if at all)?
    |
    |      //public contract for making empty rev purses
    |      contract rev(@"makePurse", return) = {
    |         @revMint!("makePurse", 0, *return)
    |      } |
    |      @SystemInstancesRegistry!("register", "rev", bundle+{*rev}) |
    |
    |      //basic wallets which exist from genesis
    |      $walletCode
    |    }
    |  }
    |}
  """.stripMargin

  private[this] def walletCode: String =
    if (wallets.isEmpty) {
      "Nil"
    } else {
      wallets.map(rhoCode).mkString(" |\n")
    }

  override final val term: Par = InterpreterUtil.mkTerm(code).right.get
}

class PreWalletRev(
    wallets: Seq[PreWallet],
    faucetCode: String => String,
    posParams: ProofOfStake
) extends Rev[PreWallet](PreWallet.rhoCode, wallets, faucetCode, posParams)
