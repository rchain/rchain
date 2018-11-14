package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.models.Par

class Rev[A](
    rhoCode: A => String,
    wallets: Seq[A],
    faucetCode: String => String,
    posParams: ProofOfStakeParams
) extends CompiledRholangSource {
  private val initialTotalBond = posParams.validators.foldLeft(0L) {
    case (acc, v) => acc + v.stake
  }
  private val initialBondsCode = ProofOfStake.initialBondsCode(posParams.validators)

  private val minimumBond = posParams.minimumBond
  private val maximumBond = posParams.maximumBond

  final val code = s"""
    |//requires MakeMint, BasicWallet, WalletCheck, MakePoS
    |new
    |  rl(`rho:registry:lookup`), MakeMintCh, WalletCheckCh, BasicWalletCh,
    |  MakePoSCh, SystemInstancesCh, revMintCh, posPurseCh, rev, posCh
    |in {
    |  rl!(`rho:id:exunyijimapk7z43g3bbr69awqdz54kyroj9q43jgu3dh567fxsftx`, *MakeMintCh) |
    |  rl!(`rho:id:oqez475nmxx9ktciscbhps18wnmnwtm6egziohc3rkdzekkmsrpuyt`, *WalletCheckCh) |
    |  rl!(`rho:id:3yicxut5xtx5tnmnneta7actof4yse3xangw4awzt8c8owqmddgyms`, *BasicWalletCh) |
    |  rl!(`rho:id:nqt875jea4rr83383ys6guzsbebg6k7o7uhrint6d7km67886c4y4s`, *MakePoSCh) |
    |  rl!(`rho:id:wdwc36f4ixa6xacck3ddepmgueum7zueuczgthcqp6771kdu8jogm8`, *SystemInstancesCh) |
    |  for(
    |    @(_, MakeMint) <- MakeMintCh; @(_, WalletCheck) <- WalletCheckCh;
    |    @(_, BasicWallet) <- BasicWalletCh; @(_, MakePoS) <- MakePoSCh;
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
    |      ${faucetCode("revMint")} |
    |
    |      //PoS purse and contract creation
    |      @revMint!("makePurse", $initialTotalBond, *posPurseCh) |
    |      for(@posPurse <- posPurseCh) {
    |        @MakePoS!(posPurse, $minimumBond, $maximumBond, $initialBondsCode, *posCh) |
    |        for(@pos <- posCh) {
    |          @SystemInstancesRegistry!("register", "pos", bundle+{pos})
    |        }
    |      } |
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

  final override val term: Par = InterpreterUtil.mkTerm(code).right.get
}

class PreWalletRev(
    wallets: Seq[PreWallet],
    faucetCode: String => String,
    posParams: ProofOfStakeParams
) extends Rev[PreWallet](PreWallet.rhoCode, wallets, faucetCode, posParams)
