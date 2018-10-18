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
    |//requires MakeMint, BasicWallet
    |new revMintCh, posPurseCh in {
    |  @"MakeMint"!(*revMintCh) | for(@revMint <- revMintCh) {
    |    //TODO: How should the revMint unforgeable name be exposed (if at all)?
    |
    |    //public contract for making empty rev purses
    |    contract @("Rev", "makePurse")(return) = {
    |       @(revMint, "makePurse")!(0, *return)
    |    } |
    |
    |    ${faucetCode("revMint")} |
    |
    |    //PoS purse and contract creation
    |    @(revMint, "makePurse")!($initialTotalBond, *posPurseCh) |
    |    for(@posPurse <- posPurseCh) {
    |      @"MakePoS"!(posPurse, $minimumBond, $maximumBond, $initialBondsCode, "proofOfStake")
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

class PreWalletRev(
    wallets: Seq[PreWallet],
    faucetCode: String => String,
    posParams: ProofOfStakeParams
) extends Rev[PreWallet](PreWallet.rhoCode, wallets, faucetCode, posParams)
