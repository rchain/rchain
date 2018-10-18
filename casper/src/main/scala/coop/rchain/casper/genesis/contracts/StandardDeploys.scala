package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.protocol.{Deploy, DeployData}
import coop.rchain.casper.util.ProtoUtil.stringToByteString
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.collection.{Either, ListOps}
import coop.rchain.rholang.interpreter.accounting
import coop.rchain.rholang.math.NonNegativeNumber
import coop.rchain.rholang.mint.{BasicWalletFaucet, MakeMint}
import coop.rchain.rholang.proofofstake.MakePoS
import coop.rchain.rholang.wallet.{BasicWallet, WalletCheck}

object StandardDeploys {
  private def toDeploy(
      compiledSource: CompiledRholangSource,
      user: String,
      timestamp: Long
  ): Deploy = {
    val deployData = DeployData(
      user = stringToByteString(user),
      timestamp = timestamp,
      term = compiledSource.code,
      phloLimit = Some(accounting.MAX_VALUE)
    )

    Deploy(
      term = Some(compiledSource.term),
      raw = Some(deployData)
    )
  }

  def listOps: Deploy = toDeploy(
    ListOps,
    "1d325ed35924b606264d4beaee7f78214aaecb23f6f3816055bc8bbe94280b5a",
    1539711168714L
  )
  def either: Deploy            = toDeploy(Either, "", 0L)
  def nonNegativeNumber: Deploy = toDeploy(NonNegativeNumber, "", 0L)
  def makeMint: Deploy          = toDeploy(MakeMint, "", 0L)
  def makePoS: Deploy           = toDeploy(MakePoS, "", 0L)
  def basicWallet: Deploy       = toDeploy(BasicWallet, "", 0L)
  def basicWalletFaucet: Deploy = toDeploy(BasicWalletFaucet, "", 0L)
  def walletCheck: Deploy       = toDeploy(WalletCheck, "", 0L)
  def rev(
      wallets: Seq[PreWallet],
      faucetCode: String => String,
      posParams: ProofOfStakeParams
  ): Deploy = toDeploy(new PreWalletRev(wallets, faucetCode, posParams), "", 0L)
}
