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
  def either: Deploy =
    toDeploy(
      Either,
      "89a6d9c47f360e8ce145f8fe3c773786dc86bd0e70d19643d02b0eb126473c55",
      1539794228064L
    )
  def nonNegativeNumber: Deploy =
    toDeploy(
      NonNegativeNumber,
      "d89a1e6d2b8f53595b3d0d47effd48f0e537d19d847ad5811cf5216157a3a63c",
      1539963224985L
    )
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
