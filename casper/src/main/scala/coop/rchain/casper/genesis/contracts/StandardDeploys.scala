package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.util.ProtoUtil.stringToByteString
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.{accounting, NormalizerEnv}
import coop.rchain.rholang.interpreter.util.RevAddress

object StandardDeploys {
  private def toDeploy(
      compiledSource: CompiledRholangSource,
      user: String,
      timestamp: Long
  ): DeployData = {
    val deployData = DeployData(
      deployer = stringToByteString(user),
      timestamp = timestamp,
      term = compiledSource.code,
      phloLimit = accounting.MAX_VALUE
    )

    deployData
  }

  def listOps: DeployData = toDeploy(
    CompiledRholangSource("ListOps.rho", NormalizerEnv.Empty),
    "040126690519dc9b0f52876cb13458e15697794dd87d7c6477707c7efa4cce8a36b634eab5056bd4e3ba385ab14a638e4ac7d3b3e4968da3d66933fc04bc7038b5",
    1559156082324L
  )
  def either: DeployData =
    toDeploy(
      CompiledRholangSource("Either.rho", NormalizerEnv.Empty),
      "04c71f6c7b87edf4bec14f16f715ee49c6fea918549abdf06c734d384b60ba922990317cc4bf68da8c85b455a65595cf7007f1e54bfd6be26ffee53d1ea6d7406b",
      1559156217509L
    )
  def nonNegativeNumber: DeployData =
    toDeploy(
      CompiledRholangSource("NonNegativeNumber.rho", NormalizerEnv.Empty),
      "04e1559d809924e564dce57e34646e155b144d2a504ce7ee519d7a5108fd42f1038d08d745e5ea21cb53d6aa7c7174a768fa373207a83bc947a20c6a02ece7a60e",
      1559156251792L
    )
  def makeMint: DeployData =
    toDeploy(
      CompiledRholangSource("MakeMint.rho", NormalizerEnv.Empty),
      "0470256c078e105d2958b9cf66f2161d83368f483c0219790277fb726a459be7f56a9a48bbecf72bcaed6a3515bd0a144faf6a6a8de8f6c9b3b7dff297eb371f28",
      1559156452968L
    )

  def authKey: DeployData =
    toDeploy(
      CompiledRholangSource("AuthKey.rho", NormalizerEnv.Empty),
      "04f4b4417f930e6fab5765ac0defcf9fce169982acfd046e7c27f9b14c0804014623c0439e5c8035e9607599a549303b5b6b90cd9685e6965278bddca65dac7510",
      1559156356769L
    )

  def lockbox: DeployData =
    toDeploy(
      CompiledRholangSource("Lockbox.rho", NormalizerEnv.Empty),
      "04c1a88afc0810d0b7e4dea817f458c9d0a1913ec3459fb91bb9acdf0d867873d5144366275c5a63b0225a5167c6a838bb02285072d7177dc9f6407aaba87bca93",
      1559156146649L
    )

  def revVault: DeployData =
    toDeploy(
      CompiledRholangSource("RevVault.rho", NormalizerEnv.Empty),
      "040f035630a5d2199184890b4b6b83440c842da0b6becca539f788f7b35d6e873561f673cd6ebe2e32236398a86f29dad992e8fba32534734300fcc5104bcfea0e",
      1559156183943L
    )

  def poSGenerator(poS: ProofOfStake): DeployData =
    toDeploy(
      poS,
      "047b43d6548b72813b89ac1b9f9ca67624a8b372feedd71d4e2da036384a3e1236812227e524e6f237cde5f80dbb921cac12e6500791e9a9ed1254a745a816fe1f",
      1559156420651L
    )

  def revGenerator(genesisPk: PublicKey, supply: Long): DeployData =
    toDeploy(
      RevGenerator(genesisPk, supply),
      Base16.encode(genesisPk.bytes),
      System.currentTimeMillis()
    )

  def vaultGenerator(genesisPk: PublicKey, vault: Vault): DeployData =
    toDeploy(
      VaultGenerator(genesisPk, vault),
      Base16.encode(genesisPk.bytes),
      System.currentTimeMillis()
    )

}
