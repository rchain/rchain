package coop.rchain.casper.genesis.contracts

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.util.ProtoUtil.stringToByteString
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.models.NormalizerEnv
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.accounting

object StandardDeploys {
  private def toDeploy(
      compiledSource: CompiledRholangSource[_],
      privateKey: String,
      timestamp: Long
  ): Signed[DeployData] = {
    val sk = PrivateKey(stringToByteString(privateKey))
    val pk = Secp256k1.toPublic(sk)
    val deployData =
      DeployData(
        deployer = ByteString.copyFrom(pk.bytes),
        timestamp = timestamp,
        term = compiledSource.code,
        phloLimit = accounting.MAX_VALUE,
        phloPrice = 0,
        validAfterBlockNumber = 0
      )

    Signed(deployData, Secp256k1, sk)
  }

  def registry: Signed[DeployData] = toDeploy(
    CompiledRholangSource("Registry.rho"),
    "5a0bde2f5857124b1379c78535b07a278e3b9cefbcacc02e62ab3294c02765a1",
    1559156071321L
  )

  def listOps: Signed[DeployData] = toDeploy(
    CompiledRholangSource("ListOps.rho"),
    "867c21c6a3245865444d80e49cac08a1c11e23b35965b566bbe9f49bb9897511",
    1559156082324L
  )
  def either: Signed[DeployData] =
    toDeploy(
      CompiledRholangSource("Either.rho"),
      "5248f8913f8572d8227a3c7787b54bd8263389f7209adc1422e36bb2beb160dc",
      1559156217509L
    )
  def nonNegativeNumber: Signed[DeployData] =
    toDeploy(
      CompiledRholangSource("NonNegativeNumber.rho"),
      "e33c9f1e925819d04733db4ec8539a84507c9e9abd32822059349449fe03997d",
      1559156251792L
    )
  def makeMint: Signed[DeployData] =
    toDeploy(
      CompiledRholangSource("MakeMint.rho"),
      "de19d53f28d4cdee74bad062342d8486a90a652055f3de4b2efa5eb2fccc9d53",
      1559156452968L
    )

  def authKey: Signed[DeployData] =
    toDeploy(
      CompiledRholangSource("AuthKey.rho"),
      "f450b26bac63e5dd9343cd46f5fae1986d367a893cd21eedd98a4cb3ac699abc",
      1559156356769L
    )

  def revVault: Signed[DeployData] =
    toDeploy(
      CompiledRholangSource("RevVault.rho"),
      "27e5718bf55dd673cc09f13c2bcf12ed7949b178aef5dcb6cd492ad422d05e9d",
      1559156183943L
    )

  def multiSigRevVault: Signed[DeployData] =
    toDeploy(
      CompiledRholangSource("MultiSigRevVault.rho"),
      "2a2eaa76d6fea9f502629e32b0f8eea19b9de8e2188ec0d589fcafa98fb1f031",
      1571408470880L
    )

  def poSGenerator(poS: ProofOfStake): Signed[DeployData] =
    toDeploy(
      poS,
      "a9585a0687761139ab3587a4938fb5ab9fcba675c79fefba889859674046d4a5",
      1559156420651L
    )

  def revGenerator(vaults: Seq[Vault], supply: Long): Signed[DeployData] =
    toDeploy(
      RevGenerator(vaults, supply),
      "a06959868e39bb3a8502846686a23119716ecd001700baf9e2ecfa0dbf1a3247",
      1565818101792L
    )

  def treeHashMap: Signed[DeployData] =
    toDeploy(
      CompiledRholangSource("TreeHashMap.rho"),
      "d60d63541bb98f31b834a4acf8a5ce825a91d5b6edca4c8f2a4acf6aafa17937",
      1566326330483L
    )

}
