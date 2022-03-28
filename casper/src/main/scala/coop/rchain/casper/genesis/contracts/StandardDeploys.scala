package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.accounting
import coop.rchain.shared.Base16
import coop.rchain.models.syntax._

object StandardDeploys {
  private def toDeploy(
      compiledSource: CompiledRholangSource[_],
      privateKey: String,
      timestamp: Long,
      shardId: String
  ): Signed[DeployData] = {
    val sk = PrivateKey(privateKey.unsafeHexToByteString)
    val deployData =
      DeployData(
        timestamp = timestamp,
        term = compiledSource.code,
        phloLimit = accounting.MAX_VALUE,
        phloPrice = 0,
        validAfterBlockNumber = 0,
        shardId = shardId
      )

    Signed(deployData, Secp256k1, sk)
  }

  // Private keys used to sign blessed (standard) contracts
  val registryPk          = "5a0bde2f5857124b1379c78535b07a278e3b9cefbcacc02e62ab3294c02765a1"
  val listOpsPk           = "867c21c6a3245865444d80e49cac08a1c11e23b35965b566bbe9f49bb9897511"
  val eitherPk            = "5248f8913f8572d8227a3c7787b54bd8263389f7209adc1422e36bb2beb160dc"
  val nonNegativeNumberPk = "e33c9f1e925819d04733db4ec8539a84507c9e9abd32822059349449fe03997d"
  val makeMintPk          = "de19d53f28d4cdee74bad062342d8486a90a652055f3de4b2efa5eb2fccc9d53"
  val authKeyPk           = "f450b26bac63e5dd9343cd46f5fae1986d367a893cd21eedd98a4cb3ac699abc"
  val revVaultPk          = "27e5718bf55dd673cc09f13c2bcf12ed7949b178aef5dcb6cd492ad422d05e9d"
  val multiSigRevVaultPk  = "2a2eaa76d6fea9f502629e32b0f8eea19b9de8e2188ec0d589fcafa98fb1f031"
  val poSGeneratorPk      = "a9585a0687761139ab3587a4938fb5ab9fcba675c79fefba889859674046d4a5"
  val revGeneratorPk      = "a06959868e39bb3a8502846686a23119716ecd001700baf9e2ecfa0dbf1a3247"

  val (registryPubKey, registryTimestamp) = (toPublic(registryPk), 1559156071321L)
  val (listOpsPubKey, listOpsTimestamp)   = (toPublic(listOpsPk), 1559156082324L)
  val (eitherPubKey, eitherTimestamp)     = (toPublic(eitherPk), 1559156217509L)
  val (nonNegativeNumberPubKey, nonNegativeNumberTimestamp) =
    (toPublic(nonNegativeNumberPk), 1559156251792L)
  val (makeMintPubKey, makeMintTimestamp) = (toPublic(makeMintPk), 1559156452968L)
  val (authKeyPubKey, authKeyTimestamp)   = (toPublic(authKeyPk), 1559156356769L)
  val (revVaultPubKey, revVaultTimestamp) = (toPublic(revVaultPk), 1559156183943L)
  val (multiSigRevVaultPubKey, multiSigRevVaultTimestamp) =
    (toPublic(multiSigRevVaultPk), 1571408470880L)
  val (poSGeneratorPubKey, poSGeneratorTimestamp) = (toPublic(poSGeneratorPk), 1559156420651L)
  val revGeneratorPubKey: PublicKey               = toPublic(revGeneratorPk)

  // Public keys used to sign blessed (standard) contracts
  val systemPublicKeys: Seq[PublicKey] = Seq(
    registryPubKey,
    listOpsPubKey,
    eitherPubKey,
    nonNegativeNumberPubKey,
    makeMintPubKey,
    authKeyPubKey,
    revVaultPubKey,
    multiSigRevVaultPubKey,
    poSGeneratorPubKey,
    revGeneratorPubKey
  )

  def registry(shardId: String): Signed[DeployData] = toDeploy(
    CompiledRholangSource("Registry.rho"),
    registryPk,
    registryTimestamp,
    shardId
  )

  def listOps(shardId: String): Signed[DeployData] = toDeploy(
    CompiledRholangSource("ListOps.rho"),
    listOpsPk,
    listOpsTimestamp,
    shardId
  )

  def either(shardId: String): Signed[DeployData] =
    toDeploy(
      CompiledRholangSource("Either.rho"),
      eitherPk,
      eitherTimestamp,
      shardId
    )

  def nonNegativeNumber(shardId: String): Signed[DeployData] =
    toDeploy(
      CompiledRholangSource("NonNegativeNumber.rho"),
      nonNegativeNumberPk,
      nonNegativeNumberTimestamp,
      shardId
    )

  def makeMint(shardId: String): Signed[DeployData] =
    toDeploy(
      CompiledRholangSource("MakeMint.rho"),
      makeMintPk,
      makeMintTimestamp,
      shardId
    )

  def authKey(shardId: String): Signed[DeployData] =
    toDeploy(
      CompiledRholangSource("AuthKey.rho"),
      authKeyPk,
      authKeyTimestamp,
      shardId
    )

  def revVault(shardId: String): Signed[DeployData] =
    toDeploy(
      CompiledRholangSource("RevVault.rho"),
      revVaultPk,
      revVaultTimestamp,
      shardId
    )

  def multiSigRevVault(shardId: String): Signed[DeployData] =
    toDeploy(
      CompiledRholangSource("MultiSigRevVault.rho"),
      multiSigRevVaultPk,
      multiSigRevVaultTimestamp,
      shardId
    )

  def poSGenerator(poS: ProofOfStake, shardId: String): Signed[DeployData] =
    toDeploy(
      poS,
      poSGeneratorPk,
      poSGeneratorTimestamp,
      shardId
    )

  def revGenerator(
      vaults: Seq[Vault],
      supply: Long,
      timestamp: Long,
      isLastBatch: Boolean,
      shardId: String
  ): Signed[DeployData] =
    toDeploy(
      RevGenerator(vaults, supply, isLastBatch),
      revGeneratorPk,
      timestamp,
      shardId
    )

  private def toPublic(privKey: String) = {
    val privateKey = PrivateKey(Base16.unsafeDecode(privKey))
    Secp256k1.toPublic(privateKey)
  }
}
