package coop.rchain.casper

import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.crypto.PrivateKey

import com.google.protobuf.ByteString
import org.scalatest.{FlatSpec, Matchers}

class SignDeploymentSpec extends FlatSpec with Matchers {

  "Deploy data" should "be signed" in {
    val data =
      DeployData(
        deployer = ByteString.EMPTY,
        term = "@1!(1)",
        timestamp = System.currentTimeMillis(),
        sig = ByteString.EMPTY,
        sigAlgorithm = "",
        phloPrice = 1L,
        phloLimit = 90000,
        validAfterBlockNumber = -1L
      )

    val pk = Base16.decode("9a801debae8bb97fe54c99389cafa576c60612503348578125b65ab182ff5850").get
    pk.length shouldBe Ed25519.keyLength

    val signedData = SignDeployment.sign(PrivateKey(pk), data)
    signedData.sigAlgorithm shouldBe Secp256k1.name
    SignDeployment.verify(signedData) shouldBe Some(true)
  }

  "Deploy data" should "be signed the same way" in {
    val term =
    """new
        |  rl(`rho:registry:lookup`), RevVaultCh,
        |  vaultCh, balanceCh,
        |  stdout(`rho:io:stdout`), log
        |in {
        |
        |  rl!(`rho:rchain:revVault`, *RevVaultCh) |
        |  for (@(_, RevVault) <- RevVaultCh) {
        |
        |
        |    // REPLACE THE REV ADDRESS HERE vvv
        |    match "11112224ZVTh5Ch1tZpfrJWFLrZ2VGesucvwFV9f6BmMaot7Meaps2" {
        |      revAddress => {
        |
        |        log!(("Accessing vault at RevAddress", revAddress)) |
        |
        |        // most RevVault methods return an `Either[String, A] = (false, String) \/ (true, A)`
        |        @RevVault!("findOrCreate", revAddress, *vaultCh) |
        |        for (@(true, vault) <- vaultCh) {
        |
        |          log!("Obtained vault, checking balance") |
        |
        |          @vault!("balance", *balanceCh) |
        |          for (@balance <- balanceCh) {
        |
        |            log!("%LOG_MARKER Vault %REV_ADDR balance is ${balance}" %%{"balance":balance})
        |          }
        |        }
        |      }
        |    }
        |
        |  } |
        |
        |  contract log(@data) = {
        |    @"DEMO"!(data) | stdout!(data)
        |  }
        |}
        |""".stripMargin


    val data =
      DeployData(
        deployer = ByteString.EMPTY,
        term = term,
        timestamp = System.currentTimeMillis(),
        sig = ByteString.EMPTY,
        sigAlgorithm = "",
        phloPrice = 1L,
        phloLimit = 90000,
        validAfterBlockNumber = -1L
      )

    val pk = Base16.decode("9a801debae8bb97fe54c99389cafa576c60612503348578125b65ab182ff5850").get
    pk.length shouldBe Ed25519.keyLength

    val signedData = SignDeployment.sign(PrivateKey(pk), data)
    signedData.sigAlgorithm shouldBe Secp256k1.name
    SignDeployment.verify(signedData) shouldBe Some(true)
  }

}
