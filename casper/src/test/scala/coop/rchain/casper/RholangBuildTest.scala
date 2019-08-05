package coop.rchain.casper

import cats.implicits._
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.genesis.contracts.Vault
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode._
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.util.GenesisBuilder._
import coop.rchain.casper.util.RSpaceUtil._
import coop.rchain.casper.util.rholang.RegistrySigGen
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.rholang.interpreter.util.RevAddress
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}
import scala.util.Random

class RholangBuildTest extends FlatSpec with Matchers {

  "Our build system" should "allow import of rholang sources into scala code" in effectTest {
    HashSetCasperTestNode
      .standaloneEff(buildGenesis())
      .use { node =>
        import node._

        val code =
          """
          |new testRet, double, rl(`rho:registry:lookup`), ListOpsCh, getBlockData(`rho:block:data`),
          |    timeRtn, doubleRet
          |in {
          |  contract double(@x, ret) = { ret!(2 * x) } |
          |  rl!(`rho:lang:listOps`, *ListOpsCh) |
          |  for(@(_, ListOps) <- ListOpsCh) {
          |    @ListOps!("map", [2, 3, 5, 7], *double, *doubleRet)
          |  } |
          |  getBlockData!(*timeRtn) |
          |  for (@_, @timestamp <- timeRtn; @doubles <- doubleRet) {
          |    testRet!((doubles, "The timestamp is ${timestamp}" %% {"timestamp" : timestamp}))
          |  }
          |}
          |""".stripMargin
        for {
          deploy <- ConstructDeploy.sourceDeployNowF(code)
          createBlockResult <- MultiParentCasper[Effect]
                                .deploy(deploy) >> MultiParentCasper[Effect].createBlock
          Created(signedBlock) = createBlockResult
          _                    <- MultiParentCasper[Effect].addBlock(signedBlock, ignoreDoppelgangerCheck[Effect])
          _                    = logEff.warns should be(Nil)
          _ <- getDataAtPrivateChannel[Effect](
                signedBlock,
                Base16.encode(
                  RegistrySigGen.generateUnforgeableNameId(
                    PublicKey(deploy.deployer.toByteArray),
                    deploy.timestamp
                  )
                )
              ).map(_ shouldBe Seq("""([4, 6, 10, 14], "The timestamp is 2")"""))
        } yield ()
      }
  }

  "Our build system" should "execute the genesis block" ignore effectTest {
    val REV_ADDRESS_COUNT = 16000

    val vaults = (1 to REV_ADDRESS_COUNT)
      .map(i => (Secp256k1.newKeyPair, i))
      .map { case ((_, publicKey), i) => Vault(RevAddress.fromPublicKey(publicKey).get, i.toLong) }

    val vault               = vaults(Random.nextInt(REV_ADDRESS_COUNT))
    val (keyPairs, genesis) = buildGenesisParameters()
    val genesisParams       = (keyPairs, genesis.copy(vaults = vaults))

    HashSetCasperTestNode.standaloneEff(buildGenesis(genesisParams)).use { node =>
      import node._

      val balanceCheckSource: String =
        s"""  new testRet, RevVaultCh, vaultCh, rl(`rho:registry:lookup`) in {
            #   rl!(`rho:rchain:revVault`, *RevVaultCh) |
            #   for (@(_, RevVault) <- RevVaultCh) {
            #     @RevVault!("findOrCreate", "${vault.revAddress.toBase58}", *vaultCh) |
            #     for(@(true, vault) <- vaultCh){
            #        @vault!("balance", *testRet)
            #     }
            #   }
            # }
            # """.stripMargin('#')

      for {
        balanceCheckDeploy <- ConstructDeploy.sourceDeployNowF(balanceCheckSource)
        createBlockResult <- MultiParentCasper[Effect]
                              .deploy(balanceCheckDeploy) >> MultiParentCasper[Effect].createBlock
        Created(signedBlock) = createBlockResult
        _                    <- MultiParentCasper[Effect].addBlock(signedBlock, ignoreDoppelgangerCheck[Effect])
        _                    = logEff.warns should be(Nil)
        _ <- getDataAtPrivateChannel[Effect](
              signedBlock,
              Base16.encode(
                RegistrySigGen.generateUnforgeableNameId(
                  PublicKey(balanceCheckDeploy.deployer.toByteArray),
                  balanceCheckDeploy.timestamp
                )
              )
            ).map(_ shouldBe Seq(s"${vault.initialBalance}"))
      } yield ()
    }
  }
}
