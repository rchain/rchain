package coop.rchain.casper

import cats.implicits._
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.genesis.contracts.Vault
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.shared.scalatestcontrib._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.util.GenesisBuilder._
import coop.rchain.casper.util.RSpaceUtil._
import coop.rchain.casper.util.rholang.RegistrySigGen
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.shared.RChainScheduler
import org.scalatest.{FlatSpec, Matchers}

class RholangBuildTest extends FlatSpec with Matchers {

  implicit val scheduler = RChainScheduler.interpreterScheduler
  val genesis            = buildGenesis()

  "Our build system" should "allow import of rholang sources into scala code" in effectTest {
    TestNode
      .standaloneEff(genesis)
      .use { node =>
        import node._

        val code =
          """
          |new testRet, double, rl(`rho:registry:lookup`), ListOpsCh, getBlockData(`rho:block:data`),
          |    timeRtn, stdout(`rho:io:stdout`), doubleRet
          |in {
          |  contract double(@x, ret) = { ret!(2 * x) } |
          |  rl!(`rho:lang:listOps`, *ListOpsCh) |
          |  for(@(_, ListOps) <- ListOpsCh) {
          |    @ListOps!("map", [2, 3, 5, 7], *double, *doubleRet)
          |  } |
          |  getBlockData!(*timeRtn) |
          |  for (@_, @timestamp, @_ <- timeRtn; @doubles <- doubleRet) {
          |    testRet!((doubles, "The timestamp is ${timestamp}" %% {"timestamp" : timestamp}))
          |  }
          |}
          |""".stripMargin
        for {
          deploy      <- ConstructDeploy.sourceDeployNowF(code)
          signedBlock <- node.addBlock(deploy)
          _           = logEff.warns should be(Nil)
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
      .toSeq
    val (keyPairs, genesis) = buildGenesisParameters()
    val genesisParams       = (keyPairs, genesis.copy(vaults = vaults))
    TestNode
      .standaloneEff(buildGenesis(genesisParams))
      .use { node =>
        import node._
        (logEff.warns should be(Nil)).pure[Effect]
      }
  }
}
