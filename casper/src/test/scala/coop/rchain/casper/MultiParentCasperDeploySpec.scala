package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.scalatestcontrib._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class MultiParentCasperDeploySpec extends FlatSpec with Matchers with Inspectors {

  import coop.rchain.casper.util.GenesisBuilder._

  implicit val timeEff = new LogicalTime[Effect]

  val genesis = buildGenesis()

  "MultiParentCasper" should "accept a deploy and return it's id" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      import node._
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deploy   <- ConstructDeploy.basicDeployData[Effect](0)
        res      <- MultiParentCasper[Effect].deploy(deploy)
        deployId = res.right.get
      } yield deployId shouldBe deploy.sig
    }
  }

  it should "not create a block with a repeated deploy" in effectTest {
    implicit val timeEff = new LogicalTime[Effect]
    TestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      val List(node0, node1) = nodes.toList
      for {
        deploy             <- ConstructDeploy.basicDeployData[Effect](0)
        _                  <- node0.propagateBlock(deploy)(node1)
        casper1            = node1.casperEff
        _                  <- casper1.deploy(deploy)
        createBlockResult2 <- casper1.createBlock
      } yield (createBlockResult2 should be(NoNewDeploys))
    }
  }

  it should "fail when deploying with insufficient phlos" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deployData <- ConstructDeploy.sourceDeployNowF[Effect]("Nil", phloLimit = 1)
        block      <- node.createBlock(deployData)
      } yield assert(block.body.deploys.head.isFailed)
    }
  }

  it should "succeed if given enough phlos for deploy" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deployData <- ConstructDeploy.sourceDeployNowF[Effect]("Nil", phloLimit = 100)
        block      <- node.createBlock(deployData)
      } yield assert(!block.body.deploys.head.isFailed)
    }
  }

  val term =
    """
      |new return, rl(`rho:registry:lookup`), RevVaultCh, vaultCh,
      |  deployId(`rho:rchain:deployId`)
      |   in {
      |    rl!(`rho:rchain:revVault`, *RevVaultCh) |
      |    for (@(_, RevVault) <- RevVaultCh) {
      |      @RevVault!("findOrCreate", "1111LZ6wp2EkfiFhbMqVZpjVEGaxGYB8rt6uLgdTBUKBcGzVMo1jk", *vaultCh) |
      |      for (@maybeVault <- vaultCh) {
      |        match maybeVault {
      |          (true, vault) => @vault!("balance", *deployId)
      |          (false, err)  => deployId!(err)
      |        }
      |      }
      |    }
      |  }
      |""".stripMargin

  val termNzpr =
    """
      |new
      |    rl(`rho:registry:lookup`), RevVaultCh,
      |    vaultCh, balanceCh,
      |    deployId(`rho:rchain:deployId`),
      |    stdout(`rho:io:stdout`)
      |  in {
      |
      |    rl!(`rho:rchain:revVault`, *RevVaultCh) |
      |    for (@(_, RevVault) <- RevVaultCh) {
      |
      |      stdout!(("MyRChainWallet.CheckBalance.rho")) |
      |
      |      match "1111wz8Ga8b6pPnDgKarjMeFBYr1Fsor8CUfpXfaNcD3D2328sbfv" {
      |        revAddress => {
      |
      |          stdout!(("Accessing vault at RevAddress", revAddress)) |
      |
      |          // most RevVault methods return an `Either[String, A] = (false, String) / (true, A)`
      |          @RevVault!("findOrCreate", revAddress, *vaultCh) |
      |          for (@(true, vault) <- vaultCh) {
      |
      |            stdout!("Obtained vault, checking balance") |
      |
      |            @vault!("balance", *balanceCh) |
      |            for (@balance <- balanceCh) {
      |
      |              stdout!(("Balance is", balance)) |
      |
      |              deployId!(balance)
      |            }
      |          }
      |
      |        }
      |      }
      |    }
      |  }
      |""".stripMargin

  it should "replay the block with hundreds of deploys" in effectTest {
    implicit val timeEff = new LogicalTime[Effect]
    TestNode.networkEff(genesis, networkSize = 10).use {
      case n0 +: n1 +: n2 +: n3 +: n4 +: n5 +: n6 +: n7 +: n8 +: n9 +: Seq() =>
        val deploys = Seq.fill(100)(
          ConstructDeploy
            .sourceDeploy(termNzpr, phloLimit = 500000L, timestamp = System.currentTimeMillis)
        )
//        val terms = Seq.fill(100)(termNzpr).mkString(" |\n")
//        println(s"$terms")
//        val deploysBig = Seq.fill(1)(
//          ConstructDeploy
//            .sourceDeploy(terms, phloLimit = 50000000L, timestamp = System.currentTimeMillis)
//        )
        for {
          b   <- n0.propagateBlock(deploys: _*)(n1, n2, n3, n4, n5, n6, n7, n8, n9)
          ok1 <- n1.casperEff.contains(b.blockHash)
          ok2 <- n2.casperEff.contains(b.blockHash)
          ok3 <- n3.casperEff.contains(b.blockHash)
          ok4 <- n4.casperEff.contains(b.blockHash)
          ok5 <- n5.casperEff.contains(b.blockHash)
          ok6 <- n5.casperEff.contains(b.blockHash)
          ok7 <- n5.casperEff.contains(b.blockHash)
          ok8 <- n5.casperEff.contains(b.blockHash)
          ok9 <- n5.casperEff.contains(b.blockHash)
//          _                  <- casper1.deploy(deploy)
//          createBlockResult2 <- casper1.createBlock
        } yield (ok1, ok2, ok3, ok4, ok5, ok6, ok7, ok8, ok9) shouldBe (true, true, true, true, true, true, true, true, true)
    }
  }

  it should "replay the block with hundreds of deploys 2" in effectTest {
    implicit val timeEff = new LogicalTime[Effect]
    TestNode.networkEff(genesis, networkSize = 5).use {
      case n0 +: n1 +: n2 +: n3 +: n4 +: Seq() =>
        val deploys = Seq.fill(1000)(
          ConstructDeploy
            .sourceDeploy(termNzpr, phloLimit = 500000L, timestamp = System.currentTimeMillis)
        )
        for {
          b   <- n0.propagateBlock(deploys: _*)(n1, n2, n3, n4)
//          b <- n0.createBlock(deploys: _*)
//          _ <- n0.syncWith(n1, n2, n3, n4)
//          _   <- n0.syncWith(n4)
          ok1 <- n1.casperEff.contains(b.blockHash)
          ok2 <- n2.casperEff.contains(b.blockHash)
          ok3 <- n3.casperEff.contains(b.blockHash)
          ok4 <- n4.casperEff.contains(b.blockHash)
        } yield (ok1, ok2, ok3, ok4) shouldBe (true, true, true, true)
    }
  }

}
