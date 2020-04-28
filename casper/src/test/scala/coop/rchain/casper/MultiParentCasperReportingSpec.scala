package coop.rchain.casper

import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode.Effect
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.models.BlockHash._
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.scalatestcontrib.effectTest
import org.scalatest.{FlatSpec, Inspectors, Matchers}
import monix.execution.Scheduler.Implicits.global

class MultiParentCasperReportingSpec extends FlatSpec with Matchers with Inspectors {

  import coop.rchain.casper.util.GenesisBuilder._

  implicit val timeEff = new LogicalTime[Effect]

  val genesis = buildGenesis()

  "ReportingCasper" should "behave the same way as MultiParentCasper" ignore effectTest {
    val correctRholang =
      """ for(@a <- @"1"){ Nil } | @"1"!("x") """
    TestNode.standaloneEff(genesis).use { node =>
      import node._
      implicit val timeEff = new LogicalTime[Effect]

      val reportingCasper: ReportingCasper[Effect] =
        ReportingCasper.rhoReporter(node.rhoHistoryRepository)

      for {
        deploy      <- ConstructDeploy.sourceDeployNowF(correctRholang)
        signedBlock <- node.addBlock(deploy)
        _           = logEff.warns.isEmpty should be(true)
        dag         <- node.casperEff.blockDag
        estimate    <- node.casperEff.estimator(dag)
        _           = estimate shouldBe IndexedSeq(signedBlock.blockHash)
        trace       <- reportingCasper.trace(signedBlock.blockHash)
        result = trace match {
          case Right(value) => value.head._2.foldLeft(0)(_ + _.length)
          case Left(_)      => 0
        }
        _ = signedBlock.body.deploys.head.deployLog.size shouldBe (result)
      } yield ()
    }
  }
}
