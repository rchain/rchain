package coop.rchain.casper.batch1

import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode.Effect
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.{ReportMemStore, ReportingCasper}
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.scalatestcontrib.effectTest
import coop.rchain.store.InMemoryStoreManager
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class MultiParentCasperReportingSpec extends FlatSpec with Matchers with Inspectors {

  import coop.rchain.casper.util.GenesisBuilder._

  implicit val timeEff = new LogicalTime[Effect]

  val genesis = buildGenesis()

  "ReportingCasper" should "behave the same way as MultiParentCasper" ignore effectTest {
    val correctRholang =
      """ for(@a <- @"1"){ Nil } | @"1"!("x") """
    TestNode.standaloneEff(genesis).use { node =>
      import node._
      import coop.rchain.rholang.interpreter.storage._
      implicit val timeEff = new LogicalTime[Effect]
      implicit val kvm     = InMemoryStoreManager[Effect]

      for {
        reportingStore <- ReportMemStore
                           .store[Effect, Par, BindPattern, ListParWithRandom, TaggedContinuation](
                             kvm
                           )
        reportingCasper = ReportingCasper.rhoReporter(node.rhoHistoryRepository, reportingStore)
        deploy          <- ConstructDeploy.sourceDeployNowF(correctRholang)
        signedBlock     <- node.addBlock(deploy)
        _               = logEff.warns.isEmpty should be(true)
        dag             <- node.casperEff.blockDag
        estimate        <- node.casperEff.estimator(dag)
        _               = estimate shouldBe IndexedSeq(signedBlock.blockHash)
        trace           <- reportingCasper.trace(signedBlock.blockHash)
        result = trace match {
          case Right(value) => value.head._2.foldLeft(0)(_ + _.length)
          case Left(_)      => 0
        }
        _ = signedBlock.body.deploys.head.deployLog.size shouldBe (result)
      } yield ()
    }
  }
}
