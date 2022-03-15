package coop.rchain.casper.batch1

import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode.Effect
import coop.rchain.casper.protocol.CommEvent
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.util.rholang.Resources
import coop.rchain.casper.{ReportStore, ReportingCasper}
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rspace.ReportingRspace.ReportingComm
import coop.rchain.shared.scalatestcontrib.effectTest
import coop.rchain.store.InMemoryStoreManager
import coop.rchain.rspace.syntax._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class MultiParentCasperReportingSpec extends FlatSpec with Matchers with Inspectors {

  import coop.rchain.casper.util.GenesisBuilder._

  implicit val timeEff: LogicalTime[Effect] = new LogicalTime[Effect]

  val genesis: GenesisContext = buildGenesis()

  "ReportingCasper" should "behave the same way as MultiParentCasper" in effectTest {
    val correctRholang =
      """ for(@a <- @"1"){ Nil } | @"1"!("x") """
    TestNode.standaloneEff(genesis).use { node =>
      import node._
      import coop.rchain.rholang.interpreter.storage._

      for {
        kvm             <- Resources.mkTestRNodeStoreManager[Effect](node.dataDir)
        rspaceStore     <- kvm.rSpaceStores
        reportingCasper = ReportingCasper.rhoReporter[Effect](rspaceStore)
        deploy = ConstructDeploy
          .sourceDeployNow(correctRholang, shardId = this.genesis.genesisBlock.shardId)
        signedBlock <- node.addBlock(deploy)
        _           = logEff.warns.isEmpty should be(true)
        trace       <- reportingCasper.trace(signedBlock)
        // only the comm events should be equal
        // it is possible that there are additional produce or consume in persistent mode
        reportingCommEventsNum = trace.deployReportResult.head.processedDeploy.deployLog.collect {
          case CommEvent(_, _, _) => 1
        }.sum
        deployCommEventsNum = signedBlock.body.deploys.head.deployLog.count {
          case CommEvent(_, _, _) => true
          case _                  => false
        }
        reportingReplayPostStateHash = trace.postStateHash
        _                            = reportingReplayPostStateHash shouldBe signedBlock.body.state.postStateHash
        _                            = deployCommEventsNum shouldBe reportingCommEventsNum
      } yield ()
    }
  }
}
