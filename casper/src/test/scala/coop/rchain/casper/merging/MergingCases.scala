package coop.rchain.casper.merging

import cats.effect.Resource
import cats.syntax.all._
import coop.rchain.casper.BlockRandomSeed
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.rholang.sysdeploys.CloseBlockDeploy
import coop.rchain.casper.rholang.{Resources, RuntimeManager}
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.{ConstructDeploy, GenesisBuilder}
import coop.rchain.models.syntax.modelsSyntaxByteString
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rspace.merger.EventLogMergingLogic.computeRelatedSets
import coop.rchain.rspace.merger.{EventLogIndex, EventLogMergingLogic}
import coop.rchain.shared.scalatestcontrib.effectTest
import coop.rchain.shared.{Log, Time}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MergingCases extends AnyFlatSpec with Matchers {

  val genesisContext             = GenesisBuilder.buildGenesis(validatorsNum = 5)
  val genesis                    = genesisContext.genesisBlock
  implicit val logEff            = Log.log[Task]
  implicit val timeF: Time[Task] = new LogicalTime[Task]

  val runtimeManagerResource: Resource[Task, RuntimeManager[Task]] = for {
    dir <- Resources.copyStorage[Task](genesisContext.storageDirectory)
    kvm <- Resource.eval(Resources.mkTestRNodeStoreManager[Task](dir))
    mergeableTag = BlockRandomSeed.nonNegativeMergeableTagName(
      genesis.shardId
    )
    rm <- Resource.eval(Resources.mkRuntimeManagerAt[Task](kvm, mergeableTag))
  } yield rm

  /**
    * Two deploys inside single state transition are using the same PVV for precharge and refund.
    * So this should be dependent over produce that puts new value into PVV balance in the first deploy.
    * TODO adjust this once/if there is a solution to make deploys touching the same PVV non dependent
    */
  "Two deploys executed inside single state transition" should "be dependent" in effectTest {
    runtimeManagerResource.use { runtimeManager =>
      {
        val baseState              = genesis.postStateHash
        val payer1Key              = genesisContext.genesisVaults.head._1
        val payer2Key              = genesisContext.genesisVaults.tail.head._1
        val stateTransitionCreator = genesisContext.validatorKeyPairs.head._2
        val seqNum                 = 1L
        val blockNum               = 1L

        for {
          d1          <- ConstructDeploy.sourceDeployNowF("Nil", sec = payer1Key)
          d2          <- ConstructDeploy.sourceDeployNowF("Nil", sec = payer2Key)
          userDeploys = Seq(d1, d2)
          blockData = BlockData(
            blockNum,
            stateTransitionCreator,
            seqNum
          )
          rand                                 = BlockRandomSeed.fromBlock(genesis)
          clodeBlockDeployIndex                = 3
          systemDeploys                        = CloseBlockDeploy(rand.splitByte(clodeBlockDeployIndex.toByte)) :: Nil
          r                                    <- runtimeManager.computeState(baseState)(userDeploys, systemDeploys, rand, blockData)
          (postStateHash, processedDeploys, _) = r
          _                                    = processedDeploys.size shouldBe 2

          blkSender    = stateTransitionCreator.bytes
          mergeableChs <- runtimeManager.loadMergeableChannels(postStateHash, blkSender, seqNum)

          // Combine processed deploys with cached mergeable channels data
          processedDeploysWithMergeable = processedDeploys.toVector.zip(mergeableChs)

          idxs <- processedDeploysWithMergeable.traverse {
                   case (d, mergeChs) =>
                     BlockIndex.createEventLogIndex(
                       d.deployLog,
                       runtimeManager.getHistoryRepo,
                       baseState.toBlake2b256Hash,
                       mergeChs
                     )
                 }
          firstDepends  = EventLogMergingLogic.depends(idxs.head, idxs(1))
          secondDepends = EventLogMergingLogic.depends(idxs(1), idxs.head)
          conflicts     = EventLogMergingLogic.areConflicting(idxs.head, idxs(1))
          deployChains = computeRelatedSets[EventLogIndex](
            idxs.toSet,
            EventLogMergingLogic.depends
          )
          // deploys inside one state transition never conflict, as executed in a sequence (for now)
          _ = conflicts shouldBe false
          // first deploy does not depend on the second
          _ = firstDepends shouldBe false
          // second deploy depends on the first, as it consumes produce put by first one when updating per validator vault balance
          _ = secondDepends shouldBe false
          // deploys should be be put in separate deploy chains
          _ = deployChains.size shouldBe 2
        } yield ()
      }
    }
  }
}
