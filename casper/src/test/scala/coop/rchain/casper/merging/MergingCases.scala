package coop.rchain.casper.merging

import cats.syntax.all._
import cats.effect.Resource
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.rholang.costacc.CloseBlockDeploy
import coop.rchain.casper.util.{ConstructDeploy, GenesisBuilder}
import coop.rchain.casper.util.rholang.{Resources, RuntimeManager, SystemDeployUtil}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.syntax.modelsSyntaxByteString
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rspace.merger.{EventLogIndex, MergingLogic}
import coop.rchain.rspace.merger.MergingLogic.computeRelatedSets
import coop.rchain.shared.{Log, Time}
import coop.rchain.shared.syntax._
import coop.rchain.shared.scalatestcontrib.effectTest
import monix.eval.Task
import org.scalatest.{FlatSpec, Matchers}
import monix.execution.Scheduler.Implicits.global

class MergingCases extends FlatSpec with Matchers {

  val genesisContext             = GenesisBuilder.buildGenesis(validatorsNum = 5)
  val genesis                    = genesisContext.genesisBlock
  private val SHARD_ID           = genesis.shardId
  implicit val logEff            = Log.log[Task]
  implicit val timeF: Time[Task] = new LogicalTime[Task]

  val runtimeManagerResource: Resource[Task, RuntimeManager[Task]] = for {
    dir <- Resources.copyStorage[Task](genesisContext.storageDirectory)
    kvm <- Resource.eval(Resources.mkTestRNodeStoreManager[Task](dir))
    rm  <- Resource.eval(Resources.mkRuntimeManagerAt[Task](kvm, shardId = SHARD_ID))
  } yield rm

  /**
    * Two deploys inside single state transition are using the same PVV for precharge and refund.
    * So this should be dependent over produce that puts new value into PVV balance in the first deploy.
    * TODO adjust this once/if there is a solution to make deploys touching the same PVV non dependent
    */
  "Two deploys executed inside single state transition" should "be dependent" in effectTest {
    runtimeManagerResource.use { runtimeManager =>
      {
        val baseState              = genesis.body.state.postStateHash
        val payer1Key              = genesisContext.genesisVaults.head._1
        val payer2Key              = genesisContext.genesisVaults.tail.head._1
        val stateTransitionCreator = genesisContext.validatorKeyPairs.head._2
        val seqNum                 = 1
        val blockNum               = 1L

        for {
          d1          <- ConstructDeploy.sourceDeployNowF("Nil", sec = payer1Key, shardId = SHARD_ID)
          d2          <- ConstructDeploy.sourceDeployNowF("Nil", sec = payer2Key, shardId = SHARD_ID)
          userDeploys = Seq(d1, d2)
          systemDeploys = CloseBlockDeploy(
            SystemDeployUtil
              .generateCloseDeployRandomSeed(
                stateTransitionCreator,
                seqNum
              )
          ) :: Nil
          blockData = BlockData(
            d1.data.timestamp,
            blockNum,
            stateTransitionCreator,
            seqNum
          )
          invalidBlocks = Map.empty[BlockHash, Validator]
          r <- runtimeManager.computeState(baseState)(
                userDeploys,
                systemDeploys,
                blockData,
                invalidBlocks
              )
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
          firstDepends  = MergingLogic.depends(idxs.head, idxs(1))
          secondDepends = MergingLogic.depends(idxs(1), idxs.head)
          conflicts     = MergingLogic.areConflicting(idxs.head, idxs(1))
          deployChains = computeRelatedSets[EventLogIndex](
            idxs.toSet,
            MergingLogic.depends
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
