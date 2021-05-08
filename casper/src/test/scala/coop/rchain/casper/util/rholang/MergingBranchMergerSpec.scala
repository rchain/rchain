package coop.rchain.casper.util.rholang

import cats.effect.{Concurrent, Resource}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.blocks.merger.{BlockIndex, CasperDagMerger, Indexer, MergingVertex}
import coop.rchain.casper.protocol.{ProcessedDeploy, ProcessedSystemDeploy}
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang.costacc.CloseBlockDeploy
import coop.rchain.casper.util.{ConstructDeploy, EventConverter, GenesisBuilder}
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.dag.{DagReader, InMemDAG}
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.metrics.NoopSpan
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.crypto.Blake2b256Hash
import coop.rchain.rspace.merger.EventChain
import coop.rchain.rspace.merger.instances.EventsIndexConflictDetectors
import coop.rchain.shared.scalatestcontrib.effectTest
import coop.rchain.shared.{Log, Time}
import coop.rchain.store.LazyKeyValueCache
import fs2.Stream
import kamon.trace.Span.Metrics
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.Seq

class MergingBranchMergerSpec extends FlatSpec with Matchers {

  val genesisContext             = GenesisBuilder.buildGenesis(validatorsNum = 10)
  val genesis                    = genesisContext.genesisBlock
  implicit val logEff            = Log.log[Task]
  implicit val timeF: Time[Task] = new LogicalTime[Task]

  val runtimeManagerResource: Resource[Task, RuntimeManager[Task]] = for {
    dirs <- Resources.copyStorage[Task](genesisContext.storageDirectory)
    kvm  <- Resource.liftF(Resources.mkTestRNodeStoreManager[Task](dirs.storageDir))
    rm   <- Resource.liftF(Resources.mkRuntimeManagerAt[Task](kvm))
  } yield rm

  def makeTxAndCommitState(
      runtimeManager: RuntimeManager[Task],
      baseState: StateHash,
      payerKey: PrivateKey,
      validator: PublicKey,
      seqNum: Int = 0,
      blockNum: Long = 0
  ): Task[(StateHash, Seq[ProcessedDeploy], Seq[ProcessedSystemDeploy])] =
    for {
      txDeploy    <- ConstructDeploy.sourceDeployNowF("Nil", sec = payerKey)
      userDeploys = txDeploy :: Nil
      systemDeploys = CloseBlockDeploy(
        SystemDeployUtil
          .generateCloseDeployRandomSeed(
            validator,
            seqNum
          )
      ) :: Nil
      blockData = BlockData(
        txDeploy.data.timestamp,
        blockNum,
        validator,
        seqNum
      )
      invalidBlocks = Map.empty[BlockHash, Validator]
      r <- runtimeManager.computeState(baseState)(
            userDeploys,
            systemDeploys,
            blockData,
            invalidBlocks
          )
    } yield r

  def mkStatesSteams(preStateHash: StateHash, seqNum: Int, blockNum: Long)(
      implicit runtimeManager: RuntimeManager[Task]
  ) =
    (genesisContext.validatorPks zip genesisContext.genesisVaultsSks).map {
      case (validatorPk, payerSk) =>
        Stream
          .eval(
            makeTxAndCommitState(
              runtimeManager,
              preStateHash,
              payerSk,
              validatorPk,
              seqNum,
              blockNum
            )
          )
    }.toList

  def mkState(stateHash: StateHash, seqNum: Int, blockNum: Long, n: Int)(
      implicit runtimeManager: RuntimeManager[Task]
  ) =
    mkStatesSteams(stateHash, seqNum, blockNum).get(n.toLong).get.compile.lastOrError

  def mkHeadState(stateHash: StateHash, seqNum: Int, blockNum: Long)(
      implicit runtimeManager: RuntimeManager[Task]
  ) =
    mkStatesSteams(stateHash, seqNum, blockNum).head.compile.lastOrError

  def mkTailStates(stateHash: StateHash, seqNum: Int, blockNum: Long)(
      implicit runtimeManager: RuntimeManager[Task]
  ) =
    Stream
      .emits(mkStatesSteams(stateHash, seqNum, blockNum).tail)
      .parJoinUnbounded
      .compile
      .toList

  "serial execution and merging" should "be equal" in effectTest {
    runtimeManagerResource.use { runtimeManager =>
      {
        implicit val rm: RuntimeManager[Task] = runtimeManager
        val genesisPostStateHash              = genesis.body.state.postStateHash

        def tryIt =
          for {
            // create two states one on top of another
            s1 <- mkState(genesisPostStateHash, 1, 1L, 0)
            s2 <- mkState(s1._1, 1, 1L, 1)
            // result of serial execution
            serialHash = s2._1

            mainPostStateHash    = s1._1
            mergingPostStateHash = s2._1
            mergeEvents          = s2._2.flatMap(_.deployLog).map(EventConverter.toRspaceEvent)
            // merge top state to the bottom state
            merger = runtimeManager.getHistoryRepo.stateMerger
            // result of merge execution
            mainHistory <- runtimeManager.getHistoryRepo
                            .reset(Blake2b256Hash.fromByteString(mainPostStateHash))
                            .map(_.history)
            baseHistory <- runtimeManager.getHistoryRepo
                            .reset(Blake2b256Hash.fromByteString(genesisPostStateHash))
                            .map(_.history)
            mergingHistory <- runtimeManager.getHistoryRepo
                               .reset(Blake2b256Hash.fromByteString(mergingPostStateHash))
                               .map(_.history)
            mergeHash <- merger
                          .merge(
                            mainHistory,
                            Seq(
                              EventChain(
                                startState = baseHistory,
                                endState = mergingHistory,
                                events = mergeEvents
                              )
                            )
                          )
                          .map(_.root.toByteString)

            // result hashes should be the same
          } yield (serialHash.toStringUtf8 == mergeHash.toStringUtf8)

        fs2.Stream
          .emits(List.range(0, 10).map(_ => fs2.Stream.eval(tryIt)))
          .parJoinUnbounded
          .compile
          .toList
          .map(_.reduce(_ && _))
          .map(r => assert(r))
      }
    }
  }

  "conflict detection for branches having blocks from the same/different validators" should "be correct" in effectTest {
    import coop.rchain.rspace.syntax._
    runtimeManagerResource.use { runtimeManager =>
      {
        implicit val rm: RuntimeManager[Task] = runtimeManager
        implicit val conc                     = Concurrent[Task]
        val genesisPostStateHash              = genesis.body.state.postStateHash
        val baseStateReader = runtimeManager.getHistoryRepo
          .getHistoryReader(Blake2b256Hash.fromByteString(genesisPostStateHash))

        for {

          // a0 and a1 are states created by the same validator, so event logs should expose conflict
          a0 <- mkState(genesisPostStateHash, 1, 1L, 0)
          a1 <- mkState(genesisPostStateHash, 2, 2L, 0)
          // a0 and b are ceated by different validators, there should be no conflicts
          b <- mkState(genesisPostStateHash, 1, 1L, 1)

          blockIndexCache <- LazyKeyValueCache[Task, MergingVertex, BlockIndex](
                              Indexer.createBlockIndex[Task]
                            )

          a0Index <- {
            implicit val ch = blockIndexCache
            Indexer.createBranchIndex(
              Seq(MergingVertex(postStateHash = a0._1, processedDeploys = a0._2.toSet))
            )
          }
          a1Index <- {
            implicit val ch = blockIndexCache
            Indexer.createBranchIndex(
              Seq(MergingVertex(postStateHash = a1._1, processedDeploys = a1._2.toSet))
            )
          }
          bIndex <- {
            implicit val ch = blockIndexCache
            Indexer.createBranchIndex(
              Seq(MergingVertex(postStateHash = b._1, processedDeploys = b._2.toSet))
            )
          }

          baseDataReader <- LazyKeyValueCache(
                             (ch: Blake2b256Hash) => {
                               implicit val c = runtimeManager.getHistoryRepo
                               baseStateReader
                                 .getDataFromChannelHash(ch)
                             }
                           )
          baseJoinsReader <- LazyKeyValueCache(
                              (ch: Blake2b256Hash) => {
                                implicit val c = runtimeManager.getHistoryRepo
                                baseStateReader
                                  .getJoinsFromChannelHash(ch)
                              }
                            )

          // branches having the same validator blocks should be confliting
          c <- EventsIndexConflictDetectors.findConflicts(
                a0Index.eventLogIndex,
                a1Index.eventLogIndex,
                baseDataReader,
                baseJoinsReader
              )
          // branches having different validator blocks should not conflict
          c1 <- EventsIndexConflictDetectors.findConflicts(
                 a0Index.eventLogIndex,
                 bIndex.eventLogIndex,
                 baseDataReader,
                 baseJoinsReader
               )

        } yield assert(c.nonEmpty && c1.isEmpty)
      }
    }
  }

  type DagTemplate = Array[List[Int]]
  def getRejectedBlocksAtTheTop(dag: DagTemplate) =
    runtimeManagerResource.use { runtimeManager =>
      {
        implicit val rm: RuntimeManager[Task] = runtimeManager
        implicit val span                     = NoopSpan[Task]
        implicit val metrics                  = new MetricsNOP[Task]

        val emptyDag             = InMemDAG[Task, MergingVertex](Map.empty, Map.empty)
        val genesisPostStateHash = genesis.body.state.postStateHash

        val genesisLayer =
          Seq(
            MergingVertex(
              ByteString.copyFromUtf8("genesis"),
              genesisPostStateHash,
              ByteString.EMPTY,
              Set.empty
            )
          )

        val blockIndexCache = LazyKeyValueCache[Task, MergingVertex, BlockIndex](
          Indexer.createBlockIndex[Task]
        ).runSyncUnsafe()

        dag.toList
          .foldLeftM[
            Task,
            (Seq[MergingVertex], InMemDAG[Task, MergingVertex], Long, Seq[ProcessedDeploy])
          ](
            (genesisLayer, emptyDag, 1L, Seq.empty)
          ) {
            case ((prevLayer, dag, blockNum, rejectedAcc), levelTemplate) =>
              println(s"creating lvl ${blockNum}, template: ${levelTemplate}")
              for {
                v <- if (prevLayer.size == 1) (prevLayer.head.postStateHash, Seq.empty).pure[Task]
                    else CasperDagMerger.merge(prevLayer, genesisLayer.head, dag, blockIndexCache)
                (preStateHash, rejectedAtLevel) = v
                newLayer <- fs2.Stream
                             .emits(
                               levelTemplate.map(
                                 n =>
                                   fs2.Stream.eval(
                                     for {
                                       s <- mkState(preStateHash, blockNum.toInt, blockNum, n)
                                     } yield MergingVertex(
                                       postStateHash = s._1,
                                       preStateHash = preStateHash,
                                       processedDeploys = s._2.toSet
                                     )
                                   )
                               )
                             )
                             .parJoinUnbounded
                             .compile
                             .toList
                newConnections = newLayer.flatMap(newV => prevLayer.map((_, newV)))
                newDag <- newConnections.foldLeftM(dag)(
                           (acc, pair) => acc.addEdge(pair._2, pair._1)
                         )
              } yield (newLayer, newDag, blockNum + 1, rejectedAcc ++ rejectedAtLevel)
          }
          .map(_._4)
      }
    }

  "stuck last finalized block should" should "be ok" in effectTest {
    //
    val graph =
      s"""
         |*
         | ***  -> when creating these blocks, error thrown
         |*
         | ***
         |*
         | ***
         |*
         | ***
         |*
         | """.stripMargin

    val layers = graph.split('\n')
    val template = layers
      .map(_.toList.zipWithIndex.filter((c) => c._1 == '*').map(_._2))
      .filter(_.nonEmpty)
      .reverse
    getRejectedBlocksAtTheTop(template)
  }

  "multiple 1 block branches" should "be merged" in effectTest {
    runtimeManagerResource.use { runtimeManager =>
      {
        implicit val rm: RuntimeManager[Task] = runtimeManager
        implicit val span                     = NoopSpan[Task]
        implicit val metrics                  = new MetricsNOP[Task]
        val emptyDag                          = InMemDAG[Task, MergingVertex](Map.empty, Map.empty)
        val genesisPostStateHash              = genesis.body.state.postStateHash

        val blockIndexCache = LazyKeyValueCache[Task, MergingVertex, BlockIndex](
          Indexer.createBlockIndex[Task]
        ).runSyncUnsafe()

        // simulates 0.99 sync threshold - first validator create state, then all other create on top of that/
        // merge state of resulting blocks returned
        def mkLayer(startPreStateHash: StateHash, n: Int): Task[StateHash] =
          for {
            // create state on first validator
            b <- mkHeadState(startPreStateHash, n * 2 + 1, (n * 2 + 1).toLong)
            base = MergingVertex(
              postStateHash = b._1,
              preStateHash = startPreStateHash,
              processedDeploys = b._2.toSet
            )
            _ = println(s"building on ${PrettyPrinter.buildString(startPreStateHash)}")

            // create children an all other
            baseChildren <- mkTailStates(base.postStateHash, n * 2 + 2, (n * 2 + 2).toLong)
            mergingTips = baseChildren.map(
              c =>
                MergingVertex(
                  postStateHash = c._1,
                  preStateHash = startPreStateHash,
                  processedDeploys = c._2.toSet
                )
            )
            dag <- mergingTips.foldLeftM(emptyDag)((acc, tip) => acc.addEdge(tip, base))

            // merge children to get next preStateHash
            v                                   <- CasperDagMerger.merge(mergingTips, base, dag, blockIndexCache)
            (nextPreStateHash, rejectedDeploys) = v
            _                                   = assert(rejectedDeploys.size == 0)
            _                                   = println(s"merge result ${PrettyPrinter.buildString(nextPreStateHash)}")
          } yield nextPreStateHash

        for {
          // create base state - child of genesis
          _ <- List.range(0, 4).foldLeftM(genesisPostStateHash) { (baseHash, i) =>
                mkLayer(baseHash, i)
              }
        } yield ()
      }
    }
  }
}
