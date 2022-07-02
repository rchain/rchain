package coop.rchain.node.mergeablity

import cats.effect.concurrent.Deferred
import cats.effect.{Concurrent, Resource}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper.dag.BlockDagKeyValueStorage
import coop.rchain.casper.merging.{BlockIndex, DagMerger, DeployChainIndex}
import coop.rchain.casper.protocol.{BlockMessage, ProcessedDeploy, ProcessedSystemDeploy}
import coop.rchain.casper.rholang.RuntimeManager.StateHash
import coop.rchain.casper.rholang.sysdeploys.CloseBlockDeploy
import coop.rchain.casper.rholang.{Resources, RuntimeManager, SystemDeployUtil}
import coop.rchain.casper.syntax.casperSyntaxRuntimeManager
import coop.rchain.casper.util.{ConstructDeploy, GenesisBuilder}
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator
import coop.rchain.models.blockImplicits.getRandomBlock
import coop.rchain.models.syntax._
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.scalatestcontrib.effectTest
import coop.rchain.shared.{Log, Time}
import coop.rchain.store.InMemoryStoreManager
import fs2.Stream
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MergingBranchMergerSpec extends AnyFlatSpec with Matchers {

  val genesisContext             = GenesisBuilder.buildGenesis(validatorsNum = 5)
  val genesis                    = genesisContext.genesisBlock
  implicit val logEff            = Log.log[Task]
  implicit val timeF: Time[Task] = new LogicalTime[Task]

  val runtimeManagerResource: Resource[Task, RuntimeManager[Task]] = for {
    dir <- Resources.copyStorage[Task](genesisContext.storageDirectory)
    kvm <- Resource.eval(Resources.mkTestRNodeStoreManager[Task](dir))
    rm  <- Resource.eval(Resources.mkRuntimeManagerAt[Task](kvm))
  } yield rm

  def txRho(payer: String, payee: String) =
    s"""
       |new
       |  rl(`rho:registry:lookup`), stdout(`rho:io:stdout`),  revVaultCh, log
       |in {
       |  rl!(`rho:rchain:revVault`, *revVaultCh) |
       |  for (@(_, revVault) <- revVaultCh) {
       |    match ("${payer}", "${payee}", 50) {
       |      (from, to, amount) => {
       |        new vaultCh, revVaultKeyCh, deployerId(`rho:rchain:deployerId`) in {
       |          @revVault!("findOrCreate", from, *vaultCh) |
       |          @revVault!("deployerAuthKey", *deployerId, *revVaultKeyCh) |
       |          for (@(true, vault) <- vaultCh; key <- revVaultKeyCh) {
       |            new resultCh in {
       |              stdout!("TX from ${payer} to ${payee} succeed.")|
       |              @vault!("transfer", to, amount, *key, *resultCh)
       |            }
       |          }
       |        }
       |      }
       |    }
       |  }
       |}""".stripMargin

  def makeTxAndCommitState(
      runtimeManager: RuntimeManager[Task],
      baseState: StateHash,
      payerKey: PrivateKey,
      validator: PublicKey,
      seqNum: Long = 0L,
      blockNum: Long = 0L
  ): Task[(StateHash, Seq[ProcessedDeploy], Seq[ProcessedSystemDeploy])] = {
    val payerAddr = RevAddress.fromPublicKey(Secp256k1.toPublic(payerKey)).get.address.toBase58
    // random address for recipient
    val payeeAddr =
      RevAddress
        .fromDeployerId(Array.fill(Validator.Length)((scala.util.Random.nextInt(256) - 128).toByte))
        .get
        .address
        .toBase58
    for {
      txDeploy    <- ConstructDeploy.sourceDeployNowF(txRho(payerAddr, payeeAddr), sec = payerKey)
      userDeploys = txDeploy :: Nil
      systemDeploys = CloseBlockDeploy(
        SystemDeployUtil.generateCloseDeployRandomSeed(validator, seqNum)
      ) :: Nil
      blockData = BlockData(blockNum, validator, seqNum)
      r         <- runtimeManager.computeState(baseState)(userDeploys, systemDeploys, blockData)
    } yield r
  }

  def mkBlocksSteams(preStateHash: StateHash, seqNum: Long, blockNum: Long)(
      implicit runtimeManager: RuntimeManager[Task]
  ) = {
    val pksWithSingleConflict = {
      val a = genesisContext.genesisVaultsSks.last
      genesisContext.genesisVaultsSks.dropRight(2) ++ List(a, a)
    }
    (genesisContext.validatorPks zip pksWithSingleConflict).map {
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
          .map { s =>
            getRandomBlock(
              setPreStateHash = preStateHash.some,
              setPostStateHash = s._1.some,
              setDeploys = s._2.some,
              setSysDeploys = s._3.some,
              setBlockNumber = blockNum.some,
              setSeqNumber = seqNum.some,
              setValidator = ByteString.copyFrom(validatorPk.bytes).some,
              setBonds = genesisContext.validatorKeyPairs
                .map(_._2)
                .map(pk => (ByteString.copyFrom(pk.bytes), 1L))
                .toMap
                .some
            )
          }
    }.toList
  }

  def mkBlocks(stateHash: StateHash, seqNum: Long, blockNum: Long, n: Int)(
      implicit runtimeManager: RuntimeManager[Task]
  ) =
    mkBlocksSteams(stateHash, seqNum, blockNum).get(n.toLong).get.compile.lastOrError

  def mkHeadBlock(stateHash: StateHash, seqNum: Long, blockNum: Long)(
      implicit runtimeManager: RuntimeManager[Task]
  ) =
    mkBlocksSteams(stateHash, seqNum, blockNum).head.compile.lastOrError

  def mkTailBlocks(stateHash: StateHash, seqNum: Long, blockNum: Long)(
      implicit runtimeManager: RuntimeManager[Task]
  ) =
    Stream
      .emits(mkBlocksSteams(stateHash, seqNum, blockNum).tail)
      .parJoinUnbounded
      .compile
      .toList

  //  "serial execution and merging" should "be equal" in effectTest {
  //    runtimeManagerResource.use { runtimeManager =>
  //      {
  //        implicit val rm: RuntimeManager[Task] = runtimeManager
  //        val genesisPostStateHash              = genesis.postStateHash
  //
  //        def tryIt =
  //          for {
  //            // create two states one on top of another
  //            s1 <- mkState(genesisPostStateHash, 1, 1L, 0)
  //            s2 <- mkState(s1._1, 1, 1L, 1)
  //            // result of serial execution
  //            serialHash = s2._1
  //
  //            mainPostStateHash    = s1._1
  //            mergingPostStateHash = s2._1
  //            mergeEvents          = s2._2.flatMap(_.deployLog).map(EventConverter.toRspaceEvent)
  //            // merge top state to the bottom state
  //            merger = runtimeManager.getHistoryRepo.stateMerger
  //            // result of merge execution
  //            mainHistory <- runtimeManager.getHistoryRepo
  //                            .reset(Blake2b256Hash.fromByteString(mainPostStateHash))
  //                            .map(_.history)
  //            baseHistory <- runtimeManager.getHistoryRepo
  //                            .reset(Blake2b256Hash.fromByteString(genesisPostStateHash))
  //                            .map(_.history)
  //            mergingHistory <- runtimeManager.getHistoryRepo
  //                               .reset(Blake2b256Hash.fromByteString(mergingPostStateHash))
  //                               .map(_.history)
  //            mergeHash <- merger
  //                          .merge(
  //                            mainHistory,
  //                            Seq(
  //                              EventChain(
  //                                startState = baseHistory,
  //                                endState = mergingHistory,
  //                                events = mergeEvents
  //                              )
  //                            )
  //                          )
  //                          .map(_.root.toByteString)
  //
  //            // result hashes should be the same
  //          } yield (serialHash.toStringUtf8 == mergeHash.toStringUtf8)
  //
  //        fs2.Stream
  //          .emits(List.range(0, 10).map(_ => fs2.Stream.eval(tryIt)))
  //          .parJoinUnbounded
  //          .compile
  //          .toList
  //          .map(_.reduce(_ && _))
  //          .map(r => assert(r))
  //      }
  //    }
  //  }

  //  "conflict detection for branches having blocks from the same/different validators" should "be correct" in effectTest {
  //    import coop.rchain.rspace.syntax._
  //    runtimeManagerResource.use { runtimeManager =>
  //      {
  //        implicit val rm: RuntimeManager[Task] = runtimeManager
  //        implicit val conc                     = Concurrent[Task]
  //        val genesisPostStateHash              = genesis.postStateHash
  //        val baseStateReader = runtimeManager.getHistoryRepo
  //          .getHistoryReader(Blake2b256Hash.fromByteString(genesisPostStateHash))
  //
  //        for {
  //
  //          // a0 and a1 are states created by the same validator, so event logs should expose conflict
  //          a0 <- mkState(genesisPostStateHash, 1, 1L, 0)
  //          a1 <- mkState(genesisPostStateHash, 2, 2L, 0)
  //          // a0 and b are ceated by different validators, there should be no conflicts
  //          b <- mkState(genesisPostStateHash, 1, 1L, 1)
  //
  //          blockIndexCache <- LazyKeyValueCache[Task, MergingVertex, BlockIndex](
  //                              Indexer.createBlockIndex[Task]
  //                            )
  //
  //          a0Index <- {
  //            implicit val ch = blockIndexCache
  //            Indexer.createBranchIndex(
  //              Seq(MergingVertex(postStateHash = a0._1, processedDeploys = a0._2.toSet))
  //            )
  //          }
  //          a1Index <- {
  //            implicit val ch = blockIndexCache
  //            Indexer.createBranchIndex(
  //              Seq(MergingVertex(postStateHash = a1._1, processedDeploys = a1._2.toSet))
  //            )
  //          }
  //          bIndex <- {
  //            implicit val ch = blockIndexCache
  //            Indexer.createBranchIndex(
  //              Seq(MergingVertex(postStateHash = b._1, processedDeploys = b._2.toSet))
  //            )
  //          }
  //
  //          baseDataReader <- LazyKeyValueCache(
  //                             (ch: Blake2b256Hash) => {
  //                               implicit val c = runtimeManager.getHistoryRepo
  //                               baseStateReader
  //                                 .getDataFromChannelHash(ch)
  //                             }
  //                           )
  //          baseJoinsReader <- LazyKeyValueCache(
  //                              (ch: Blake2b256Hash) => {
  //                                implicit val c = runtimeManager.getHistoryRepo
  //                                baseStateReader
  //                                  .getJoinsFromChannelHash(ch)
  //                              }
  //                            )
  //
  //          // branches having the same validator blocks should be confliting
  //          c <- EventsIndexConflictDetectors.findConflicts(
  //                a0Index.eventLogIndex,
  //                a1Index.eventLogIndex,
  //                baseDataReader,
  //                baseJoinsReader
  //              )
  //          // branches having different validator blocks should not conflict
  //          c1 <- EventsIndexConflictDetectors.findConflicts(
  //                 a0Index.eventLogIndex,
  //                 bIndex.eventLogIndex,
  //                 baseDataReader,
  //                 baseJoinsReader
  //               )
  //
  //        } yield assert(c.nonEmpty && c1.isEmpty)
  //      }
  //    }
  //  }
  //
  //  type DagTemplate = Array[List[Int]]
  //  def getRejectedBlocksAtTheTop(dag: DagTemplate) =
  //    runtimeManagerResource.use { runtimeManager =>
  //      {
  //        implicit val rm: RuntimeManager[Task] = runtimeManager
  //        implicit val span                     = NoopSpan[Task]
  //        implicit val metrics                  = new MetricsNOP[Task]
  //
  //        val emptyDag             = InMemDAG[Task, MergingVertex](Map.empty, Map.empty)
  //        val genesisPostStateHash = genesis.postStateHash
  //
  //        val genesisLayer =
  //          Seq(
  //            MergingVertex(
  //              ByteString.copyFromUtf8("genesis"),
  //              genesisPostStateHash,
  //              ByteString.EMPTY,
  //              Set.empty
  //            )
  //          )
  //
  //        val blockIndexCache = LazyKeyValueCache[Task, MergingVertex, BlockIndex](
  //          Indexer.createBlockIndex[Task]
  //        ).runSyncUnsafe()
  //
  //        dag.toList
  //          .foldLeftM[
  //            Task,
  //            (Seq[MergingVertex], InMemDAG[Task, MergingVertex], Long, Seq[ProcessedDeploy])
  //          ](
  //            (genesisLayer, emptyDag, 1L, Seq.empty)
  //          ) {
  //            case ((prevLayer, dag, blockNum, rejectedAcc), levelTemplate) =>
  //              println(s"creating lvl ${blockNum}, template: ${levelTemplate}")
  //              for {
  //                v <- if (prevLayer.size == 1) (prevLayer.head.postStateHash, Seq.empty).pure[Task]
  //                    else CasperDagMerger.merge(prevLayer, genesisLayer.head, dag, blockIndexCache)
  //                (preStateHash, rejectedAtLevel) = v
  //                newLayer <- fs2.Stream
  //                             .emits(
  //                               levelTemplate.map(
  //                                 n =>
  //                                   fs2.Stream.eval(
  //                                     for {
  //                                       s <- mkState(preStateHash, blockNum.toInt, blockNum, n)
  //                                     } yield MergingVertex(
  //                                       postStateHash = s._1,
  //                                       preStateHash = preStateHash,
  //                                       processedDeploys = s._2.toSet
  //                                     )
  //                                   )
  //                               )
  //                             )
  //                             .parJoinUnbounded
  //                             .compile
  //                             .toList
  //                newConnections = newLayer.flatMap(newV => prevLayer.map((_, newV)))
  //                newDag <- newConnections.foldLeftM(dag)(
  //                           (acc, pair) => acc.addEdge(pair._2, pair._1)
  //                         )
  //              } yield (newLayer, newDag, blockNum + 1, rejectedAcc ++ rejectedAtLevel)
  //          }
  //          .map(_._4)
  //      }
  //    }

  //  "stuck last finalized block should" should "be ok" in effectTest {
  //    //
  //    val graph =
  //      s"""
  //         |*
  //         | ***  -> when creating these blocks, error thrown
  //         |*
  //         | ***
  //         |*
  //         | ***
  //         |*
  //         | ***
  //         |*
  //         | """.stripMargin
  //
  //    val layers = graph.split('\n')
  //    val template = layers
  //      .map(_.toList.zipWithIndex.filter((c) => c._1 == '*').map(_._2))
  //      .filter(_.nonEmpty)
  //      .reverse
  //    getRejectedBlocksAtTheTop(template)
  //  }

  "merging with leader" should "work" ignore effectTest {
    runtimeManagerResource.use { runtimeManager =>
      {
        implicit val rm: RuntimeManager[Task] = runtimeManager
        implicit val concurrent               = Concurrent[Task]
        implicit val metrics                  = new Metrics.MetricsNOP

        // simulates 0.99 sync threshold
        def mkLayer(
            baseBlock: BlockMessage,
            dagStore: BlockDagStorage[Task]
        ): Task[BlockMessage] = {

          val baseState        = baseBlock.postStateHash
          val seqNum           = baseBlock.seqNum + 1
          val mergingBlocksNum = (baseBlock.seqNum * 2 + 1).toLong
          val nexBaseBlockNum  = (baseBlock.seqNum * 2 + 2).toLong

          for {
            // create children blocks
            r <- mkTailBlocks(baseState, seqNum, mergingBlocksNum)
            mergingBlocks = r.map(
              b => b.copy(justifications = List(baseBlock.blockHash))
            )
            _ <- mergingBlocks.traverse(dagStore.insert(_, false))

            // merge children blocks
            indices <- (baseBlock +: mergingBlocks)
                        .traverse { b =>
                          val preStateHash  = b.preStateHash
                          val postStateHash = b.postStateHash
                          val seqNum        = b.seqNum
                          val sender        = b.sender
                          for {
                            numberChsData <- runtimeManager.loadMergeableChannels(
                                              postStateHash,
                                              sender.toByteArray,
                                              seqNum
                                            )

                            blockIndex <- BlockIndex(
                                           b.blockHash,
                                           b.state.deploys,
                                           b.state.systemDeploys,
                                           preStateHash.toBlake2b256Hash,
                                           postStateHash.toBlake2b256Hash,
                                           runtimeManager.getHistoryRepo,
                                           numberChsData
                                         ).map(b.blockHash -> _)
                          } yield blockIndex
                        }
                        .map(_.toMap)
            dag             <- dagStore.getRepresentation
            acceptedFinally = indices(baseBlock.blockHash).deployChains.toSet
            rejectedFinally = Set.empty[DeployChainIndex]
            v <- DagMerger.merge[Task](
                  mergingBlocks.map(b => b.blockHash).toSet,
                  Set(baseBlock.blockHash),
                  baseBlock.postStateHash.toBlake2b256Hash,
                  dag.dagMessageState.msgMap,
                  acceptedFinally,
                  rejectedFinally,
                  runtimeManager.getHistoryRepo,
                  (b: BlockHash) => indices(b).pure
                )
            (postState, rejectedDeploys) = v
            mergedState                  = ByteString.copyFrom(postState.bytes.toArray)
            _                            = assert(rejectedDeploys.size == 0)
            _                            = assert(mergedState != baseBlock.postStateHash)

            // create next base block (merge block)
            r             <- mkHeadBlock(mergedState, seqNum, nexBaseBlockNum)
            nextBaseBlock = r.copy(justifications = mergingBlocks.map(_.blockHash))
            _             <- dagStore.insert(nextBaseBlock, false)
//            _             <- dagStore.recordDirectlyFinalized(nextBaseBlock.blockHash, _ => ().pure[Task])
          } yield nextBaseBlock
        }

        val kvm = new InMemoryStoreManager
        for {
          dagStore <- BlockDagKeyValueStorage.create[Task](kvm)
          _        <- dagStore.insert(genesis, false, true)
          _ <- ((genesis, 0)).tailRecM {
                case (start, layerNum) =>
                  if (layerNum < 4)
                    mkLayer(start, dagStore).map(
                      post => (post, layerNum + 1).asLeft[BlockMessage]
                    )
                  else (start).asRight[(BlockMessage, Int)].pure
              }
        } yield ()
      }
    }
  }

}
