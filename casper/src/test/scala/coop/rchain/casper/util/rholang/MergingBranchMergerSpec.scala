package coop.rchain.casper.util.rholang

import cats.effect.{Concurrent, Resource}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.blocks.merger.{
  CasperDagMerger,
  ConflictDetectors,
  MergingBranchIndex,
  MergingVertex
}
import coop.rchain.casper.protocol.{ProcessedDeploy, ProcessedSystemDeploy}
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang.costacc.CloseBlockDeploy
import coop.rchain.casper.util.{ConstructDeploy, EventConverter, GenesisBuilder}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.dag.{DagReader, InMemDAG}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.merger.EventChain
import coop.rchain.shared.scalatestcontrib.effectTest
import coop.rchain.shared.{Log, Time}
import coop.rchain.store.LazyKeyValueCache
import fs2.Stream
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.Seq
import scala.io.Source

class MergingBranchMergerSpec extends FlatSpec with Matchers {

  val genesisContext             = GenesisBuilder.buildGenesis(validatorsNum = 11)
  val genesis                    = genesisContext.genesisBlock
  implicit val logEff            = Log.log[Task]
  implicit val timeF: Time[Task] = new LogicalTime[Task]

  val runtimeManagerResource: Resource[Task, RuntimeManager[Task]] = for {
    dirs <- Resources.copyStorage[Task](genesisContext.storageDirectory)
    rm   <- Resources.mkRuntimeManagerAt[Task](dirs.rspaceDir)()
  } yield rm

//  private def txTerm(from: String = "", to: String = "") =
//    Source
//      .fromResource("RevTx.rho")
//      .mkString
//      .replaceAll("%from%", from)
//      .replaceAll("%to%", to)
//      .replaceAll("%amount%", 100000.toString)

  private def biTxTerm(from: String, to: String) =
    Source
      .fromResource("RevTxBackAndForth.rho")
      .mkString
      .replaceAll("%from%", from)
      .replaceAll("%to%", to)
      .replaceAll("%amt%", 1000000.toString)
      .replaceAll("%amt_back%", 100.toString)

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
            merger <- runtimeManager.getHistoryRepo.stateMerger
            // result of merge execution
            mergeHash <- merger
                          .merge(
                            Blake2b256Hash.fromByteString(mainPostStateHash),
                            Seq(
                              EventChain(
                                startState = Blake2b256Hash.fromByteString(genesisPostStateHash),
                                endState = Blake2b256Hash.fromByteString(mergingPostStateHash),
                                events = mergeEvents
                              )
                            )
                          )
                          .map(_.toByteString)

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

        for {
          baseStateReader <- runtimeManager.getHistoryRepo
                              .reset(Blake2b256Hash.fromByteString(genesisPostStateHash))

          // a0 and a1 are states created by the same validator, so event logs should expose conflict
          a0 <- mkState(genesisPostStateHash, 1, 1L, 0)
          a1 <- mkState(genesisPostStateHash, 2, 2L, 0)
          // a0 and b are ceated by different validators, there should be no conflicts
          b <- mkState(genesisPostStateHash, 1, 1L, 1)

          a0Index <- MergingBranchIndex.create(
                      Seq(MergingVertex(postStateHash = a0._1, processedDeploys = a0._2.toSet))
                    )
          a1Index <- MergingBranchIndex.create(
                      Seq(MergingVertex(postStateHash = a1._1, processedDeploys = a1._2.toSet))
                    )
          bIndex <- MergingBranchIndex.create(
                     Seq(MergingVertex(postStateHash = b._1, processedDeploys = b._2.toSet))
                   )

          baseDataReader <- LazyKeyValueCache(
                             (ch: Blake2b256Hash) =>
                               baseStateReader
                                 .getDataFromChannelHash(ch)
                           )
          baseJoinsReader <- LazyKeyValueCache(
                              (ch: Blake2b256Hash) =>
                                baseStateReader
                                  .getJoinsFromChannelHash(ch)
                            )

          // branches having the same validator blocks should be confliting
          c <- ConflictDetectors.findConflicts(a0Index, a1Index, baseDataReader, baseJoinsReader)
          // branches having different validator blocks should not conflict
          c1 <- ConflictDetectors.findConflicts(a0Index, bIndex, baseDataReader, baseJoinsReader)

        } yield assert(c.nonEmpty && c1.isEmpty)
      }
    }
  }

  "conflict detection for branches having blocks from the same/different validators having b" +
    "ase as an ancestor but not direct parent" should "be correct" in effectTest {
    import coop.rchain.rspace.syntax._
    runtimeManagerResource.use { runtimeManager =>
      {
        implicit val rm: RuntimeManager[Task] = runtimeManager
        implicit val conc                     = Concurrent[Task]
        val genesisPostStateHash              = genesis.body.state.postStateHash

        for {
          baseStateReader <- runtimeManager.getHistoryRepo
                              .reset(Blake2b256Hash.fromByteString(genesisPostStateHash))

          // a0 and a1 are states created by the same validator, so event logs should expose conflict
          a0 <- mkState(genesisPostStateHash, 1, 1L, 0)
          a1 <- mkState(genesisPostStateHash, 2, 2L, 0)
          // a2 and a3 are children of base, and base is finalized block
          // this should as well expose conflict, but ITS NOT. Because the fist conflict rule "ace for base event"
          // is not detected.
          // It is not detected because event in the base is not EXACTLY the same event that event logs ae competing for.
          a2 <- mkState(a1._1, 3, 3L, 0)
          a3 <- mkState(a2._1, 4, 4L, 0)

          a2Index <- MergingBranchIndex.create(
                      Seq(MergingVertex(postStateHash = a2._1, processedDeploys = a2._2.toSet))
                    )
          a3Index <- MergingBranchIndex.create(
                      Seq(MergingVertex(postStateHash = a3._1, processedDeploys = a3._2.toSet))
                    )

          baseDataReader <- LazyKeyValueCache(
                             (ch: Blake2b256Hash) =>
                               baseStateReader
                                 .getDataFromChannelHash(ch)
                           )
          baseJoinsReader <- LazyKeyValueCache(
                              (ch: Blake2b256Hash) =>
                                baseStateReader
                                  .getJoinsFromChannelHash(ch)
                            )

          // branches having the same validator blocks should be confliting
          c <- ConflictDetectors.findConflicts(a2Index, a3Index, baseDataReader, baseJoinsReader)

        } yield assert(c.nonEmpty)
      }
    }
  }

  type DagTemplate = Array[List[Int]]
  def getRejectedBlocksAtTheTop(dag: DagTemplate) =
    runtimeManagerResource.use { runtimeManager =>
      {
        implicit val rm: RuntimeManager[Task] = runtimeManager
        val emptyDag                          = InMemDAG[Task, MergingVertex](Map.empty, Map.empty)
        val genesisPostStateHash              = genesis.body.state.postStateHash

        val genesisLayer =
          Seq(
            MergingVertex(
              ByteString.copyFromUtf8("genesis"),
              genesisPostStateHash,
              ByteString.EMPTY,
              Set.empty
            )
          )

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
                    else CasperDagMerger.merge(prevLayer, genesisLayer.head, dag)
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

  "conflicting branches" should "be conflicting" in effectTest {
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
         | ***
         |*
         | ***
         |*
         | ***
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

  "`sequence of serial execution` and `merging when base is not direct parent`" should "be equal" in effectTest {
    runtimeManagerResource.use { runtimeManager =>
      {
        implicit val rm: RuntimeManager[Task] = runtimeManager
        val genesisPostStateHash              = genesis.body.state.postStateHash

        val tryIt = for {
          merger <- runtimeManager.getHistoryRepo.stateMerger
          _      = println(s"New try")

          // serial execution
          a0 <- mkState(genesisPostStateHash, 1, 1L, 0)
          b0 <- mkState(a0._1, 1, 1L, 1)
          a1 <- mkState(b0._1, 2, 2L, 0)

          (a0Events, b0Events, a1Events) = Seq(a0, b0, a1).map(
            _._2.flatMap(_.deployLog).map(EventConverter.toRspaceEvent)
          ) match {
            case Seq(a, b, c) => (a, b, c)
          }
          (a0Serial, b0Serial, a1Serial) = Seq(a0._1, b0._1, a1._1)
            .map(Blake2b256Hash.fromByteString) match {
            case Seq(a, b, c) => (a, b, c)
          }

          // merge execution
          a0merge <- merger
                      .merge(
                        Blake2b256Hash.fromByteString(genesisPostStateHash),
                        Seq(
                          EventChain(
                            Blake2b256Hash.fromByteString(genesisPostStateHash),
                            a0Serial,
                            a0Events
                          )
                        )
                      )
                      .map(_.toByteString)
          b0Merge <- merger
                      .merge(
                        // if base is a0merge - its equal to first test `serial execution and merging should be merged`
                        Blake2b256Hash.fromByteString(a0merge),
                        Seq(
                          EventChain(
                            Blake2b256Hash.fromByteString(genesisPostStateHash),
                            b0Serial,
                            b0Events
                          )
                        )
                      )
                      .map(_.toByteString)
          a1Merge <- merger
                      .merge(
                        // if base is b0merge - its equal to first test `serial execution and merging should be merged`
                        Blake2b256Hash.fromByteString(b0Merge),
                        Seq(
                          EventChain(
                            Blake2b256Hash.fromByteString(genesisPostStateHash),
                            a1Serial,
                            a1Events
                          )
                        )
                      )
                      .map(_.toByteString)

          _ = println(
            s"${(Seq(a0._1, b0._1, a1._1) zip Seq(a0merge, b0Merge, a1Merge))
              .map(a => PrettyPrinter.buildString(a._1) + " <-> " + PrettyPrinter.buildString(a._2))
              .mkString("\n")}"
          )
          _ = assert(PrettyPrinter.buildString(b0._1) == PrettyPrinter.buildString(b0Merge))
          _ = assert(PrettyPrinter.buildString(a1._1) == PrettyPrinter.buildString(a1Merge))
        } yield ()

        fs2.Stream
          .emits(List.range(0, 10).map(_ => fs2.Stream.eval(tryIt)))
          .parJoin(1)
          .compile
          .drain

      }
    }
  }

  "1 block branches" should "be merged" in effectTest {
    runtimeManagerResource.use { runtimeManager =>
      {
        implicit val rm: RuntimeManager[Task] = runtimeManager
        val emptyDag                          = InMemDAG[Task, MergingVertex](Map.empty, Map.empty)
        val genesisPostStateHash              = genesis.body.state.postStateHash

        // simulates 0.99 sync threshold - first validator create state, then all other create on top of that/
        // merge state of resulting blocks returned
        def mkLayer(startPreStateHash: StateHash, n: Int): Task[StateHash] =
          for {
            // create state on first validator
            b    <- mkHeadState(startPreStateHash, n * 2 + 1, (n * 2 + 1).toLong)
            base = MergingVertex(postStateHash = b._1, processedDeploys = b._2.toSet)
            _    = println(s"building on ${PrettyPrinter.buildString(startPreStateHash)}")

            // create children an all other
            baseChildren <- mkTailStates(base.postStateHash, n * 2 + 2, (n * 2 + 2).toLong)
            mergingTips = baseChildren.map(
              c => MergingVertex(postStateHash = c._1, processedDeploys = c._2.toSet)
            )
            dag <- mergingTips.foldLeftM(emptyDag)((acc, tip) => acc.addEdge(tip, base))

            // merge children to get next preStateHash
            v                                   <- CasperDagMerger.merge(mergingTips, base, dag)
            (nextPreStateHash, rejectedDeploys) = v
            _                                   = assert(rejectedDeploys.size == 0)
            _                                   = println(s"merge result ${PrettyPrinter.buildString(nextPreStateHash)}")
          } yield nextPreStateHash

        for {
          // create base state - child of genesis
          _ <- List.range(0, 0).foldLeftM(genesisPostStateHash) { (baseHash, i) =>
                mkLayer(baseHash, i)
              }
        } yield ()
      }
    }
  }

  /**
    *
    * Builds DAG like this in a loop.
    * 1. head validator creates a block on top of input state
    * 2. tail validators build on top
    * 3. head validator merges and builds
    * 4. tail validators build on top
    * 5. head validator merges (having base the very first block) and pass further to the loop
    *
    * In other wods, network build this DAG (validator number/block height)
    *
    *      1 2 3 4 5 .. N
    *   5  *
    *   4    * * * * .. *
    *   3  *
    *   2    * * * * .. *
    *   1  *                <---- this block is always base when doing merges
    *
    */
  "DAG" should "be merged" in effectTest {

    val graph =
      s"""

         |** 
         |*
         |* *********
         |*
         | """.stripMargin

    //works
//    val graph =
//      s"""
//         |          *
//         |          *
//         |        **
//         |    **
//         |*
//         | ***  *
//         |*
//         |      ****
//         | """.stripMargin

    // rarely fails
//    val graph =
//      s"""
//         |*  *
//         |* *
//         |**
//         | """.stripMargin

//    // sometimes fail
//    val graph =
//      s"""
//         |**
//         |**
//         |**
//         |**
//         |**
//         | """.stripMargin

    val layers = graph.split('\n')
    val dag = layers
      .map(_.toList.zipWithIndex.filter((c) => c._1 == '*').map(_._2))
      .filter(_.nonEmpty)
      .reverse
    getRejectedBlocksAtTheTop(dag)
  }

  /** This created dag like this
    *
    *   12
    * 3 **
    * 2 **
    * 1 **
    *
    * Two validators create states, and use merged previous layer as a preState.
    * Merge happens 2 times, so on the the second merge states that have to be merged
    * are built on top of merged states themselves.
    *
    *
    */
//              *  * -> blocks on top of merge 2 -> ERROR: refund fail here
//              |\/|
//              |/\|
//             a1  b1 -> blocks on top of merge 1
//              |\/|
//              |/\|
//             a0  b0
//               \/
//                * -> genesis (always base when merging)

  "branch containing merged blocks" should "be merged" in effectTest {
    runtimeManagerResource.use { runtimeManager =>
      {
        implicit val rm: RuntimeManager[Task] = runtimeManager
        val genesisPostStateHash              = genesis.body.state.postStateHash

        val tryIt = for {

          merger1 <- runtimeManager.getHistoryRepo.stateMerger

          _ = println(s"New try")

          a1       <- mkState(genesisPostStateHash, 1, 1L, 0)
          b1       <- mkState(genesisPostStateHash, 1, 1L, 1)
          main1    = a1
          merging1 = b1
          mergeHash1 <- merger1
                         .merge(
                           Blake2b256Hash.fromByteString(main1._1),
                           Seq(
                             EventChain(
                               Blake2b256Hash.fromByteString(genesisPostStateHash),
                               Blake2b256Hash.fromByteString(merging1._1),
                               merging1._2.flatMap(_.deployLog).map(EventConverter.toRspaceEvent)
                             )
                           )
                         )
                         .map(_.toByteString)

          a2       <- mkState(mergeHash1, 2, 2L, 0)
          b2       <- mkState(mergeHash1, 2, 2L, 1)
          main2    = a2
          merging2 = b2

          merger2 <- runtimeManager.getHistoryRepo.stateMerger
          mergeHash2 <- merger2
                         .merge(
                           Blake2b256Hash.fromByteString(main2._1),
                           Seq(
                             EventChain(
                               Blake2b256Hash.fromByteString(genesisPostStateHash),
                               Blake2b256Hash.fromByteString(merging2._1),
                               merging2._2.flatMap(_.deployLog).map(EventConverter.toRspaceEvent)
                             )
                           )
                         )
                         .map(_.toByteString)

          a3 <- mkState(mergeHash2, 3, 3L, 0)
          b3 <- mkState(mergeHash2, 3, 3L, 1)

        } yield ()

        fs2.Stream
          .emits(List.range(0, 10).map(_ => fs2.Stream.eval(tryIt)))
          // sequential execution of attempts also fails, most probably within 2-5 attempts
          .parJoin(1)
          .compile
          .drain

      }
    }
  }

  "runtimeManagerResource" should "make non conflicting parallel transfers" in effectTest {
    // requires changing in AuthKey.rho
    // ret!(response == { bundle0{ (*_authKey, shape) } } )
    //  to
    // ret!(true)
    // to be able to access any REV vault with any deployerId
    runtimeManagerResource.use { runtimeManager =>
      {
        implicit val timeF: Time[Task] = new LogicalTime[Task]

        val term =
          (genesisContext.validatorPks.drop(1) zip genesisContext.genesisVailtsPks.drop(1)).map {
            case (_, vault1) => {
              val vault2 = Secp256k1.newKeyPair._2
              biTxTerm(
                RevAddress.fromPublicKey(vault1).get.toBase58,
                RevAddress.fromPublicKey(vault2).get.toBase58
              )
            }
          }
        for {
          txDeploy <- ConstructDeploy.sourceDeployNowF(
                       term.mkString("| \n"),
                       sec = genesisContext.genesisVaults.head._1,
                       phloLimit = 1000000000000L
                     )
          _ = println(s"Deploying ${txDeploy}")
          r <- runtimeManager.computeState(genesis.body.state.postStateHash)(
                txDeploy :: Nil,
                CloseBlockDeploy(
                  SystemDeployUtil
                    .generateCloseDeployRandomSeed(
                      genesisContext.validatorKeyPairs.head._2,
                      0
                    )
                ) :: Nil,
                BlockData(
                  txDeploy.data.timestamp,
                  0,
                  genesisContext.validatorKeyPairs.head._2,
                  0
                ),
                Map.empty
              )
        } yield r
      }
    }
  }
}
