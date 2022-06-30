package coop.rchain.casper.rholang

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper.BlockRandomSeed
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.helper._
import coop.rchain.casper.protocol._
import coop.rchain.casper.rholang.InterpreterUtil._
import coop.rchain.casper.rholang.Resources.mkRuntimeManager
import coop.rchain.casper.rholang.RuntimeManager.StateHash
import coop.rchain.casper.rholang.types.SystemDeploy
import coop.rchain.casper.util.GenesisBuilder.{buildGenesis, buildGenesisParametersSize}
import coop.rchain.casper.util.RSpaceUtil._
import coop.rchain.casper.util.{ConstructDeploy, GenesisBuilder}
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.PCost
import coop.rchain.models.syntax._
import coop.rchain.p2p.EffectsTestInstances.LogStub
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.scalatestcontrib._
import coop.rchain.shared.{Log, LogSource, Time}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InterpreterUtilTest
    extends AnyFlatSpec
    with Matchers
    with BlockGenerator
    with BlockDagStorageFixture {
  import BlockGenerator.step

  implicit val logEff                    = new LogStub[Task]
  implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  implicit val span: Span[Task]          = new NoopSpan[Task]
  implicit val logSource: LogSource      = LogSource(this.getClass)
  implicit private val timeEff           = Time.fromTimer[Task]

  val genesisContext = GenesisBuilder.buildGenesis()
  val genesis        = genesisContext.genesisBlock

  def computeDeploysCheckpoint[F[_]: Concurrent: RuntimeManager: BlockDagStorage: BlockStore: Log: Metrics: Span](
      parents: Seq[BlockHash],
      deploys: Seq[Signed[DeployData]],
      blockNumber: Long = 0L,
      seqNum: Long = 0
  ): F[
    Either[
      Throwable,
      (StateHash, StateHash, Seq[ProcessedDeploy], Seq[ByteString], Seq[ProcessedSystemDeploy])
    ]
  ] =
    computeParentsPostState(parents, dummyParentsPreState)
      .flatMap(
        preState => {
          val seed = BlockRandomSeed(
            genesis.shardId,
            blockNumber,
            genesisContext.validatorPks.head,
            preState._1.toBlake2b256Hash
          )
          val rand = BlockRandomSeed.generateRandomNumber(seed)
          InterpreterUtil
            .computeDeploysCheckpoint[F](
              deploys,
              List.empty[SystemDeploy],
              rand,
              BlockData(blockNumber, genesisContext.validatorPks.head, seqNum),
              preState
            )
        }
      )
      .attempt

  "computeBlockCheckpoint" should "compute the final post-state of a chain properly" in effectTest {
    val time    = 0L
    val shardId = genesis.shardId
    val b0Deploys = Vector(
      "@1!(1)",
      "@2!(2)",
      "for(@a <- @1){ @123!(5 * a) }"
    ).map(d => ConstructDeploy.sourceDeploy(d, time + 1, shardId = shardId))

    val b1Deploys = Vector(
      "@1!(1)",
      "for(@a <- @2){ @456!(5 * a) }"
    ).map(d => ConstructDeploy.sourceDeploy(d, time + 2, shardId = shardId))

    val b2Deploys = Vector(
      "for(@a <- @123 & @b <- @456){ @1!(a + b) }"
    ).map(d => ConstructDeploy.sourceDeploy(d, time + 3, shardId = shardId))

    val b3Deploys = Vector(
      "@7!(7)"
    ).map(d => ConstructDeploy.sourceDeploy(d, time + 4, shardId = shardId))

    /*
     * DAG Looks like this:
     *
     *          b3
     *           |
     *          b2
     *           |
     *          b1
     *           |
     *          b0
     *           |
     *          genesis
     */
    val genesisContext = buildGenesis(buildGenesisParametersSize(4))
    TestNode.standaloneEff(genesisContext).use { node =>
      implicit val runtimeManager = node.runtimeManager
      for {
        b0 <- node.addBlock(b0Deploys: _*)
        b1 <- node.addBlock(b1Deploys: _*)
        b2 <- node.addBlock(b2Deploys: _*)
        b3 <- node.addBlock(b3Deploys: _*)
        _  <- getDataAtPublicChannel[Task](b0, 2) shouldBeF Seq("2")
        _  <- getDataAtPublicChannel[Task](b0, 123) shouldBeF Seq("5")
        _  <- getDataAtPublicChannel[Task](b1, 1) shouldBeF Seq("1")
        _  <- getDataAtPublicChannel[Task](b1, 123) shouldBeF Seq("5")
        _  <- getDataAtPublicChannel[Task](b1, 456) shouldBeF Seq("10")
        _ <- getDataAtPublicChannel[Task](b3, 1)
              .map(_ should contain theSameElementsAs Seq("1", "15"))
        _ <- getDataAtPublicChannel[Task](b3, 7) shouldBeF Seq("7")
      } yield ()
    }
  }

  //TODO reenable when merging of REV balances is done
  it should "merge histories in case of multiple parents" ignore effectTest {

    val b1Deploys = Vector(
      "@5!(5)",
      "@2!(2)",
      "for(@a <- @2){ @456!(5 * a) }"
    ).map(ConstructDeploy.sourceDeployNow(_, ConstructDeploy.defaultSec2))

    val b2Deploys = Vector(
      "@1!(1)",
      "for(@a <- @1){ @123!(5 * a) }"
    ).map(ConstructDeploy.sourceDeployNow(_))

    val b3Deploys = Vector(
      "for(@a <- @123 & @b <- @456){ @1!(a + b) }"
    ).map(ConstructDeploy.sourceDeployNow(_))

    /*
     * DAG Looks like this:
     *
     *        b3
     *        |   \
     *        b1    b2
     *         \    /
     *         genesis
     */

    TestNode.networkEff(genesisContext, networkSize = 2).use {
      case node1 +: node2 +: _ =>
        implicit val runtimeManager = node1.runtimeManager
        for {
          b1 <- node1.addBlock(b1Deploys: _*)
          b2 <- node2.propagateBlock(b2Deploys: _*)(node1)
          b3 <- node1.addBlock(b3Deploys: _*)

          _ = b3.justifications shouldBe Set(b1, b2).map(_.blockHash)
          _ <- getDataAtPublicChannel[Task](b3, 5) shouldBeF Seq("5")
          _ <- getDataAtPublicChannel[Task](b3, 1) shouldBeF Seq("15")
        } yield ()
    }
  }

  val registry =
    """
    |new ri(`rho:registry:insertArbitrary`) in {
    |  new X, Y in {
    |    ri!(*X, *Y)
    |  }
    |}
  """.stripMargin

  val other =
    """
    |new helloWorld, stdout(`rho:io:stdout`), stdoutAck(`rho:io:stdoutAck`) in {
    |  contract helloWorld(@name) = {
    |    new ack in {
    |      stdoutAck!("Hello, ", *ack) |
    |      for (_ <- ack) {
    |        stdoutAck!(name, *ack) |
    |        for (_ <- ack) {
    |          stdout!("\n")
    |        }
    |      }
    |    }
    |  } |
    |  helloWorld!("Joe")
    |}
    |
  """.stripMargin

  def prepareDeploys(v: Vector[String], c: PCost) = {
    val genesisDeploys = v.map(ConstructDeploy.sourceDeployNow(_))
    genesisDeploys.map(d => ProcessedDeploy(d, c, List.empty, false))
  }

  it should "merge histories in case of multiple parents with complex contract" ignore withGenesis(
    genesisContext
  ) { implicit blockStore => implicit blockDagStorage => implicit runtimeManager =>
    val contract = registry

    val b1DeploysWithCost = prepareDeploys(Vector(contract), PCost(2L))
    val b2DeploysWithCost = prepareDeploys(Vector(contract), PCost(1L))
    val b3DeploysWithCost = prepareDeploys(Vector.empty, PCost(5L))

    /*
     * DAG Looks like this:
     *
     *           b3
     *          /  \
     *        b1    b2
     *         \    /
     *         genesis
     */
    for {
      b1 <- buildBlock[Task](justifications = Seq(genesis.blockHash), deploys = b1DeploysWithCost)
      b2 <- buildBlock[Task](justifications = Seq(genesis.blockHash), deploys = b2DeploysWithCost)
      b3 <- buildBlock[Task](
             justifications = Seq(b1.blockHash, b2.blockHash),
             deploys = b3DeploysWithCost
           )
      _         <- step[Task](b1)
      _         <- step[Task](b2)
      postState <- validateBlockCheckpoint[Task](b3)
      result    = postState shouldBe Right(None)
    } yield result
  }

  it should "merge histories in case of multiple parents (uneven histories)" ignore withGenesis(
    genesisContext
  ) { implicit blockStore => implicit blockDagStorage => implicit runtimeManager =>
    val contract = registry

    val b1DeploysWithCost = prepareDeploys(Vector(contract), PCost(2L))
    val b2DeploysWithCost = prepareDeploys(Vector(contract), PCost(1L))
    val b3DeploysWithCost = prepareDeploys(Vector(contract), PCost(5L))
    val b4DeploysWithCost = prepareDeploys(Vector(contract), PCost(5L))
    val b5DeploysWithCost = prepareDeploys(Vector(contract), PCost(5L))

    /*
     * DAG Looks like this:
     *
     *           b5
     *          /  \
     *         |    |
     *         |    b4
     *         |    |
     *        b2    b3
     *         \    /
     *          \  /
     *           |
     *           b1
     *           |
     *         genesis
     */
    for {
      b1 <- buildBlock[Task](justifications = Seq(genesis.blockHash), deploys = b1DeploysWithCost)
      b2 <- buildBlock[Task](justifications = Seq(b1.blockHash), deploys = b2DeploysWithCost)
      b3 <- buildBlock[Task](justifications = Seq(b1.blockHash), deploys = b3DeploysWithCost)
      b4 <- buildBlock[Task](justifications = Seq(b3.blockHash), deploys = b4DeploysWithCost)
      b5 <- buildBlock[Task](
             justifications = Seq(b2.blockHash, b4.blockHash),
             deploys = b5DeploysWithCost
           )
      _ <- step[Task](b1)
      _ <- step[Task](b2)
      _ <- step[Task](b3)
      _ <- step[Task](b4)

      postState <- validateBlockCheckpoint[Task](b5)
      result    = postState shouldBe Right(None)
    } yield result
  }

  def computeDeployCosts(deploy: Signed[DeployData]*)(
      implicit runtimeManager: RuntimeManager[Task],
      bds: BlockDagStorage[Task],
      blockStore: BlockStore[Task]
  ): Task[Seq[PCost]] =
    for {
      computeResult                         <- computeDeploysCheckpoint[Task](Seq(genesis.blockHash), deploy)
      Right((_, _, processedDeploys, _, _)) = computeResult
    } yield processedDeploys.map(_.cost)

  "computeDeploysCheckpoint" should "aggregate cost of deploying rholang programs within the block" in withGenesis(
    genesisContext
  ) { implicit blockStore => implicit blockDagStorage => implicit runtimeManager =>
    //reference costs
    //deploy each Rholang program separately and record its cost

    for {
      deploy1 <- ConstructDeploy.sourceDeployNowF("@1!(Nil)")
      deploy2 <- ConstructDeploy.sourceDeployNowF("@3!([1,2,3,4])")
      deploy3 <- ConstructDeploy.sourceDeployNowF("for(@x <- @0) { @4!(x.toByteArray()) }")

      cost1        <- computeDeployCosts(deploy1)
      cost2        <- computeDeployCosts(deploy2)
      cost3        <- computeDeployCosts(deploy3)
      accCostsSep  = cost1 ++ cost2 ++ cost3
      accCostBatch <- computeDeployCosts(deploy1, deploy2, deploy3)
    } yield accCostBatch should contain theSameElementsAs accCostsSep
  }

  it should "return cost of deploying even if one of the programs within the deployment throws an error" in
    pendingUntilFixed { //reference costs
      withGenesis(genesisContext) {
        implicit blockStore => implicit blockDagStorage =>
          implicit runtimeManager =>
            //deploy each Rholang program separately and record its cost
            val deploy1 = ConstructDeploy.sourceDeployNow("@1!(Nil)")
            val deploy2 = ConstructDeploy.sourceDeployNow("@2!([1,2,3,4])")
            for {
              cost1 <- computeDeployCosts(deploy1)
              cost2 <- computeDeployCosts(deploy2)

              accCostsSep = cost1 ++ cost2

              deployErr    = ConstructDeploy.sourceDeployNow("@3!(\"a\" + 3)")
              accCostBatch <- computeDeployCosts(deploy1, deploy2, deployErr)
            } yield accCostBatch should contain theSameElementsAs accCostsSep
      }
    }

  // TODO: ignored until support is for invalid block is implemented
  "validateBlockCheckpoint" should "not return a checkpoint for an invalid block" ignore withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val deploys = Vector("@1!(1)").map(ConstructDeploy.sourceDeployNow(_))
      val processedDeploys =
        deploys.map(d => ProcessedDeploy(d, PCost(1L), List.empty, false))
      val invalidHash = ByteString.EMPTY
      mkRuntimeManager[Task](
        "interpreter-util-test",
        BlockRandomSeed.nonNegativeMergeableTagName(genesis.shardId)
      ).use { implicit runtimeManager =>
        for {
          block            <- createGenesis[Task](deploys = processedDeploys, tsHash = invalidHash)
          validateResult   <- validateBlockCheckpoint[Task](block)
          Right(stateHash) = validateResult
        } yield stateHash should be(None)
      }
  }

  it should "return a checkpoint with the right hash for a valid block" in withGenesis(
    genesisContext
  ) { implicit blockStore => implicit blockDagStorage => implicit runtimeManager =>
    val deploys =
      Vector(
        "@1!(1)",
        "@2!(1)",
        "@2!(2)",
        "@2!(3)",
        "@2!(4)",
        "@2!(5)",
        "for (@x <- @1) { @2!(x) }",
        "for (@x <- @2) { @3!(x) }"
      ).map(ConstructDeploy.sourceDeployNow(_))
    for {
      deploysCheckpoint <- computeDeploysCheckpoint[Task](
                            Seq(genesis.blockHash),
                            deploys,
                            blockNumber = 1L
                          )
      Right((preStateHash, computedTsHash, processedDeploys, _, _)) = deploysCheckpoint
      block <- createBlock[Task](
                ByteString.copyFrom(genesisContext.validatorPks.head.bytes),
                deploys = processedDeploys,
                postStateHash = computedTsHash,
                preStateHash = preStateHash,
                justifications = Seq(genesis.blockHash)
              )

      validateResult <- validateBlockCheckpoint[Task](block)
      Right(tsHash)  = validateResult
    } yield tsHash should be(Some(computedTsHash))
  }

  it should "pass linked list test" in withGenesis(genesisContext) {
    implicit blockStore => implicit blockDagStorage => implicit runtimeManager =>
      val deploys = Vector(
        """
        |contract @"recursionTest"(@list) = {
        |  new loop in {
        |    contract loop(@rem, @acc) = {
        |      match rem {
        |        [head, ...tail] => {
        |          new newAccCh in {
        |            newAccCh!([head, acc]) |
        |            for(@newAcc <- newAccCh) {
        |              loop!(tail, newAcc)
        |            }
        |          }
        |        }
        |        _ => { Nil } // Normally we would print the "acc" ([2,[1,[]]]) out
        |      }
        |    } |
        |    new unusedCh in {
        |      loop!(list, [])
        |    }
        |  }
        |} |
        |@"recursionTest"!([1,2])
      """.stripMargin
      ).map(ConstructDeploy.sourceDeployNow(_))
      for {
        deploysCheckpoint <- computeDeploysCheckpoint[Task](
                              Seq(genesis.blockHash),
                              deploys,
                              1L
                            )
        Right((preStateHash, computedTsHash, processedDeploys, _, _)) = deploysCheckpoint
        block <- createBlock[Task](
                  ByteString.copyFrom(genesisContext.validatorPks.head.bytes),
                  deploys = processedDeploys,
                  postStateHash = computedTsHash,
                  preStateHash = preStateHash,
                  justifications = Seq(genesis.blockHash)
                )
        validateResult <- validateBlockCheckpoint[Task](block)
        Right(tsHash)  = validateResult
      } yield tsHash should be(Some(computedTsHash))
  }

  it should "pass persistent produce test with causality" in withGenesis(genesisContext) {
    implicit blockStore => implicit blockDagStorage => implicit runtimeManager =>
      val deploys =
        Vector("""new x, y, delay in {
              contract delay(@n) = {
                if (n < 100) {
                  delay!(n + 1)
                } else {
                  x!!(1)
                }
              } |
              delay!(0) |
              y!(0) |
              for (_ <- x & @0 <- y) { y!(1) } |
              for (_ <- x & @1 <- y) { y!(2) } |
              for (_ <- x & @2 <- y) { y!(3) } |
              for (_ <- x & @3 <- y) { y!(4) } |
              for (_ <- x & @4 <- y) { y!(5) } |
              for (_ <- x & @5 <- y) { y!(6) } |
              for (_ <- x & @6 <- y) { y!(7) } |
              for (_ <- x & @7 <- y) { y!(8) } |
              for (_ <- x & @8 <- y) { y!(9) } |
              for (_ <- x & @9 <- y) { y!(10) } |
              for (_ <- x & @10 <- y) { y!(11) } |
              for (_ <- x & @11 <- y) { y!(12) } |
              for (_ <- x & @12 <- y) { y!(13) } |
              for (_ <- x & @13 <- y) { y!(14) } |
              for (_ <- x & @14 <- y) { Nil }
             }
          """)
          .map(ConstructDeploy.sourceDeployNow(_))

      for {
        deploysCheckpoint <- computeDeploysCheckpoint[Task](
                              Seq(genesis.blockHash),
                              deploys,
                              1L
                            )
        Right((preStateHash, computedTsHash, processedDeploys, _, _)) = deploysCheckpoint
        block <- createBlock[Task](
                  ByteString.copyFrom(genesisContext.validatorPks.head.bytes),
                  deploys = processedDeploys,
                  postStateHash = computedTsHash,
                  preStateHash = preStateHash,
                  justifications = Seq(genesis.blockHash)
                )
        validateResult <- validateBlockCheckpoint[Task](block)
        Right(tsHash)  = validateResult
      } yield tsHash should be(Some(computedTsHash))
  }

  it should "pass tests involving primitives" in withGenesis(genesisContext) {
    implicit blockStore => implicit blockDagStorage => implicit runtimeManager =>
      val deploys =
        Vector(
          """
          |new loop, primeCheck, stdoutAck(`rho:io:stdoutAck`) in {
          |  contract loop(@x) = {
          |    match x {
          |      [] => Nil
          |      [head ...tail] => {
          |        new ret in {
          |          for (_ <- ret) {
          |            loop!(tail)
          |          } | primeCheck!(head, *ret)
          |        }
          |      }
          |    }
          |  } |
          |  contract primeCheck(@x, ret) = {
          |    match x {
          |      Nil => stdoutAck!("Nil", *ret)
          |      ~{~Nil | ~Nil} => stdoutAck!("Prime", *ret)
          |      _ => stdoutAck!("Composite", *ret)
          |    }
          |  } |
          |  loop!([Nil, 7, 7 | 8, 9 | Nil, 9 | 10, Nil, 9])
          |}""".stripMargin
        ).map(ConstructDeploy.sourceDeployNow(_))

      for {
        deploysCheckpoint <- computeDeploysCheckpoint[Task](
                              Seq(genesis.blockHash),
                              deploys,
                              1L
                            )
        Right((preStateHash, computedTsHash, processedDeploys, _, _)) = deploysCheckpoint
        block <- createBlock[Task](
                  ByteString.copyFrom(genesisContext.validatorPks.head.bytes),
                  deploys = processedDeploys,
                  postStateHash = computedTsHash,
                  preStateHash = preStateHash,
                  justifications = Seq(genesis.blockHash)
                )
        validateResult <- validateBlockCheckpoint[Task](block)
        Right(tsHash)  = validateResult
      } yield tsHash should be(Some(computedTsHash))
  }

  it should "pass tests involving races" in withGenesis(genesisContext) {
    implicit blockStore => implicit blockDagStorage => implicit runtimeManager =>
      (0 to 10).toList.traverse_ { i =>
        val deploys =
          Vector(
            """
            | contract @"loop"(@xs) = {
            |   match xs {
            |     [] => {
            |       for (@winner <- @"ch") {
            |         @"return"!(winner)
            |       }
            |     }
            |     [first, ...rest] => {
            |       @"ch"!(first) | @"loop"!(rest)
            |     }
            |   }
            | } | @"loop"!(["a","b","c","d"])
            |""".stripMargin
          ).map(ConstructDeploy.sourceDeployNow(_))

        for {
          deploysCheckpoint <- computeDeploysCheckpoint[Task](
                                Seq(genesis.blockHash),
                                deploys,
                                1L,
                                i + 1L
                              )
          Right((preStateHash, computedTsHash, processedDeploys, _, _)) = deploysCheckpoint
          block <- createBlock[Task](
                    ByteString.copyFrom(genesisContext.validatorPks.head.bytes),
                    deploys = processedDeploys,
                    postStateHash = computedTsHash,
                    preStateHash = preStateHash,
                    seqNum = i + 1L,
                    justifications = Seq(genesis.blockHash)
                  )

          validateResult <- validateBlockCheckpoint[Task](block)
          Right(tsHash)  = validateResult
        } yield tsHash should be(Some(computedTsHash))
      }
  }

  it should "return None for logs containing extra comm events" in withGenesis(genesisContext) {
    implicit blockStore => implicit blockDagStorage => implicit runtimeManager =>
      val deploys =
        (0 until 1).map(i => ConstructDeploy.sourceDeployNow(s"for(_ <- @$i){ Nil } | @$i!($i)"))

      for {
        deploysCheckpoint <- computeDeploysCheckpoint[Task](
                              Seq(genesis.blockHash),
                              deploys,
                              1L
                            )
        Right((preStateHash, computedTsHash, processedDeploys, _, _)) = deploysCheckpoint
        //create single deploy with log that includes excess comm events
        badProcessedDeploy = processedDeploys.head.copy(
          deployLog = processedDeploys.head.deployLog ++ processedDeploys.last.deployLog.take(5)
        )
        block <- createBlock[Task](
                  ByteString.copyFrom(genesisContext.validatorPks.head.bytes),
                  deploys = Seq(badProcessedDeploy, processedDeploys.last),
                  postStateHash = computedTsHash,
                  preStateHash = preStateHash,
                  justifications = Seq(genesis.blockHash)
                )
        validateResult <- validateBlockCheckpoint[Task](block)
        Right(tsHash)  = validateResult
      } yield tsHash should be(None)
  }

  it should "pass map update test" in withGenesis(genesisContext) {
    implicit blockStore => implicit blockDagStorage => implicit runtimeManager =>
      (0 to 10).toList.traverse_ { i =>
        val deploys =
          Vector(
            """
            | @"mapStore"!({}) |
            | contract @"store"(@value) = {
            |   new key in {
            |     for (@map <- @"mapStore") {
            |       @"mapStore"!(map.set(*key.toByteArray(), value))
            |     }
            |   }
            | }
            |""".stripMargin,
            """
            |@"store"!("1")
          """.stripMargin,
            """
            |@"store"!("2")
          """.stripMargin
          ).map(s => ConstructDeploy.sourceDeployNow(s))

        for {
          deploysCheckpoint <- computeDeploysCheckpoint[Task](
                                Seq(genesis.blockHash),
                                deploys,
                                1L,
                                i + 1L
                              )
          Right((preStateHash, computedTsHash, processedDeploys, _, _)) = deploysCheckpoint
          block <- createBlock[Task](
                    ByteString.copyFrom(genesisContext.validatorPks.head.bytes),
                    deploys = processedDeploys,
                    postStateHash = computedTsHash,
                    preStateHash = preStateHash,
                    seqNum = i + 1L,
                    justifications = Seq(genesis.blockHash)
                  )
          validateResult <- validateBlockCheckpoint[Task](block)
          Right(tsHash)  = validateResult
        } yield tsHash.map(_.toHexString) should be(Some(computedTsHash.toHexString))
      }
  }

  // Test for cost mismatch between play and replay in case of out of phlo error
  "used deploy with insufficient phlos" should "be added to a block with all phlos consumed" in effectTest {
    val sampleTerm =
      """
        |  new
        |    rl(`rho:registry:lookup`), RevVaultCh, vaultCh, balanceCh, deployId(`rho:rchain:deployId`)
        |  in {
        |    rl!(`rho:rchain:revVault`, *RevVaultCh) |
        |    for (@(_, RevVault) <- RevVaultCh) {
        |      match "1111MnCcfyG9sExhw1jQcW6hSb98c2XUtu3E4KGSxENo1nTn4e5cx" {
        |        revAddress => {
        |          @RevVault!("findOrCreate", revAddress, *vaultCh) |
        |          for (@(true, vault) <- vaultCh) {
        |            @vault!("balance", *balanceCh) |
        |            for (@balance <- balanceCh) {
        |              deployId!(balance)
        |            }
        |          }
        |        }
        |      }
        |    }
        |  }
        |""".stripMargin
    val deploy =
      ConstructDeploy.sourceDeploy(
        sampleTerm,
        System.currentTimeMillis,
        phloLimit = 3000,
        shardId = genesis.shardId
      )

    TestNode.standaloneEff(genesisContext).use { node =>
      for {
        b <- node.addBlock(deploy)
        _ = b.state.deploys.size shouldBe 1
        _ = b.state.deploys.get(0).get.cost.cost shouldBe 3000
      } yield ()
    }
  }

  val multiBranchSampleTermWithError =
    """
      |  new rl(`rho:registry:lookup`), RevVaultCh, ackCh, out(`rho:io:stdout`)
      |  in {
      |    new signal in {
      |      signal!(0) | signal!(0) | signal!(0) | signal!(0) | signal!(0) | signal!(0) | signal!(1) |
      |      contract signal(@x) = {
      |        rl!(`rho:rchain:revVault`, *RevVaultCh) | ackCh!(x) |
      |        if (x == 1) {}.xxx() // Simulates error in one branch
      |      }
      |    } |
      |    for (@(_, RevVault) <= RevVaultCh & @x<= ackCh) {
      |      @(*ackCh, "parallel universe")!("Rick and Morty")
      |    }
      |  }
      |""".stripMargin

  "replay" should "match in case of Out of Phlo error" in effectTest {
    val deploy =
      ConstructDeploy.sourceDeploy(
        multiBranchSampleTermWithError,
        System.currentTimeMillis,
        // Not enough phlo
        phloLimit = 20000,
        shardId = genesis.shardId
      )

    TestNode.standaloneEff(genesisContext).use { node =>
      for {
        b <- node.addBlock(deploy)
        _ = b.state.deploys.size shouldBe 1
        _ = b.state.deploys.get(0).get.cost.cost shouldBe 20000
      } yield ()
    }
  }

  "replay" should "match in case of user execution error" in effectTest {
    val deploy =
      ConstructDeploy.sourceDeploy(
        multiBranchSampleTermWithError,
        System.currentTimeMillis,
        // Enough phlo
        phloLimit = 300000,
        shardId = genesis.shardId
      )

    TestNode.standaloneEff(genesisContext).use { node =>
      for {
        b <- node.addBlock(deploy)
        _ = b.state.deploys.size shouldBe 1
        _ = b.state.deploys.get(0).get.cost.cost shouldBe 300000
      } yield ()
    }
  }

}
