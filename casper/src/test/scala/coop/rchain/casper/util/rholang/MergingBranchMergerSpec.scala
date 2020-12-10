package coop.rchain.casper.util.rholang

import cats.effect.{Concurrent, Resource, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.blocks.BranchMerger
import coop.rchain.casper.blocks.BranchMerger.MergingVertex
import coop.rchain.casper.protocol.{ProcessedDeploy, ProcessedSystemDeploy}
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang.costacc.CloseBlockDeploy
import coop.rchain.casper.util.{ConstructDeploy, GenesisBuilder}
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.dag.InMemDAG
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.p2p.EffectsTestInstances.{LogStub, LogicalTime}
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.shared.scalatestcontrib.effectTest
import coop.rchain.shared.{Log, Time}
import fs2.Stream
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.Seq
import scala.concurrent.duration.FiniteDuration
import scala.io.Source

class MergingBranchMergerSpec extends FlatSpec with Matchers {

  val genesisContext             = GenesisBuilder.buildGenesis(validatorsNum = 6)
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
      // _ <- runtimeManager.computeBonds(r._1)
      // _ <- runtimeManager.replayComputeState(baseState)(r._2, r._3, blockData, invalidBlocks, false)

      _ = println(
        s"Created: preState ${PrettyPrinter.buildString(baseState)}, postState ${PrettyPrinter
          .buildString(r._1)}; ${r._2.size} deploys; ${r._2.flatMap(_.deployLog).size} events; payed by ${Base16
          .encode(payerKey.bytes)}, to ${Base16.encode(validator.bytes)}"
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

  def mkSingleState(stateHash: StateHash, seqNum: Int, blockNum: Long)(
      implicit runtimeManager: RuntimeManager[Task]
  ) =
    mkStatesSteams(stateHash, seqNum, blockNum).head.compile.lastOrError

  def mkAllStates(stateHash: StateHash, seqNum: Int, blockNum: Long)(
      implicit runtimeManager: RuntimeManager[Task]
  ) =
    Stream
      .emits(mkStatesSteams(stateHash, seqNum, blockNum))
      .parJoinUnbounded
      .compile
      .toList

  "blocks" should "be merged" in effectTest {
    runtimeManagerResource.use { runtimeManager =>
      {
        implicit val rm: RuntimeManager[Task] = runtimeManager
        val emptyDag                          = InMemDAG[Task, MergingVertex](Map.empty, Map.empty)
        val genesisPostStateHash              = genesis.body.state.postStateHash

        for {
          // create base state - child of genesis
          b    <- mkSingleState(genesisPostStateHash, 1, 1)
          base = MergingVertex(stateHash = b._1, processedDeploys = b._2)
          // create base children
          baseChildren <- mkAllStates(base.stateHash, 2, 2)
//          baseChildren1 <- mkAllStates(baseChildren.head._1, 3, 3)
          mergingTips = baseChildren.map(
            c => MergingVertex(stateHash = c._1, processedDeploys = c._2)
          )
          dag <- mergingTips.foldLeftM(emptyDag)((acc, tip) => acc.addEdge(tip, base))
          // merge
          _ <- BranchMerger.merge(mergingTips, base, dag)
        } yield ()
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
