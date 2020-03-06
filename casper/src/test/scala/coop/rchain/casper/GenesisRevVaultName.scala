package coop.rchain.casper.api

import cats.effect.Sync
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.helper.{BlockDagStorageFixture, BlockGenerator, TestNode}
import coop.rchain.casper.util.GenesisBuilder.{buildGenesis, buildGenesisParameters}
import coop.rchain.casper.util.ConstructDeploy.{defaultSec, defaultSec2}
import coop.rchain.shared.scalatestcontrib.effectTest
import coop.rchain.casper.engine.Engine
import coop.rchain.crypto.signatures.Secp256k1.toPublic
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.casper.{EngineWithCasper, SafetyOracle}
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.util.ConstructDeploy.{basicDeployData, sourceDeployNowF}
import coop.rchain.metrics.Metrics
import coop.rchain.shared.{Cell, Log}
import coop.rchain.models._
import coop.rchain.models.{BlockMetadata, Par}
import coop.rchain.models.rholang.sorter.Sortable._
import coop.rchain.models.serialization.implicits.mkProtobufInstance
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.EventConverter
import coop.rchain.crypto.PrivateKey
import coop.rchain.rspace.StableHashProvider
import coop.rchain.rspace.trace.{COMM, Consume, Produce}
import monix.eval.Task
import org.scalatest.{EitherValues, FlatSpec, Matchers}
import monix.execution.Scheduler.Implicits.global

import scala.collection.immutable

class GenesisRevVaultName
    extends FlatSpec
    with Matchers
    with EitherValues
    with BlockGenerator
    with BlockDagStorageFixture {
  implicit val metricsEff = new Metrics.MetricsNOP[Task]
  val genesisParameters   = buildGenesisParameters(bondsFunction = _.zip(List(10L, 10L, 10L)).toMap)
  val genesisContext      = buildGenesis(genesisParameters)
  val revVaultDeploy =
    genesisContext.genesisBlock.body.deploys.filter(p => p.deploy.data.term.contains("_revVault"))

  def exploratoryDeploy(term: String)(engineCell: Cell[Task, Engine[Task]])(
      implicit blockStore: BlockStore[Task],
      safetyOracle: SafetyOracle[Task],
      log: Log[Task]
  ) =
    BlockAPI
      .exploratoryDeploy(term)(
        Sync[Task],
        engineCell,
        log,
        safetyOracle,
        blockStore
      )

//  private def transferDeploy(from: PrivateKey, to: PrivateKey) =
//    sourceDeployNowF[Task](
//      s"""new
//       |  rl(`rho:registry:lookup`), RevVaultCh,
//       |  stdout(`rho:io:stdout`)
//       |in {
//       |
//       |  rl!(`rho:rchain:revVault`, *RevVaultCh) |
//       |  for (@(_, RevVault) <- RevVaultCh) {
//       |
//       |    stdout!(("3.transfer_funds.rho")) |
//       |
//       |    // REPLACE THE REV ADDRESSES HERE vvv
//       |    match (
//       |      "${RevAddress.fromPublicKey(toPublic(from)).get.toBase58}",
//       |      "${RevAddress.fromPublicKey(toPublic(to)).get.toBase58}",
//       |      1000
//       |    ) {
//       |      (from, to, amount) => {
//       |
//       |        new vaultCh, revVaultkeyCh, deployerId(`rho:rchain:deployerId`) in {
//       |          @RevVault!("findOrCreate", from, *vaultCh) |
//       |          @RevVault!("deployerAuthKey", *deployerId, *revVaultkeyCh) |
//       |          for (@(true, vault) <- vaultCh; key <- revVaultkeyCh) {
//       |
//       |            stdout!(("Beginning transfer of ", amount, "REV from", from, "to", to)) |
//       |
//       |            new resultCh in {
//       |              @vault!("transfer", to, amount, *key, *resultCh) |
//       |              for (@result <- resultCh) {
//       |
//       |                stdout!(("Finished transfer of ", amount, "REV to", to, "result was:", result))
//       |              }
//       |            }
//       |          }
//       |        }
//       |      }
//       |    }
//       |  }
//       |
//       |}
//       |""".stripMargin,
//      sec = from
//    )

  private def findRelatedLogs(
      block: BlockMessage,
      sortedListeningName: Seq[Par]
  ) = {
    val serializedLog = for {
      pd    <- block.body.deploys
      event <- pd.deployLog
    } yield event
    val log =
      serializedLog.map(EventConverter.toRspaceEvent)
    log.filter {
      case Produce(channelHash, _, _) =>
        channelHash == StableHashProvider.hash(sortedListeningName)
      case Consume(channelsHashes, _, _) =>
        channelsHashes.toList.sorted == sortedListeningName
          .map(StableHashProvider.hash(_))
          .toList
          .sorted
      case COMM(consume, produces, _, _) =>
        (consume.channelsHashes.toList.sorted ==
          sortedListeningName.map(StableHashProvider.hash(_)).toList.sorted) ||
          produces.exists(
            produce => produce.channelsHash == StableHashProvider.hash(sortedListeningName)
          )
    }
  }
  it should "exploratoryDeploy get data from the read only node" in effectTest {
    TestNode.networkEff(genesisContext, networkSize = 1, withReadOnlySize = 1).use {
      case nodes @ n1 +: readOnly +: Seq() =>
        import readOnly.{blockStore, cliqueOracleEffect, logEff, runtimeManager}
        val engine = new EngineWithCasper[Task](readOnly.casperEff)
        for {
          produceDeploys <- (0 until 2).toList.traverse(i => basicDeployData[Task](i))
          b1             <- n1.propagateBlock(produceDeploys(0))(nodes: _*)
//          _              <- n2.propagateBlock(produceDeploys(1))(nodes: _*)
//          transferDeploy <- transferDeploy(defaultSec, defaultSec2)
//          b1              <- n1.propagateBlock(transferDeploy)(nodes: _*)
          engineCell <- Cell.mvarCell[Task, Engine[Task]](engine)
          result <- exploratoryDeploy("""new
                                        |  return, rl(`rho:registry:lookup`), RevVaultCh
                                        |in{
                                        |  rl!(`rho:rchain:revVault`, *RevVaultCh)|
                                        |  for (@(_, RevVault) <- RevVaultCh){
                                        |    return!(RevVault)
                                        |  }
                                        |}""".stripMargin)(
                     engineCell
                   ).map(_.right.value)
          (par, lastFinalizedBlock) = result
//          _                         = lastFinalizedBlock.blockHash shouldBe PrettyPrinter.buildStringNoLimit(b2.blockHash)
          _ = println(par)
          revVaultPrivate = par match {
            case Seq(Par(_, _, _, _, _, _, Seq(bundle), _, _, _)) =>
              bundle.body
//              match {
//                case Par(_, _, _, _, _, Seq(unforgeable), _, _, _, _) =>
//                  unforgeable.unfInstance.value
//              }
          }
          relatedLog = findRelatedLogs(b1, Seq(revVaultPrivate))
          dataHash = relatedLog.head match {
            case Produce(_, hash, _) => hash
            case _                   => none
          }
          data  <- runtimeManager.getData(b1.body.state.postStateHash)(revVaultPrivate)
          data2 <- runtimeManager.getContinuation(b1.body.state.postStateHash)(Seq(revVaultPrivate))
          _     = println("asd")
        } yield ()
    }
  }
}
